#!/usr/bin/env python3.6
from __future__ import print_function
import re
import csv
import os
import asyncio
from os import environ
from datetime import date
from threading import Thread
from hypothesis import HypothesisUtils, HypothesisAnnotation
from hypush.subscribe import preFilter, setup_websocket
from hypush.handlers import filterHandler
#from pyontutils.hierarchies import ???
from IPython import embed

from flask import Flask, url_for, redirect, request, render_template, render_template_string, make_response, abort 


app = Flask('protc curation id service')

api_token = environ.get('HYP_API_TOKEN', 'TOKEN')  # Hypothesis API token
username = environ.get('HYP_USERNAME', 'USERNAME') # Hypothesis username
group = environ.get('HYP_GROUP', '__world__')

print(api_token, username, group)  # sanity check

# utility
def get_proper_citation(xml):
    root = etree.fromstring(xml)
    if root.findall('error'):
        proper_citation = ''
    else:
        data_elements = root.findall('data')[0]
        data_elements = [(e.find('name').text, e.find('value').text) for e in data_elements]  # these shouldn't duplicate
        a = [v for n, v in data_elements if n == 'Proper Citation']
        proper_citation = a[0] if a else ''

    return proper_citation

def fix_trailing_slash(annotated_urls):
    for key in [k for k in annotated_urls.keys()]:
        if key.endswith('/'):
            new_key = key.rstrip('/')
            print(new_key)
            if new_key in annotated_urls:
                annotated_urls[key].extend(annotated_urls.pop(new_key))

def get_hypothesis_local(uri):
    if 'hypothesis-local' in uri:
        return os.path.splitext(os.path.basename(uri))[0]

def hypothesis_local(hln):
    return 'http://hypothesis-local.olympiangods.org/' + hln + '.pdf'

def url_doi(doi):
    return 'https://doi.org/' + doi

def url_pmid(pmid):
    return 'https://www.ncbi.nlm.nih.gov/pubmed/' + pmid

# hypothesis API

def get_annos():
    h = HypothesisUtils(username=username, token=api_token, group=group, max_results=100000)
    params = {'group' : h.group }
    rows = h.search_all(params)
    annos = [HypothesisAnnotation(row) for row in rows]
    return annos

def export_json_impl(annos):
    output_json = [anno.__dict__ for anno in annos]
    DATE = date.today().strftime('%Y-%m-%d')
    return output_json, DATE

# hypothesis websocket

class protcurHandler(filterHandler):
    def __init__(self, annos):
        self.annos = annos
    def handler(self, message):
        try:
            if message['options']['action'] == 'delete':
                mid = message['payload'][0]['id']
                gone = [_ for _ in self.annos if _.id == mid][0]
                self.annos.remove(gone)
            else:
                anno = HypothesisAnnotation(message['payload'][0])
                self.annos.append(anno)
            print(len(self.annos), 'annotations.')
        except KeyError as e:
            embed()

def streaming(annos):
    filters = preFilter(groups=[group]).export()
    filter_handlers = [protcurHandler(annos)]
    ws_loop = setup_websocket(api_token, filters, filter_handlers)
    return ws_loop

def loop_target(loop, ws_loop):
    asyncio.set_event_loop(loop)
    loop.run_until_complete(ws_loop())

def start_loop():
    annos = get_annos()
    loop = asyncio.get_event_loop()
    ws_loop = streaming(annos)
    stream_loop = Thread(target=loop_target, args=(loop, ws_loop))
    return annos, stream_loop


# rendering

def identifiers(annos):
    idents = {}
    def add_tag_text(hl, anno, tag):
            if tag in anno.tags:
                idents[hl][tag] = anno.text.strip()

    for anno in annos:
        hl = get_hypothesis_local(anno.uri)
        if hl:
            if hl not in idents:
                idents[hl] = {}
            #print(hl)
            #print(anno.exact)
            #print(anno.tags)
            #print(anno.text)
            #print(anno.user)
            #print('---------------------')
            add_tag_text(hl, anno, 'DOI:')
            add_tag_text(hl, anno, 'protc:parent-doi')
            add_tag_text(hl, anno, 'PMID:')

    return idents

def render_idents(idents):
    #print(idents)
    HLN, DOI, PMID, PDOI = 'HLN', 'DOI:', 'PMID:', 'protc:parent-doi'
    fields = DOI, PMID, PDOI
    cols = {f:len(f) + 2 for f in fields}
    cols[HLN] = 0
    for hl_name, others in idents.items():
        lhln = len(hl_name) + 2
        if lhln > cols[HLN]: cols[HLN] = lhln
        for f in fields:
            if f in others:
                v = others[f]
                lv = len(v) + 2
                if lv > cols[f]: cols[f] = lv
    output = []
    #output.append(f'{HLN:<{cols[HLN]}}{DOI:<{cols[DOI]}}{PMID:<{cols[PMID]}}{PDOI:<{cols[PDOI]}}')
    output.append('<style>'
                  'th { text-align: left; padding-right: 20px; }'
                  'table { font-family: Dejavu Sans Mono; }'
                  'a:link { color: black; }'
                  'a:visited { color: grey; }'
                  '</style>'
                 )
    output.append(f'<tr><th>{HLN}</th><th>{DOI}</th><th>{PMID}</th><th>{PDOI}</tr>')
    for hl_name, others in sorted(idents.items()):
        doi = others[DOI] if DOI in others else ''
        pmid = others[PMID] if PMID in others else ''
        pdoi = others[PDOI] if PDOI in others else ''
        #output.append(f'{hl_name:<{cols[HLN]}}{doi:<{cols[DOI]}}{pmid:<{cols[PMID]}}{pdoi:<{cols[PDOI]}}')
        output.append(f'<tr><th><a href={hypothesis_local(hl_name)}>{hl_name}</a></th>'
                      f'<th><a href={url_doi(doi)}>{doi}</th>'
                      f'<th><a href={url_pmid(pmid)}>{pmid}</th>'
                      f'<th><a href={url_doi(pdoi)}>{pdoi}</tr>')
    #out = '<pre>' + '\n'.join(output) + '</pre>'
    out = '<table>' + '\n'.join(output) + '</table>'
    #print(out)
    return out

def citation_tree(annos):
    p = 'protc:references-for-use' 
    trips = []
    for anno in annos:
        hl = get_hypothesis_local(anno.uri)
        if hl:
            s = hl
            if p in anno.tags:
                t = anno.text.strip()
                o = get_hypothesis_local(t)
                o = o if o else t
                trips.append((p, s, o))

    return trips

# setup 

annos, stream_loop = start_loop()
stream_loop.start()

# routes

@app.route('/curation/identifiers', methods=['GET'])
def woooo():
    return render_idents(identifiers(annos))

def main():
    app.debug = False
    app.run(host='localhost', port=7000, threaded=True)  # nginxwoo
    stream_loop.stop()

def test():
    annos = get_annos()
    i = identifiers(annos)
    print(i)
    print(render_idents(i))
    return
    t = citation_tree(annos)
    tu = [(p, hypothesis_local(s), hypothesis_local(o)) for p, s, o in t]
    embed()

if __name__ == '__main__':
    main()
    #test()

