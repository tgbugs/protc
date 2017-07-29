#!/usr/bin/env python3.6
import os
import pickle
import asyncio
from os import environ
from datetime import date
from threading import Thread
from markdown import markdown
from scibot.hypothesis import HypothesisUtils, HypothesisAnnotation
import analysis
from analysis import hypothesis_local, get_hypothesis_local, url_doi, url_pmid
from analysis import papers, statistics, tagdefs, readTagDocs, addDocLinks, addReplies, _addParent
from hypush.subscribe import preFilter, setup_websocket
from hypush.handlers import filterHandler
from IPython import embed

from flask import Flask, url_for, redirect, request, render_template, render_template_string, make_response, abort 

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

# hypothesis API

def get_annos_from_api(offset=0, limit=None):
    print('yes we have to start from here')
    h = HypothesisUtils(username=username, token=api_token, group=group, max_results=100000)
    params = {'offset':offset,
              'group':h.group}
    if limit is None:
        rows = h.search_all(params)
    else:
        params['limit'] = limit
        obj = h.search(params)
        rows = obj['rows']
        if 'replies' in obj:
            rows += obj['replies']
    annos = [HypothesisAnnotation(row) for row in rows]
    if limit is None:
        addReplies(annos)
    return annos

def get_annos_from_file(memoization_file):
    try:
        with open(memoization_file, 'rb') as f:
            annos = pickle.load(f)
        if annos is None:
            return []
        else:
            return annos
    except FileNotFoundError:
        return []

def add_missing_annos(annos):
    offset = 0
    limit = 200
    done = False
    while not done:
        new_annos = get_annos_from_api(offset, limit)
        offset += limit
        if not new_annos:
            break
        for anno in new_annos:
            if anno not in annos:
                annos.append(anno)
            else:
                done = True
                break  # assume that annotations return newest first
    addReplies(annos)

def get_annos(memoization_file='/tmp/annotations.pickle'):
    annos = get_annos_from_file(memoization_file)
    if not annos:
        new_annos = get_annos_from_api()
        annos.extend(new_annos)
    add_missing_annos(annos)
    memoize_annos(annos, memoization_file)
    return annos

def memoize_annos(annos, memoization_file):  # FIXME if there are multiple ws listeners we will have race conditions?
    print(f'annos updated, memoizing new version with, {len(annos)} members')
    with open(memoization_file, 'wb') as f:
        pickle.dump(annos, f)

def export_json_impl(annos):
    output_json = [anno.__dict__ for anno in annos]
    DATE = date.today().strftime('%Y-%m-%d')
    return output_json, DATE

# hypothesis websocket

class protcurHandler(filterHandler):
    def __init__(self, annos, memoization_file):
        self.annos = annos
        self.memoization_file = memoization_file
    def handler(self, message):
        try:
            act = message['options']['action'] 
            if act != 'create': # update delete
                mid = message['payload'][0]['id']
                gone = [_ for _ in self.annos if _.id == mid][0]
                self.annos.remove(gone)
            if act != 'delete':  # create update
                anno = HypothesisAnnotation(message['payload'][0])
                _addParent(anno, self.annos)  # this is safe because time exists!
                self.annos.append(anno)
            #print(len(self.annos), 'annotations.')
            memoize_annos(self.annos, self.memoization_file)
        except KeyError as e:
            embed()

def streaming(annos, memoization_file):
    filters = preFilter(groups=[group]).export()
    filter_handlers = [protcurHandler(annos, memoization_file)]
    ws_loop = setup_websocket(api_token, filters, filter_handlers)
    return ws_loop 

def loop_target(loop, ws_loop):
    asyncio.set_event_loop(loop)
    loop.run_until_complete(ws_loop())

def start_loop(annos, memoization_file):
    loop = asyncio.get_event_loop()
    ws_loop = streaming(annos, memoization_file)
    stream_loop = Thread(target=loop_target, args=(loop, ws_loop))
    return stream_loop 

# rendering

table_style = ('<style>'
               'th { text-align: left; padding-right: 20px; }'
               'table { font-family: Dejavu Sans Mono; }'
               'a:link { color: black; }'
               'a:visited { color: grey; }'
               '</style>')


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
    output.append(table_style)
    output.append(f'<tr><th>{HLN}</th><th>{DOI}</th><th>{PMID}</th><th>{PDOI}</th></tr>')
    for hl_name, others in sorted(idents.items()):
        doi = others[DOI] if DOI in others else ''
        pmid = others[PMID] if PMID in others else ''
        pdoi = others[PDOI] if PDOI in others else ''
        #output.append(f'{hl_name:<{cols[HLN]}}{doi:<{cols[DOI]}}{pmid:<{cols[PMID]}}{pdoi:<{cols[PDOI]}}')
        output.append(f'<tr><th><a href={hypothesis_local(hl_name)}>{hl_name}</a></th>'
                      f'<th><a href={url_doi(doi)}>{doi}</th>'
                      f'<th><a href={url_pmid(pmid)}>{pmid}</th>'
                      f'<th><a href={url_doi(pdoi)}>{pdoi}</th></tr>')
    #out = '<pre>' + '\n'.join(output) + '</pre>'
    out = '<table>' + '\n'.join(output) + '</table>'
    #print(out)
    return out

def render_2col_table(dict_, h1, h2, uriconv=lambda a:a):  # FIXME this sucks and uriconv only works on the first row...
    output = []
    output.append(table_style)
    output.append(f'<tr><th>{h1}</th><th>{h2}</th></tr>')
    for hl_name, thing in sorted(dict_.items()):
        output.append(f'<tr><th><a href={uriconv(hl_name)}>{hl_name}</a></th>'
                      f'<th>{thing}</th></tr>')
    out = '<table>' + '\n'.join(output) + '</table>'
    return out

def main():
    app = Flask('protc curation id service')
    annos = get_annos()
    stream_loop = start_loop(annos, '/tmp/protcur-annos.pickle')
    stream_loop.start()

    # routes

    @app.route('/curation/papers', methods=['GET'])
    def route_papers():
        return render_idents(papers(annos))

    @app.route('/curation/annotations', methods=['GET'])
    def route_annotations():
        stats = statistics(annos)
        return render_2col_table(stats, 'HLN', 'Annotation count', hypothesis_local)

    @app.route('/curation/tags', methods=['GET'])
    def route_tags():
        querybase = ''

        tags = {t:f'<a href=https://hypothes.is/search?q=tag:{t}>{d}</a>'
                for t, d in tagdefs(annos).items()
                if all(p not in t for p in ('RRID:', 'NIFORG:', 'CHEBI:')) }
    
        def uriconv(v):
            uri = request.base_url + '/' + v
            return uri
        return render_2col_table(tags, 'Tag', 'Count', uriconv=uriconv)

    @app.route('/curation/tags/<tagname>', methods=['GET'])
    def route_tags_star(tagname):
        try:
            return markdown(addDocLinks(request.base_url.rsplit('/',1)[0],
                                        readTagDocs()[tagname]))  # sure it is slow but it allows live updates
        except KeyError:
            return abort(404)

    @app.route('/curation', methods=['GET'])
    @app.route('/curation/', methods=['GET'])
    def route_curation():
        out = ''
        for route in 'papers', 'annotations', 'tags':
            url = 'http://protc.olympiangods.org/curation/' + route
            out += f'<a href={url}>{route}</a> <br>'
        return out

    app.debug = False
    app.run(host='localhost', port=7000, threaded=True)  # nginxwoo
    os.sys.exit()

if __name__ == '__main__':
    main()

