#!/usr/bin/env python3.6
from __future__ import print_function
import re
import csv
import os
from os import environ
from datetime import date
from hypothesis import HypothesisUtils, HypothesisAnnotation
from IPython import embed

from flask import Flask, url_for, redirect, request, render_template, render_template_string, make_response, abort 


app = Flask('protc curation id service')

api_token = environ.get('HYP_API_TOKEN', 'TOKEN')  # Hypothesis API token
username = environ.get('HYP_USERNAME', 'USERNAME') # Hypothesis username
group = environ.get('HYP_GROUP', '__world__')

print(api_token, username, group)  # sanity check

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

def export_json_impl():
    h = HypothesisUtils(username=username, token=api_token, group=group, max_results=100000)
    params = {'group' : h.group }
    rows = h.search_all(params)
    annos = [HypothesisAnnotation(row) for row in rows]
    output_json = [anno.__dict__ for anno in annos]
    DATE = date.today().strftime('%Y-%m-%d')
    return output_json, DATE

def get_hypothesis_local(uri):
    if 'hypothesis-local' in uri:
        return os.path.splitext(os.path.basename(uri))[0]

def identifiers():
    h = HypothesisUtils(username=username, token=api_token, group=group, max_results=100000)
    params = {'group' : h.group }
    rows = h.search_all(params)
    annos = [HypothesisAnnotation(row) for row in rows]

    # clean up bugs from old curation workflow
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
    output.append(f'{HLN:<{cols[HLN]}}{DOI:<{cols[DOI]}}{PMID:<{cols[PMID]}}{PDOI:<{cols[PDOI]}}')
    for hl_name, others in sorted(idents.items()):
        doi = others[DOI] if DOI in others else ''
        pmid = others[PMID] if PMID in others else ''
        pdoi = others[PDOI] if PDOI in others else ''
        output.append(f'{hl_name:<{cols[HLN]}}{doi:<{cols[DOI]}}{pmid:<{cols[PMID]}}{pdoi:<{cols[PDOI]}}')
    out = '<pre>' + '\n'.join(output) + '</pre>'
    #print(out)
    return out

@app.route('/identifiers', methods=['GET'])
def woooo():
    return render_idents(identifiers())

def test():
    i = identifiers()
    print(i)
    print(render_idents(i))
    embed()

def main():
    app.debug = False
    app.run(host='localhost', port=7000, threaded=True)  # nginxwoo

if __name__ == '__main__':
    main()

