#!/usr/bin/env python3.6
import os
from datetime import date
from markdown import markdown
from hyputils.hypothesis import HypothesisUtils
from analysis import hypothesis_local, get_hypothesis_local, url_doi, url_pmid
from analysis import citation_tree, papers, statistics, tagdefs, readTagDocs, justTags, addDocLinks, protc
from IPython import embed
from flask import Flask, url_for, redirect, request, render_template, render_template_string, make_response, abort 

hutils = HypothesisUtils(username='')

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

def export_json_impl(annos):
    output_json = [anno.__dict__ for anno in annos]
    DATE = date.today().strftime('%Y-%m-%d')
    return output_json, DATE


# rendering

table_style = ('<style>'
               'th { text-align: left; padding-right: 20px; }'
               'table { font-family: Dejavu Sans Mono; }'
               'a:link { color: black; }'
               'a:visited { color: grey; }'
               '</style>')

def atag(href, value=None):
    if value is None:
        value = href
    return f'<a href={href}>{value}</a>'

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
    from core import annoSync
    app = Flask('protc curation id service')
    get_annos, annos, stream_loop = annoSync('/tmp/protcure-server-annos.pickle',
                                             helpers=(protc,))
    stream_loop.start()
    [protc(a, annos) for a in annos]

    # routes

    @app.route('/curation/citations', methods=['GET'])
    def route_citations():
        tree, extra = citation_tree(annos)
        return extra.html

    @app.route('/curation/ast', methods=['GET'])
    def route_ast():
        return '<pre>' + protc.parentless() + '</pre>'

    @app.route('/curation/papers', methods=['GET'])
    def route_papers():
        return render_idents(papers(annos))

    @app.route('/curation/annotations', methods=['GET'])
    def route_annotations():
        stats = statistics(annos)
        total = sum(stats.values())
        stats = {hl:atag(hutils.search_url(url=hypothesis_local(hl)), n) for hl, n in stats.items()}
        return render_2col_table(stats, f'HLN n={len(stats)}', f'Annotation count n={total}', hypothesis_local)

    @app.route('/curation/tags', methods=['GET'])
    def route_tags():
        querybase = ''

        tags = {t:atag(hutils.search_url(tag=t), d)
                for t, d in tagdefs(annos).items()
                if all(p not in t for p in ('RRID:', 'NIFORG:', 'CHEBI:')) }
    
        def uriconv(v):
            uri = request.base_url + '/' + v
            return uri
        return render_2col_table(tags, f'Tags n={len(tags)}', f'Count n={sum(int(_.split(">",1)[1].split("<")[0]) for _ in tags.values())}', uriconv=uriconv)

    @app.route('/curation/tags/<tagname>', methods=['GET'])
    def route_tags_star(tagname):
        try:
            return markdown(addDocLinks(request.base_url.rsplit('/',1)[0],
                                        readTagDocs()[tagname].doc))  # sure it is slow but it allows live updates
        except KeyError:
            return abort(404)

    @app.route('/curation/controlled-tags', methods=['GET'])
    def route_controlled_tags():
        return '\n'.join(tag for tag in justTags()), 200, {'Content-Type':'text/plain; charset=utf-8'}

    @app.route('/curation', methods=['GET'])
    @app.route('/curation/', methods=['GET'])
    def route_curation():
        out = ''
        for route in 'papers', 'annotations', 'tags', 'ast', 'citations':
            url = request.base_url + route
            out += f'<a href={url}>{route}</a> <br>'
        return out

    app.debug = False
    app.run(host='localhost', port=7000, threaded=True)  # nginxwoo
    os.sys.exit()

if __name__ == '__main__':
    main()
