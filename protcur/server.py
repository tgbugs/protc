#!/usr/bin/env python3.6
import os
import re
import subprocess
from pathlib import Path
from datetime import date
from markdown import markdown
from hyputils.hypothesis import HypothesisUtils
from protcur.core import htmldoc, atag, deltag, titletag
from protcur.analysis import hypothesis_local, get_hypothesis_local, url_doi, url_pmid
from protcur.analysis import citation_tree, papers, statistics, readTagDocs, justTags, addDocLinks, Hybrid, protc
from IPython import embed
from flask import Flask, url_for, redirect, request, render_template, render_template_string, make_response, abort 

PID = os.getpid()
UID = os.getuid()
THIS_FILE = Path(__file__).absolute()

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

# styles

monospace_body_style = 'body { font-family: Dejavu Sans Mono; font-size: 11pt }'

table_style = ('th { text-align: left; padding-right: 20px; }'
               'table { font-family: Dejavu Sans Mono; }'
               'a:link { color: black; }'
               'a:visited { color: grey; }'
               'del { color: white; }')

details_style = ('details summary::-webkit-details-marker { display: none; }\n'
                'details > summary:first-of-type { list-style-type: none; }')

# rendering

def render_idents(idents, stats):
    #print(idents)
    HLN, DOI, PMID, ISBN, PDOI = 'hl:', 'DOI:', 'PMID:', 'ISBN:', 'protc:parent-doi'
    records = []
    #output.append(f'{HLN:<{cols[HLN]}}{DOI:<{cols[DOI]}}{PMID:<{cols[PMID]}}{PDOI:<{cols[PDOI]}}')
    #output.append(f'<tr><th>{HLN}</th><th>{DOI}</th><th>{PMID}</th><th>{PDOI}</th></tr>')
    for hl, others in sorted(idents.items()):
        hl_uri = hypothesis_local(hl)
        doi = atag(others[DOI], uriconv=url_doi) if DOI in others else ''
        pmid = atag(others[PMID], uriconv=url_pmid) if PMID in others else ''
        isbn = others[ISBN] if ISBN in others else ''
        pdoi = atag(others[PDOI], uriconv=url_doi) if PDOI in others else ''
        count = atag(hutils.search_url(url=hl_uri), stats[hl])
        records.append([atag(hl_uri, hl), doi, pmid, isbn, pdoi, count])
        continue
        #output.append(f'{hl_name:<{cols[HLN]}}{doi:<{cols[DOI]}}{pmid:<{cols[PMID]}}{pdoi:<{cols[PDOI]}}')
        output.append(f'<tr><th><a href={hypothesis_local(hl_name)}>{hl_name}</a></th>'
                      f'<th><a href={url_doi(doi)}>{doi}</th>'
                      f'<th><a href={url_pmid(pmid)}>{pmid}</th>'
                      f'<th><a href={url_doi(pdoi)}>{pdoi}</th></tr>')

    return render_table(records, HLN, DOI, PMID, ISBN, PDOI, '# annos')
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

def render_table(rows, *headers):
    output = []
    output.append('<tr><th>' + '</th><th>'.join(headers) + '</th><tr>')
    for row in rows:
        output.append('<tr><th>' + '</th><th>'.join(row) + '</th><tr>')

    out = '<table>' + '\n'.join(output) + '</table>'
    return out

#asdf = re.compile(r'>\ +<')
#asdf = re.compile(r'>\n\ +<')
match_span = re.compile(r'(>\ +<)|(>\n\ +<)')
def space_to_nbsp(match):
    # return match.group().replace(' ', '&nbsp;')  # keep around for debug viewing
    return match.group().replace(' ', '\u00A0')

#match_http = re.compile(r'(http.+)(</span>){1}')  # doesn't work right
match_comment = re.compile(r'(<span class="comment">; )(.+)(</span>)')
def comment_to_atag(match):
    return match.group(1) + atag(match.group(2), match.group(2), new_tab=True) + match.group(3)

match_quote = re.compile(r"(\n)(\ +)('|<)")
def quote_fix(match):
    return '<br>' + match.group(1) + match.group(2).replace(' ', '\u00A0') + match.group(3)
    
def correct_colorized(html):
    html1 = html.replace('\n</span>', '</span><br>\n')
    html2 = html1.replace('</span>\n', '</span><br>\n')
    html3 = match_span.sub(space_to_nbsp, html2)
    html4 = match_comment.sub(comment_to_atag, html3)
    html5 = match_quote.sub(quote_fix, html4)
    return html5

colorizer_command = THIS_FILE.parent / 'colorizer.lisp'
ast_file = Path(f'/tmp/{UID}-protc-ast-render.rkt')
ast_html_file = Path(f'/tmp/{UID}-protc-ast-render.html')
if ast_html_file.exists(): os.remove(ast_html_file.as_posix())  # cleanup at startup
html_holder = ['']
def render_ast():
    new_raw = protc.parentless()
    old_raw = None
    if ast_file.exists():
        with open(ast_file.as_posix(), 'rt') as f:
            old_raw = f.read()

    if old_raw != new_raw:
        with open(ast_file.as_posix(), 'wt') as f:
            f.write(new_raw)

    if not ast_html_file.exists() or old_raw != new_raw:
        subprocess.check_output([colorizer_command, ast_file.as_posix()])

        with open(ast_html_file.as_posix(), 'rt') as f:
            html_uncorrected = f.read()

        html_holder[0] = correct_colorized(html_uncorrected)

    return html_holder[0]

def make_app(annos):
    app = Flask('protc curation id service')

    # routes

    @app.route('/curation/citations', methods=['GET'])
    def route_citations():
        #tree, extra = citation_tree(annos)
        tree, extra = citation_tree(protc, html_head=(titletag('citation tree'),))
        return extra.html

    @app.route('/curation/ast', methods=['GET'])
    def route_ast():
        #return '<pre>' + protc.parentless() + '</pre>'
        return render_ast()

    @app.route('/curation/papers', methods=['GET'])
    def route_papers():

        return htmldoc(render_idents(papers(annos), statistics(annos)),
                       title='papers',
                       styles=(table_style,))

    @app.route('/curation/annotations', methods=['GET'])
    def route_annotations():
        stats = statistics(annos)
        total = sum(stats.values())
        rows = []
        for hl, count in stats.items():
            if hl is None:
                hl = 'None'
            expanded = hypothesis_local(hl)
            rows.append([atag(expanded, hl), atag(hutils.search_url(url=expanded), count)])
        return htmldoc(render_table(sorted(rows), f'HLN n={len(stats)}', f'Annotation count n={total}'),
                       title='annotations by paper',
                       styles=(table_style,))

    @app.route('/curation/annotations/<id>', methods=['GET'])
    def route_annotations_star(id):
        return '<html>' ''.join((
            # repr(HypothesisHelper.byId(id)).replace('\n', '<br>\n'),
            Hybrid.byId(id).__repr__(html=True, number='').replace('\n', '<br>\n'),
            protc.byId(id).__repr__(html=True, number='').replace('\n', '<br>\n'),
            )) + '</html>'

    @app.route('/curation/tags', methods=['GET'])
    def route_tags():
        def uriconv(v):
            uri = request.base_url + '/' + v
            return uri

        ptags = {t:len([p for p in v if p.isAstNode]) for t, v in protc._tagIndex.items()}
        def renderprotct(tag, acount):
            count = ptags.get(tag, 0)
            sc = str(count)
            link = atag(uriconv(tag + '/annotations'), sc) + '\u00A0' * (5 - len(sc))  # will fail with > 9999 annos (heh)
            if count > acount:
                return link + '+'
            elif count == acount:
                return link
            elif not count:
                return ''
            else:
                return link + '-'
        tag_docs = readTagDocs()
        def rendertagname(tag):
            if tag in tag_docs and tag_docs[tag].deprecated:
                return deltag(tag)
            else:
                return tag

        skip = ('RRID:', 'NIFORG:', 'CHEBI:', 'SO:')
        atags = {t:len(v) for t, v in Hybrid._tagIndex.items()}
        tags = [[atag(uriconv(t), rendertagname(t)),
                 atag(hutils.search_url(tag=t), d),
                 renderprotct(t, d)]
                for t, d in atags.items()
                if all(p not in t for p in skip)]

        total = sum([c for t, c in atags.items() if all(p not in t for p in skip)])
        ptotal = sum([c for t, c in ptags.items() if all(p not in t for p in skip)])

        return htmldoc(render_table(sorted(tags),
                                    f'Tags n={len(tags)}',
                                    #f'Count n={sum(int(v.split(">",1)[1].split("<")[0]) for _, v in tags)}'
                                    f'Count n={total}',
                                    f'Count n={ptotal}'),
                       title='Tags',
                       styles=(table_style,))

    @app.route('/curation/tags/<tagname>', methods=['GET'])
    def route_tags_star(tagname):
        try:
            return markdown(addDocLinks(request.base_url.rsplit('/',1)[0],
                                        readTagDocs()[tagname].doc))  # sure it is slow but it allows live updates
        except KeyError:
            return abort(404)

    @app.route('/curation/tags/<tagname>/annotations', methods=['GET'])
    def route_tags_star_annos(tagname):
        HYB = request.url + '#Hybrids'
        PTC = request.url + '#protcs'
        try:
            return htmldoc(''.join(
                [f'<a href="{HYB}" id="protcs"><b>protcs</b></a><br>\n'] +
                [a.__repr__(html=True, number=n + 1).replace('\n', '<br>\n')
                 for n, a in  enumerate(sorted(protc.byTags(tagname), key=lambda p: p.ast_updated, reverse=True))] +
                [f'<br>\n<a href="{PTC}" id="Hybrids"><b>Hybrids</b></a><br>\n'] +
                [a.__repr__(html=True, number=n + 1).replace('\n', '<br>\n')
                 for n, a in  enumerate(sorted(Hybrid.byTags(tagname), key=lambda p: p.ast_updated, reverse=True))]
                ),
                           title=f'{tagname} annotations',
                           styles=(table_style, monospace_body_style, details_style))
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

    return app

def main():
    from core import annoSync
    get_annos, annos, stream_loop = annoSync('/tmp/protcur-server-annos.pickle',
                                             helpers=(Hybrid, protc,))
                                             #helpers=(HypothesisHelper, Hybrid, protc,))
    stream_loop.start()
    #[HypothesisHelper(a, annos) for a in annos]
    [Hybrid(a, annos) for a in annos]
    [protc(a, annos) for a in annos]

    app = make_app(annos)
    app.debug = False
    app.run(host='localhost', port=7000, threaded=True)  # nginxwoo
    os.sys.exit()

if __name__ == '__main__':
    main()
