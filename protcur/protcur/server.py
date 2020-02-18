#!/usr/bin/env python3.7
"""protcur development server
Usage:
    protcur-server [options]

Options:
    -p --port=PORT    tcp port for server [default: 7000]
    -n --no-comment   do not render comments
    -s --sync         sync
    --sparc           run sparc main
"""
import os
import re
import subprocess
from pathlib import Path
from datetime import date
from markdown import markdown
from hyputils.hypothesis import HypothesisUtils, ucd
from htmlfn import htmldoc, atag, deltag, titletag, render_table, zerotag, zeronotetag, h1tag
from htmlfn import monospace_body_style, table_style, details_style, ttl_html_style, emacs_style
from protcur.core import url_doi, url_pmid, log as protcur_log
from protcur.core import readTagDocs, justTags, addDocLinks
from protcur.sparc import SparcMI
from protcur.analysis import Hybrid, protc
from protcur.analysis import hypothesis_local, get_hypothesis_local
from protcur.analysis import citation_tree, papers, statistics, ast_statistics
from flask import Flask, request, abort 
try:
    breakpoint
except NameError:
    from IPython import embed as breakpoint


log = protcur_log.getChild('server')
PID = os.getpid()
THIS_FILE = Path(__file__).absolute()

hutils = HypothesisUtils(username='')

def uriconv(v):
    uri = os.path.join(request.base_url + v)
    return uri

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

def render_idents(idents, stats, ast_stats):
    HLN, DOI, PMID, ISBN, PDOI = 'hl:', 'DOI:', 'PMID:', 'ISBN:', 'protc:parent-doi'
    records = []
    for hl, others in sorted(idents.items()):
        hl_uri = hypothesis_local(hl)
        doi = atag(others[DOI], new_tab=True, uriconv=url_doi) if DOI in others else ''
        pmid = atag(others[PMID], new_tab=True, uriconv=url_pmid) if PMID in others else ''
        isbn = others[ISBN] if ISBN in others else ''
        pdoi = atag(others[PDOI], new_tab=True, uriconv=url_doi) if PDOI in others else ''
        count = atag(hutils.search_url(url=hl_uri), stats[hl], new_tab=True)
        ast_count = atag(uriconv(HLN + hl + '/annotations'), ast_stats[hl], new_tab=True)
        records.append([atag(hl_uri, hl, new_tab=True), doi, pmid, isbn, pdoi, count, ast_count])
        continue

    return render_table(records, HLN, DOI, PMID, ISBN, PDOI, '# annos', '# ast')

def render_2col_table(dict_, h1, h2, uriconv=lambda a:a):  # FIXME this sucks and uriconv only works on the first row...
    output = []
    output.append(table_style)
    output.append(f'<tr><th>{h1}</th><th>{h2}</th></tr>')
    for hl_name, thing in sorted(dict_.items()):
        output.append(f'<tr><th><a href={uriconv(hl_name)}>{hl_name}</a></th>'
                      f'<th>{thing}</th></tr>')
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

colorizer_command = THIS_FILE.resolve().parent.parent / 'bin/colorizer.lisp'
ast_file = Path(f'{ucd}/protcur/protc-ast-render.rkt')
ast_html_file = Path(f'{ucd}/protcur/protc-ast-render.html')
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
        subprocess.check_output([colorizer_command.as_posix(), ast_file.as_posix()])

        with open(ast_html_file.as_posix(), 'rt') as f:
            html_uncorrected = f.read()

        html_holder[0] = correct_colorized(html_uncorrected)

    return html_holder[0]


def star_annos(ast, funcname, search_by):
    ast_ifunc = getattr(ast, funcname)
    simple_ifunc = getattr(Hybrid, funcname)
    HYB = request.url + '#Hybrids'
    SPC = request.url + f'#{ast.namespace}'
    try:
        join = ast._repr_join.replace('\n', '<br>\n').join
        return htmldoc(''.join(
            [f'<div class="{ast.namespace}">\n',
             f'<a href="{HYB}" id="{ast.namespace}"><b>{ast.namespace}</b></a><br>\n',
             join([a.__repr__(html=True, number=atag(HYB + str(n + 1),
                                                     n + 1,
                                                     id=ast.namespace + str(n + 1)))
                   for n, a in enumerate(sorted(ast_ifunc(search_by),
                                                key=lambda p: p.ast_updated, reverse=True))]),
             '\n</div>\n',
             f'<br>\n<a href="{SPC}" id="Hybrids"><b>Hybrids</b></a><br>\n'] +
            [a.__repr__(html=True, number=atag(SPC + str(n + 1),
                                               n + 1,
                                               id='Hybrids' + str(n + 1))).replace('\n', '<br>\n')
             for n, a in  enumerate(sorted(simple_ifunc(search_by),
                                           key=lambda p: p.ast_updated, reverse=True))]),
                       title=f'{search_by} annotations',
                       styles=(table_style, monospace_body_style, details_style,
                               ttl_html_style, emacs_style,  # paren style: just in case
                               (f'.{ast.namespace} '
                                'a:link { text-decoration: none; } '
                                'body { white-space: nowrap; }')))
    except KeyError:
        return abort(404)


def make_ast(nodes_to_render, ast):
        join = ast._repr_join.replace('\n', '<br>\n').join
        return htmldoc(join(
            [a.__repr__(html=True, number=n + 1)
             for n, a in enumerate(nodes_to_render)]),
                       title=f'EVERYTHING',
                       styles=(table_style, monospace_body_style, details_style,
                               ttl_html_style, emacs_style,
                               (f'.{ast.namespace} '
                                'a:link { text-decoration: none; } '
                                'body { white-space: nowrap; }')))


def make_app(annos):
    app = Flask('protc curation id service')

    # routes

    @app.route('/curation/citations', methods=['GET'])
    def route_citations():
        title = 'citation tree'
        all = request.args.get('all', False)
        if all:
            all = True if all.lower() == 'true' else False
        tree, extra = citation_tree(protc, html_head=(titletag(title),), all=all)
        if extra:
            return extra.html
        else:
            return htmldoc('No citations yet!', title=title)

    @app.route('/curation/ast', methods=['GET'])
    def route_ast():
        ast = protc
        everything = sorted((o for o in ast if o is not None and o.isAstNode and not o.hasAstParent))
        return make_ast(everything, ast)

    @app.route('/curation/fast', methods=['GET'])
    def route_fast():
        ast = protc
        everything = (o for o in ast if o is not None and o.isAstNode and not o.hasAstParent)
        return make_ast(everything, ast)

    @app.route('/curation/papers', methods=['GET'])
    @app.route('/curation/papers/', methods=['GET'])
    def route_papers():

        return htmldoc(render_idents(papers(annos),
                                     statistics(annos),
                                     ast_statistics(protc)),
                       title='papers',
                       styles=(table_style,))

    @app.route('/curation/papers/<paper_id>/annotations', methods=['GET'])
    def route_papers_star_annos(paper_id):
        if 'hl:' not in paper_id:
            return abort(404)
        iri = hypothesis_local(paper_id.split(':', 1)[-1])  # FIXME risk of fail if no visit to documents
        #log.debug(f'iri? {iri!r}')
        return star_annos(protc, 'byIri', iri)

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
    @app.route('/curation/tags/', methods=['GET'])
    def route_tags():
        ptags = {t:len([p for p in v if p.isAstNode])
                 for t, v in protc._tagIndex.items()}

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

        skip = ('RRID:', 'NIFORG:', 'CHEBI:', 'SO:', 'sparc:', 'DEADBEEF')
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
                                    f'Raw Anno Count n={total}',
                                    f'Protc Count n={ptotal}'),
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
        if tagname not in protc._tagIndex:  # FIXME make sure this is populated
            return abort(404)

        return star_annos(protc, 'byTags', tagname)

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


def make_sparc(app=Flask('sparc curation services'), debug=False, comments=True):
    from protcur.sparc import oqsetup
    from scibot.papers import KeyAccessor  # TODO
    OntTerm, ghq = oqsetup()
    SparcMI.graph = ghq.graph
    sparc_ents = OntTerm.search(None, prefix='sparc')
    all_tags = set(t.curie for t in sparc_ents)
    tags = '\n'.join(sorted(t.curie for t in sparc_ents))
    skip = ('protc:', 'RRID:', 'NIFORG:', 'CHEBI:', 'SO:',
            'PROCUR:', 'mo:', 'annotation-', 'RRIDCUR:', 'DEADBEEF')

    if debug:
        from pyontutils.core import OntId
        from pyontutils.closed_namespaces import rdf, rdfs, owl
        breakpoint()
        import sys
        sys.exit()

    def makelink(value):
        return uriconv(value).replace('tabulation', 'tags')  # FIXME ick

    def make_tags():
        ptags = {t:len([p for p in v if p.isAstNode]) for t, v in SparcMI._tagIndex.items()}
        def rendersparct(tag, acount):
            count = ptags.get(tag, 0)
            sc = str(count)
            link = atag(makelink(tag + '/annotations'), sc) + '\u00A0' * (5 - len(sc))  # will fail with > 9999 annos (heh)
            if count > acount:
                return link + '+'
            elif count == acount:
                return link
            elif not count:
                return ''
            else:
                return link + '-'

        tag_docs = SparcMI.makeTagDocs(comments=comments)
        def rendertagname(tag, acount):
            if tag in tag_docs and tag_docs[tag].deprecated:
                return deltag(tag)
            elif acount == 0:
                if tag in tag_docs and tag_docs[tag].editorNote:
                    return zeronotetag(tag)
                else:
                    return zerotag(tag)
            else:
                return tag

        atags = {t:0 for t in tag_docs}
        atags.update({t:len(v) for t, v in Hybrid._tagIndex.items()})
        _tags = [[atag(makelink(t), rendertagname(t, d)),
                  atag(hutils.search_url(tag=t), d),
                  rendersparct(t, d),
                  ' '.join(tag_docs[t].types) if t in tag_docs else '',
                  tag_docs[t].modality if t in tag_docs else '',
                  tag_docs[t].editorNote if t in tag_docs else '']
                for t, d in atags.items()
                if all(p not in t for p in skip)]
        tags = sorted(_tags, key=lambda t:(t[4], t[3]))  # sort by type

        return tags, atags, ptags, tag_docs

    def make_tags_header(tags, atags, ptags, tag_docs, modality=None):
        sparc_any = len([t for t, d  in atags.items()
                         if 'sparc:' in t and d > 0 and
                         (modality is None or tag_docs[t].modality == modality)])
        sparc_total = len([t for t in atags if 'sparc:' in t and
                           (modality is None or tag_docs[t].modality == modality)])
        total = sum([c for t, c in atags.items() if all(p not in t for p in skip) and
                     (modality is None or t in tag_docs and tag_docs[t].modality == modality)])
        ptotal = sum([c for t, c in ptags.items() if all(p not in t for p in skip) and
                      (modality is None or t in tag_docs and tag_docs[t].modality == modality)])

        ntags = len([t for t in atags if modality is None or
                     t in tag_docs and tag_docs[t].modality == modality])
        dta = len([t for t, d in tag_docs.items()
                   if (modality is None or d.modality == modality) and
                   'owl:DatatypeProperty' in d.types and
                   atags[t] > 0])
        dtt = len([t for t, d in tag_docs.items()
                   if (modality is None or d.modality == modality) and
                   'owl:DatatypeProperty' in d.types])
        return (f'sparc any/total={sparc_any}/{sparc_total} DTProps {dta}/{dtt}',
                #f'Count n={sum(int(v.split(">",1)[1].split("<")[0]) for _, v in tags)}'
                f'Count n={total}',
                f'Converted n={ptotal}',
                'Types',
                'Modality',
                'Comment')

    @app.route('/sparc/documents', methods=['GET'])
    @app.route('/sparc/documents/', methods=['GET'])
    def sparc_documents():
        hls = set(get_hypothesis_local(uri) for uri in SparcMI.uris)
        hls |= set(uri for uri in SparcMI.uris if 'dropbox' in uri)
        # doesn't work due to temp iris in iframe
        #print(hls)
        p = {k:v for k, v in papers(SparcMI._annos_list).items() if k in hls}
        s = {k:v for k, v in statistics(SparcMI._annos_list).items() if k in hls}
        ast = {k:v for k, v in ast_statistics(SparcMI).items() if k in hls}
        return htmldoc(render_idents(p, s, ast),
                       title='documents',
                       styles=(table_style,))

    @app.route('/sparc/documents/<document_id>/annotations', methods=['GET'])
    def sparc_documents_star_annos(document_id):
        if 'hl:' not in document_id:
            return abort(404)
        iri = hypothesis_local(document_id.split(':', 1)[-1])  # FIXME risk of fail if no visit to documents
        #log.debug(f'iri? {iri!r}')
        return star_annos(SparcMI, 'byIri', iri)

    @app.route('/sparc/annotations/<id>', methods=['GET'])
    def sparc_annotations_star(id):
        return '<html>' ''.join((
            # repr(HypothesisHelper.byId(id)).replace('\n', '<br>\n'),
            Hybrid.byId(id).__repr__(html=True, number='').replace('\n', '<br>\n'),
            SparcMI.byId(id).__repr__(html=True, number='').replace('\n', '<br>\n'),
            )) + '</html>'

    @app.route('/sparc/tabulation', methods=['GET'])
    @app.route('/sparc/tabulation/', methods=['GET'])
    def sparc_tabulation():
        """ data type properties by modality """
        tags, atags, ptags, tag_docs = make_tags()
        pairs = [(h1tag(modality, id=modality), render_table([[t.replace(' ', '\xA0') if i == 4 else t
                                                  for i, t in enumerate(row)]
                                                 for row in tags if row[4] == modality],
                                                *make_tags_header(tags, atags, ptags, tag_docs, modality)))
                 for modality in ('general', 'cell culture', 'ephys', 'histology',
                                  'microscopy', 'optical', 'radiology',
                                  'transcriptomics', 'various')]
        return htmldoc(*(thing for ab in pairs for thing in ab),
                       title='tabulations',
                       styles=(table_style,))

    @app.route('/sparc/tags', methods=['GET'])
    @app.route('/sparc/tags/', methods=['GET'])
    def sparc_tags():
        tags, atags, ptags, tag_docs = make_tags()

        tags_header = make_tags_header(tags, atags, ptags, tag_docs)
        return htmldoc(render_table(tags,
                                    *tags_header),
                       title='Tags',
                       styles=(table_style,))

    @app.route('/sparc/tags/<tagname>', methods=['GET'])
    def sparc_tags_star(tagname):
        try:
            tag_docs = SparcMI.makeTagDocs()
            td = tag_docs[tagname]
            domains = '` `'.join(td.domain)
            ranges = '` `'.join(td.range)
            doc = td.doc + (f'  \ndomain: `{domains}`'
                            if domains else '') + (f'  \nrange: `{ranges}`'
                                                   if ranges else '')
            return markdown(addDocLinks(request.base_url.rsplit('/',1)[0],
                                        doc))
        except KeyError:
            return abort(404)

    @app.route('/sparc/tags/<tagname>/annotations', methods=['GET'])
    def sparc_tags_star_annos(tagname):
        if tagname not in all_tags:
            return abort(404)

        return star_annos(SparcMI, 'byTags', tagname)

    @app.route('/sparc/ast')
    @app.route('/sparc/all-annotations<extension>')
    def sparc_all_annotations_ttl(extension='.html'):
        if not extension.startswith('.'):
            return abort(404)

        extension = extension[1:]

        if extension == 'html':
            body = SparcMI.html()
            return htmldoc(body,
                           title='all-annotations',
                           styles=(table_style, ttl_html_style))
        else:  # TODO more
            return SparcMI.ttl(), 200, {'Content-Type':'text/plain; charset=utf-8'}

    @app.route('/sparc/coverage')
    @app.route('/sparc/coverage<extension>')
    def sparc_coverage(extension='.tsv'):
        if not extension.startswith('.'):
            return abort(404)

        extension = extension[1:]
        return SparcMI.report(format=extension), 200, {'Content-Type':'text/plain; charset=utf-8'}

    @app.route('/sparc', methods=['GET'])
    @app.route('/sparc/', methods=['GET'])
    def sparc_curation():
        body = []
        for route in 'documents', 'tags', 'ast', 'coverage', 'tabulation':
            url = request.base_url + route
            body.append(atag(url, route, new_tab=True) + '<br>\n')
        return htmldoc(*body,
                       title='sparc curation dashboard')

    @app.route('/sparc/controlled-tags', methods=['GET'])
    def sparc_controlled_tags():
        """
        The js required for this in the google docs script editory is quite simple.
        ```
        function loadFromUrl() {
        var url = "https://url.to/your/controlled-tags";
        var doc = DocumentApp.openById("GOOGLE-DOC-IDENTIFIER");
        var paragraph = doc.getParagraphs()[0];
        var resp = UrlFetchApp.fetch(url);
        var newText = resp.getContentText();
        paragraph.setText(newText);
        }
        ```"""
        return tags, 200, {'Content-Type':'text/plain; charset=utf-8'}

    @app.route('/sparc/tabulations')
    def sparc_tabulations():
        return 'TODO'

    @app.route('/sparc/tabulations/<experiment>')
    def sparc_tabulations_document(experiment):
        """ TODO not quite document level, more experiment level """
        return 'TODO'

    @app.before_first_request
    def sparc_runonce():
        # populate existing iris
        # FIXME might still cause issue if someone types a new paper
        # into the url bar before updating the documents page
        hls = set(get_hypothesis_local(uri) for uri in SparcMI.uris)

    return app


def make_server_app(memfile=None, comments=True):
    import atexit
    from protcur.core import annoSync
    helpers = Hybrid, protc, SparcMI
    get_annos, annos, stream_thread, exit_loop = annoSync(memfile,
                                                          helpers=helpers)
    stream_thread.start()

    # FIXME put this on AstGeneric
    for t in justTags():
        protc._tagIndex[t] = set()
        Hybrid._tagIndex[t] = set()

    for h in helpers:
        h._annos_list = annos
        if not h._annos_list:
            h._done_loading = True  # FIXME hack
            h._tagIndex['DEADBEEF'] = set()  # HACK for empty case so annos will add
        [h(a, h._annos_list) for a in h._annos_list]  # TODO stepping stone to passing annos as a dict
        [o.populateTags() for o in h]

    app = make_app(annos)
    make_sparc(app, comments=comments)

    atexit.register(exit_loop)
    app.exit_loop = exit_loop
    return app


def main():
    from docopt import docopt, parse_defaults
    from hyputils.hypothesis import group_to_memfile, group
    args = docopt(__doc__)
    defaults = {o.name:o.value if o.argcount else None for o in parse_defaults(__doc__)}
    port = int(args['--port'])
    comments = not args['--no-comment']

    _, ghash = group_to_memfile(group).as_posix().rsplit('-', 1)
    ghashshort = ghash[:10]

    if args['--sparc']:
        port = port if port != int(defaults['--port']) else None
        memfile = f'{ucd}/protcur/sparc-{port}-{ghashshort}-server-annos.json'
        return sparc_main(memfile, args, port)

    app = make_server_app(f'{ucd}/protcur/protcur-{port}-{ghashshort}-server-annos.json', comments)
    app.debug = False
    app.run(host='localhost', port=port, threaded=True)
    app.exit_loop()


def sparc_main(memfile, args=None, port=None):
    if port is None:
        port = 7001

    from core import annoSync
    get_annos, annos, stream_thread, exit_loop = annoSync(memfile,
                                                          #tags=('sparc:',),
                                                          helpers=(SparcMI,))
    #stream_thread.start()
    [protc(a, annos) for a in annos]
    [SparcMI(a, annos) for a in annos]
    SparcMI.byTags('sparc:lastName')
    app = make_sparc()
    app.debug = False
    app.run(host='localhost', port=port, threaded=True)  # nginxwoo


if __name__ == '__main__':
    main()
