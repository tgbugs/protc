#!/usr/bin/env python3.6

import os
import re
import ast
from collections import Counter
from IPython import embed
from pyontutils.hierarchies import creatTree
from pyontutils.utils import makeGraph, makePrefixes, async_getter
from pyontutils.scigraph_client import Vocabulary
import parsing
from scibot.hypothesis import HypothesisAnnotation

sgv = Vocabulary(cache=True)
RFU = 'protc:references-for-use'

error_output = []

# utility

def get_hypothesis_local(uri):
    if 'hypothesis-local' in uri:
        return os.path.splitext(os.path.basename(uri))[0]

def hypothesis_local(hln):
    return 'http://hypothesis-local.olympiangods.org/' + hln + '.pdf'

def url_doi(doi):
    return 'https://doi.org/' + doi

def url_pmid(pmid):
    return 'https://www.ncbi.nlm.nih.gov/pubmed/' + pmid

# docs

def readTagDocs():
    with open('../protc-tags.rkt', 'rt') as f:
        text = f.read()
    success, docs, rest = parsing.tag_docs(text)
    tag_lookup = {tag:doc for _, tag, doc in docs}
    return tag_lookup

def addDocLinks(base_url, doc):
    prefix = base_url + '/'
    return re.sub(r'`((?:protc|mo):[^\s]+)`', rf'[\1]({prefix}\1)', doc)

# stats

def citation_tree(annos):
    p = RFU
    trips = []
    for anno in annos:
        hl = get_hypothesis_local(anno.uri)
        if hl:
            s = hl
            if p in anno.tags and 'TODO' not in anno.tags:
                if 'no access' in anno.text:
                    continue  # there are some cases where TODO is missing
                t = anno.text.strip()
                o = get_hypothesis_local(t)
                o = o if o else t
                trips.append((p, s, o))

    return trips

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

def statistics(annos):
    stats = {}
    for anno in annos:
        hl = str(get_hypothesis_local(anno.uri))
        if hl not in stats:
            stats[hl] = 0
        stats[hl] += 1
    return stats

def tagdefs(annos):
    tags = Counter()
    for anno in annos:
        for tag in anno.tags:
            tags[tag] += 1
    return dict(tags)

def idFromShareLink(link):  # XXX warning this will break
    if 'hyp.is' in link:
        id_ = link.split('/')[3]
        return id_

def shareLinkFromId(id_):
    return 'https://hyp.is/' + id_

def shareLinkFromAnno(anno):
    if anno.type == 'reply':
        #print(f'WARNING: Reply {anno.id} to {shareLinkFromId(parent.id)}')
        parent = getParentForReply(anno)
        return shareLinkFromId(parent.id)
    else:
        return shareLinkFromId(anno.id)

def splitLines(text):
    for line in text.split('\n'):
        yield line

def inputRefs(annos):
    for anno in annos:
        if any(t in anno.tags for t in ('protc:input', 'protc:*measure', 'protc:symbolic-measure')):
            for line in splitLines(anno.text):
                if line:
                    id_ = idFromShareLink(line)
                    if id_:
                        yield id_

def getAnnoById(id_):
    try:
        return [_ for _ in annos if _.id == id_][0]
    except IndexError:
        print('could not find', id_, shareLinkFromId(id_))
        return None

class AstNode:
    def __init__(self, type_, value, anno, children):
        self.type_ = type_
        self.value = value.strip() if type(value) == str else value
        self.anno_id = anno.id
        self.children = children
        self.shareLink = shareLinkFromAnno(anno)

    def __repr__(self, top=True, depth=1, nparens=1, plast=True):
        link = f'  ; {self.shareLink}'

        if self.children:
            linestart = '\n' + ' ' * 2 * depth
            nsibs = len(self.children)
            #childs = link + linestart + linestart.join(c.__repr__(False, depth + 1, nparens + 1 if nsibs == i + 1 else nparens)
                                                       #for i, c in
                                                       #enumerate(self.children))
            cs = []
            for i, c in enumerate(self.children):
                new_plast = i + 1 == nsibs
                # if we are at the end of multiple children the child node needs to add one more paren
                if new_plast:
                    new_nparens = nparens + 1
                else:
                    new_nparens = 1  # new children start their own tree, nparens only tracks the last node
                try:
                    s = c.__repr__(False, depth + 1, new_nparens, new_plast)
                except TypeError:
                    raise TypeError('%s is not an AstNode' % c)
                cs.append(s)
            childs = link + linestart + linestart.join(cs)
        else:
            childs = ')' * nparens + link  

        if self.type_ == 'protc:parameter*' or self.type_ == 'protc:invariant':
            value = repr(self.value)
        else:
            value = '"' + self.value.replace('"', '\\"') + '"'

        start = '\n(' if top else '('
        return f'{start}{self.type_} {value}{childs}'

    def __eq__(self, other):
        return self.anno_id == other.anno_id

    def __gt__(self, other):
        return repr(self) > repr(other)

    def __lt__(self, other):
        return not self.__gt__(other)

def getParentForReply(anno):
    if anno.type != 'reply':
        return anno
    else:
        return getParentForReply(getAnnoById(anno.references[0]))

def basic_start(anno):
    if anno.text and not anno.text.startswith('SKIP') and not anno.text.startswith('https://hyp.is') and 'RRID' not in anno.text:
        value = anno.text
        error = ('text found for annotaiton in addition to exact\n'
                 f'exact: {anno.exact}\n'
                 f'text: {anno.text}')
        error_output.append(error)
    elif anno.exact is not None:  # not a reply
        value = anno.exact
    elif anno.type == 'reply':
        value = ''
    else:
        raise ValueError(f'{anno.id} {shareLinkFromAnno(anno)} has no text and no exact and is not a reply.')
    return value

class ParameterValue:
    def __init__(self, success, v, rest, front):
        self.value = success, v, rest, front
    def __repr__(self):
        success, v, rest, front = self.value
        if not success:
            out = str((success, v, rest))
        else:
            out = v + f' (rest-front "{rest}" "{front}")'
        return out

test_params = []
def protc_parameter(anno):
    value = basic_start(anno)
    #cleaned = value.strip(' ').strip('(')
    cleaned = value.replace(' mL–1', ' * mL–1').replace(' kg–1',' * kg–1')  # FIXME temporary (and bad) fix for superscript issues
    cleaned = cleaned.strip()
    #cleaned = value.strip()
    cleaned_orig = cleaned

    # ignore gargabe at the start
    success = False
    front = ''
    while not success:
        success_always_true, v, rest = parsing.parameter_expression(cleaned)
        success = v[0] != 'param:parse-failure'
        if not success:
            if len(cleaned) > 1:
                more_front, cleaned = cleaned[0], cleaned[1:]
                front += more_front
            else:
                front += cleaned
                success, v, rest = parsing.parameter_expression(cleaned_orig)  # reword but whatever
                error_output.append((success, v, rest))
                break

    def format_unit_atom(param_unit, name, prefix=None):
        if prefix is not None:
            return f"({param_unit} '{name} '{prefix})"
        else:
            return f"({param_unit} '{name})"

    def format_value(list_):
        out = []
        if list_:
            if 0:  # list_[0] == 'param:unit':  # TODO unit atom, unit by itself can be much more complex
                return format_unit(*list_)
            else:
                for v in list_:
                    if type(v) is list:
                        v = format_value(v)
                    if v is not None:
                        out.append(f'{v}')
        if out:
            return '(' + ' '.join(out) + ')'

    if v is not None:
        v = format_value(v)
    out = ParameterValue(success, v, rest, front)
    test_params.append((value, (success, v, rest)))
    return out

def protc_invariant(anno):
    return protc_parameter(anno)

test_input = []
def protc_input(anno):
    value = basic_start(anno)
    value = value.strip()
    data = sgv.findByTerm(value)  # TODO could try the annotate endpoint?
    if data:
        subset = [d for d in data if value in d['labels']]
        if subset:
            data = subset[0]
        else:
            data = data[0]  # TODO could check other rules I have used in the past
        id_ = data['curie'] if 'curie' in data else data['iri']
        value += f" ({id_}, {data['labels'][0]})"
    else:
        test_input.append(value)

    return value

def valueForAnno(anno):
    #type
    if anno.tags:
        type_ = [_ for _ in anno.tags if 'protc:' in _][0]  # XXX this will fail in nasty ways
    else:
        print('Anno with no tag!', shareLinkFromAnno(anno))
        type_ = None  # just see where it goes...

    # value
    if type_ == 'protc:parameter*':
        value = protc_parameter(anno)
    elif type_ == 'protc:invariant':
        value = protc_invariant(anno)
    elif type_ == 'protc:input':
        value = protc_input(anno)
    #elif type_ == 'protc:*measure':
    #elif type_ == 'protc:symbolic-measure':
    else:
        value = basic_start(anno)
    return type_, value

def buildAst(anno, implied_lookup, correction_lookup, trees, done, depth=0):
    # check anno.tags for one of our known good tags
    # give that tag, switch to something for that tag
    # check anno.text for hyp.is
    # for line in splitLines(anno.text):
    #   buildAst(getAnnoById(idFromShareLink(line))
    
    if anno.id in done:  # woo memoization
        return done[anno.id]

    # corrections
    if anno.id in correction_lookup:
        corr = correction_lookup[anno.id]
        ctags = [t for t in corr.tags if t != 'PROTCUR:correction']
        if corr.text and not corr.text.startswith('SKIP'):
            anno._orig_text = anno.text  # woo monkey patttcc
            anno.text = corr.text
        if ctags:
            anno._orig_tags = anno.tags
            anno.tags = ctags

    type_, value = valueForAnno(anno)
    # next level
    children = []
    if not anno.text.startswith('SKIP'):
        for line in splitLines(anno.text):
            if line:
                id_ = idFromShareLink(line)
                if id_ is not None:
                    child = getAnnoById(id_)
                    if child is None: # sanity
                        print('Problem in', shareLinkFromAnno(anno))
                        continue
                    try:
                        subtree = buildAst(child, implied_lookup, correction_lookup, trees, done, depth + 1)  # somehwere we try to get [0] and it fails if anno is none... ctrl a n
                        children.append(subtree)
                    except RecursionError:
                        print('Circular link in', shareLinkFromAnno(anno))

    out = AstNode(type_, value, anno, children)
    #if type_ == 'protc:parameter*' and anno.id in implied_lookup:
    if anno.id in implied_lookup:
        a, atext = implied_lookup[anno.id]
        type_ = [t for t in a.tags if t.startswith('protc:implied-')][0]
        out = AstNode(type_, atext, a, [out])
    if depth == 0:
        trees.append(out)
    done[anno.id] = out

    # restore corrected
    if anno.id in correction_lookup:
        if hasattr(anno, '_orig_text'):
            anno.text = anno._orig_text
        if hasattr(anno, '_orig_tags'):
            anno.tags = anno._orig_tags
    return out

def makeAst():
    """ Outer wrapper that manages setup for buildAst """
    implied_lookup = {a.references[0]:
                     (a, a.text.split(':')[1].split('\n')[0].strip())
                     if ':' in a.text
                     else (a, a.text)
                     for a in annos
                     if 'protc:implied-aspect' in a.tags}# or 'protc:implied-output' in a.tags}  # FIXME implied-output is wack
    correction_lookup = {a.references[0]:a for a in annos if 'PROTCUR:correction' in a.tags}
    trees = []
    done = {}
    head_tags = 'protc:input', 'protc:implied-input', 'protc:*measure', 'protc:symbolic-measure', 'protc:output'  # FIXME output issues
    skip_tags = 'PROTCUR:correction',
    for anno in annos:
        if any(t in anno.tags for t in head_tags) and not any(t in anno.tags for t in skip_tags):
            buildAst(anno, implied_lookup, correction_lookup, trees, done)
    # TODO we need a check somewhere that alerts when an input is not an output and has no parents -> dangling curation status
    #(protc:implied-input "pcr mix solution")  ; https://hyp.is/WxoSjG46EeewpVfX6gocoQ,  problematic with one text overwriting the other
    #  I think I need to make it so that non correction replies don't overwrite??? max confusion
    return trees

def main():
    from pprint import pformat
    from protcur import get_annos, get_annos_from_api, start_loop
    from time import sleep
    import requests

    mem_file = '/tmp/protocol-annotations.pickle'

    global annos  # this is too useful not to do
    annos = get_annos(mem_file)  # TODO memoize annos... and maybe start with a big offset?
    stream_loop = start_loop(annos, mem_file)

    input_text_args = [(basic_start(a).strip(),) for a in annos if 'protc:input' in a.tags or 'protc:output' in a.tags]
    async_getter(sgv.findByTerm, input_text_args)  # prime the cache FIXME issues with conflicting loops...

    stream_loop.start()

    i = identifiers(annos)

    t = citation_tree(annos)
    PREFIXES = {'protc':'http://protc.olympiangods.org/curation/tags/',
                'hl':'http://hypothesis-local.olympiangods.org/'}
    PREFIXES.update(makePrefixes('rdfs'))
    g = makeGraph('', prefixes=PREFIXES)
    for p, s, o in t:
        su = hypothesis_local(s)
        ou = hypothesis_local(o)
        g.add_node(su, p, ou)
        g.add_node(su, 'rdfs:label', s)  # redundant
        g.add_node(ou, 'rdfs:label', o)  # redundant
    ref_graph = g.make_scigraph_json(RFU, direct=True)
    tree, extra = creatTree('hl:ma2015.pdf', RFU, 'OUTGOING', 10, json=ref_graph)

    irs = sorted(inputRefs(annos))


    trees = makeAst()
    with open('/tmp/protcur.rkt', 'wt') as f:
        f.write(repr(sorted(trees)))

    test_inputs = sorted(set(test_input))
    def check_inputs():
        with open(os.path.expanduser('~/files/bioportal_api_keys'), 'rt') as f:
            bioportal_api_key = f.read().strip()
        def getBiop(term):
            #url = f'http://data.bioontology.org/search?q={term}&ontologies=CHEBI&apikey={bioportal_api_key}'
            url = f'http://data.bioontology.org/search?q={term}&apikey={bioportal_api_key}'
            print(url)
            return requests.get(url)

        res = [(t, getBiop(t)) for t in test_inputs]
        jsons = [(t, r.json()) for t, r in res if r.ok] 

        def chebis(j):
            return set((c['@id'], c['prefLabel'] if 'prefLabel' in c else tuple(c['synonym']))
                       for c in j['collection'] if 'CHEBI' in c['@id'])
        cs = [(t, chebis(j)) for t, j in jsons]
        cs = set((t, r) for t, a in cs for r in a if a)
        cs = sorted(((t, (c.rsplit('/',1)[-1].replace('_',':'), m)) for t, (c, m) in cs), key=lambda v:v[0])
        ids_only = sorted(set(list(zip(*cs))[1]), key=lambda v:v[0])
        check = [(i, sgv.findById(i)) for i in sorted(set(i for i, n in ids_only))]
        in_ = [c for c, v in check if v]
        in_val = [(c, v) for c, v in check if v]
        missing = [c for c, v in check if v is None]
        ids_only = [(i, n) for i, n in ids_only if i not in in_]
        cs_out = [(t, (c, n)) for t, (c, n) in cs if c not in in_]
        return res, jsons, cs_out, ids_only, missing

    #res, jsons, cs_out, ids_only, missing = check_inputs()
    #with open(os.path.expanduser('~/ni/nifstd/chebimissing2.txt'), 'wt') as f: f.write(pformat(cs_out))
    #with open(os.path.expanduser('~/ni/nifstd/chebimissing_id_names2.txt'), 'wt') as f: f.write(pformat(ids_only))
    #with open(os.path.expanduser('~/ni/nifstd/chebimissing_ids2.txt'), 'wt') as f: f.write('\n'.join(missing))

    embed()
    # HOW DO I KILL THE STREAM LOOP!??!

if __name__ == '__main__':
    main()
