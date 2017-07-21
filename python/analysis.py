#!/usr/bin/env python3.6

import os
import ast
from collections import Counter
from IPython import embed
from pyontutils.hierarchies import creatTree
from pyontutils.utils import makeGraph, makePrefixes

RFU = 'protc:references-for-use'

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

def idFromShareLink(link):
    if 'hyp.is' in link:
        id_ = link.split('/')[3]
        return id_

def shareLinkFromId(id_):
    return 'https://hyp.is/' + id_

def splitLines(text):
    for line in text.split('\n'):
        yield line

def inputRefs(annos):
    for anno in annos:
        if 'protc:input' in anno.tags:
            for line in splitLines(anno.text):
                if line:
                    id_ = idFromShareLink(line)
                    if id_:
                        yield id_

def getAnnoById(id_, annos):
    try:
        return [_ for _ in annos if _.id == id_][0]
    except IndexError:
        print('could not find', id_)
        return None

class AstNode:
    def __init__(self, type_, value, anno_id, children):
        self.type_ = type_
        self.value = value.strip()
        self.anno_id = anno_id
        self.children = children
    def __repr__(self, top=True, depth=1, nparens=1):
        link = f'  ; {shareLinkFromId(self.anno_id)}'

        if self.children:
            linestart = '\n' + ' ' * 2 * depth
            nsibs = len(self.children)
            childs = link + linestart + linestart.join(c.__repr__(False, depth + 1, nparens + 1 if nsibs == i + 1 else nparens)
                                                       for i, c in
                                                       enumerate(self.children))
        else:
            childs = ')' * nparens + link  

        start = '\n(' if top else '('
        try:
            value = int(self.value)
        except ValueError:
            value = '"' + self.value + '"'
        return f'{start}{self.type_} {value}{childs}'

def protc_parameter(anno):
    if anno.text:
        value = anno.text
        print('text found for annotaiton in addition to exact')
        print('exact:', anno.exact)
        print('text:', anno.text)
    else:
        value = anno.exact

    parts = value.split(' ')
    out = parse_mess(parts)

    return out

def parse_mess(value):
    def OR(*funcs):
        def or_(p):
            for f in funcs:
                success, v, rest = f(p)
                if success:
                    return success, v, rest
            return success, v, rest
        return or_

    def TIMES(func, min_, max_=None):
        def times(p):
            matches = []
            for i in range(min_):
                success, v, rest = func(p)
                if not success:
                    return success, None, p
                else:
                    matches.append(v)
                    p = rest
            if max_ is not None:
                for i in range(max_ - min_):
                    success, v, rest = func(p)
                    if not success:
                        return True, matches, p
                    else:
                        matches.append(v)
                        p = rest
                success, v, rest = func(p)
                if success:
                    return False, None, p

            return success, matches, rest
        return times

    infinity = 9999999  # you know it baby, if we go this deep we will get recursion errors
    def MANY(func):
        return TIMES(func, 0, infinity)

    def MANY1(func):
        return TIMES(func, 1, infinity)

    def ANDTHEN(func1, func2):
        def andthen(p):
            success, v1, rest = func1(p)
            if success:
                p2 = rest
                success, v2, rest = func2(p2)
                if success:
                    return success, [v1, v2], rest
            return success, None, p
        return andthen

    def JOINT(*funcs, join=True):
        def joint(p):
            matches = []
            rest = p
            for func in funcs:
                success, v, rest = func(rest)
                if not success:
                    return success, None, p
                else:
                    if join and hasattr(v, '__iter__'):
                        matches.extend(v)
                    else:
                        matches.append(v)
            return success, matches, rest 
        return joint

    def comp(p, val):
        if p:
            v = p[0]
        else:
            return False, None, p  # we are at the end
        return v == val, v, p[1:]

    def oper(p, func):
        if p:
            v = p[0]
        else:
            return False, None, p  # we are at the end
        return func(v), v, p[1:]

    def space(p): return comp(p, ' ')
    def plus_or_minus_symbol(p): return comp(p,'±')  # NOTE range and +- are interconvertable...
    def plus_or_minus_pair(p): return comp(p,'+-')
    plus_or_minus = OR(plus_or_minus_symbol, plus_or_minus_pair)
    def explicit_range_single(p): return comp(p,'-')
    explicit_range_pair = TIMES(explicit_range_single, 2)
    explicit_range = OR(explicit_range_pair, explicit_range_single)  # order matters since '-' is in '--'
    def lt(p): return comp(p,'<')
    def gt(p): return comp(p,'>')
    comparison = OR(lt, gt)
    def by(p): return comp(p,'x')
    op = OR(plus_or_minus, explicit_range, lt, gt, by)
    digits = [str(_) for _ in range(10)]
    def digit(p): return oper(p, lambda d: d in digits)
    def point(p): return comp(p,'.')
    def char(p): return oper(p, lambda c: c.isalpha())

    # patterns:
    # num op num unit
    # num unit op num unit
    # unit num
    # op num
    # opnum
    # numunit


    int_ = MANY1(digit)
    float_ = OR(JOINT(MANY1(digit), point, MANY(digit)),
                JOINT(MANY(digit), point, MANY1(digit)))
    num = OR(int_, float_)

    pat1 = JOINT(num, MANY(space), op, MANY(space), num, join=False)
    unit_ = MANY1(char)

    percent = '%'
    degree = '°'
    degree_c = degree + 'C'


    out = '('
    out += ')'
    embed()

    return out


def buildAst(anno, annos):
    # check anno.tags for one of our known good tags
    # give that tag, switch to something for that tag
    # check anno.text for hyp.is
    # for line in splitLines(anno.text):
    #   buildAst(getAnnoById(idFromShareLink(line))
    
    # sanity
    if anno is None:
        return None, []

    #types
    type_ = anno.tags[0]  # XXX this will fail in nasty ways

    #values
    if type_ == 'protc:parameter*':
        value = protc_parameter(anno)
    else:
        value = anno.exact

    # next level
    nodes = []
    for line in splitLines(anno.text):
        if line:
            id_ = idFromShareLink(line)
            if id_ is not None:
                subtree = buildAst(getAnnoById(id_, annos), annos)  # somehwere we try to get [0] and it fails if anno is none... ctrl a n
                nodes.append(subtree)
    return AstNode(type_, value, anno.id, nodes)

def main():
    from protcur import start_loop
    from time import sleep

    annos, stream_loop = start_loop()
    stream_loop.start()

    i = identifiers(annos)
    print(i)

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
    trees = [buildAst(_, annos) for _ in annos if 'protc:input' in _.tags]
    print(trees)

    embed()
    # HOW DO I KILL THE STREAM LOOP!??!

if __name__ == '__main__':
    main()
