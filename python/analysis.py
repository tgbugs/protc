#!/usr/bin/env python3.6

import os
import ast
from collections import Counter
from IPython import embed
from pyontutils.hierarchies import creatTree
from pyontutils.utils import makeGraph, makePrefixes
import parsing

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
        print('could not find', id_, shareLinkFromId(id_))
        return None

class AstNode:
    def __init__(self, type_, value, anno_id, children):
        self.type_ = type_
        self.value = value.strip()
        self.anno_id = anno_id
        self.children = children
    def __repr__(self, top=True, depth=1, nparens=1, plast=True):
        link = f'  ; {shareLinkFromId(self.anno_id)}'

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

        start = '\n(' if top else '('
        try:
            value = int(self.value)
        except ValueError:
            value = '"' + self.value + '"'
        return f'{start}{self.type_} {value}{childs}'

test_params = []
def protc_parameter(anno):
    if anno.text:
        value = anno.text
        error = ('text found for annotaiton in addition to exact\n'
                 f'exact: {anno.exact}\n'
                 f'text: {anno.text}')
        error_output.append(error)
    else:
        value = anno.exact

    #cleaned = value.strip(' ').strip('(')
    cleaned = value.replace('mL–1', '/mL').replace('kg–1','/kg')  # FIXME temporary (and bad) fix for superscript issues
    cleaned = cleaned.strip()
    cleaned_orig = cleaned

    # ignore gargabe at the start
    success = False
    front = ''
    while not success:
        success, v, rest = parsing.parameter_expression(cleaned)
        if not success:
            if len(cleaned) > 1:
                more_front, cleaned = cleaned[0], cleaned[1:]
                front += more_front
            else:
                front += cleaned
                success, v, rest = parsing.parameter_expression(cleaned_orig)  # reword but whatever
                error_output.append((success, v, rest))
                break

    #out = parse_mess(parts)
    #out = value
    out = str((success, v, rest))
    if front and front != rest:
        out = "'" + front + "', " + out

    test_params.append((value, (success, v, rest)))
    return out

def protc_invariant(anno):
    return protc_parameter(anno)

def buildAst(anno, annos):
    # check anno.tags for one of our known good tags
    # give that tag, switch to something for that tag
    # check anno.text for hyp.is
    # for line in splitLines(anno.text):
    #   buildAst(getAnnoById(idFromShareLink(line))
    
    #types
    if anno.tags:
        type_ = anno.tags[0]  # XXX this will fail in nasty ways
    else:
        print('Anno with no tag!', shareLinkFromId(anno.id))
        type_ = None  # just see where it goes...

    #values
    if type_ == 'protc:parameter*':
        value = protc_parameter(anno)
    elif type_ == 'protc:invariant':
        value = protc_invariant(anno)
    elif type_ == 'protc:input':
        value = anno.text if anno.type == 'reply' else anno.exact  # TODO
    else:
        value = anno.exact

    # next level
    children = []
    for line in splitLines(anno.text):
        if line:
            id_ = idFromShareLink(line)
            if id_ is not None:
                child = getAnnoById(id_, annos)
                if child is None: # sanity
                    print('Problem in', shareLinkFromId(anno.id))
                    continue
                subtree = buildAst(child, annos)  # somehwere we try to get [0] and it fails if anno is none... ctrl a n
                children.append(subtree)

    return AstNode(type_, value, anno.id, children)

def main():
    from protcur import start_loop
    from time import sleep

    annos, stream_loop = start_loop()
    stream_loop.start()

    i = identifiers(annos)
    #print(i)

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
    trees = []
    for _ in annos:
        if 'protc:input' in _.tags:
            te = buildAst(_, annos)
            trees.append(te)

    #print(trees)
    with open('/tmp/protcur.rkt', 'wt') as f:
        f.write(repr(trees))

    embed()
    # HOW DO I KILL THE STREAM LOOP!??!

if __name__ == '__main__':
    main()
