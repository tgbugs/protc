#!/usr/bin/env python3
"""Run protcur analaysis
Usage:
    protcur-analysis [options]

Options:
    -s --sync   sync
"""

import re
import ast
import json
import inspect
from types import MappingProxyType
from pathlib import PurePath, Path
from datetime import datetime
from itertools import chain
import rdflib
from pyontutils.core import makePrefixes, OntId, OntGraph
from pyontutils.utils import async_getter, noneMembers, allMembers, anyMembers
from htmlfn import atag
from pyontutils.hierarchies import creatTree
from pyontutils.scigraph_client import Vocabulary
from pyontutils.namespaces import rdf, rdfs, owl, OntCuries
from pysercomb.parsers import racket, units
from pysercomb.parsing import _raw_dash_things
#from pysercomb import parsing_parsec
from hyputils.hypothesis import HypothesisAnnotation, HypothesisHelper, idFromShareLink
from protcur.core import linewrap, color_pda, log, logd
from protcur.config import __units_folder__ as units_folder

try:
    breakpoint
except NameError:
    from IPython import embed as breakpoint

sgv = Vocabulary(cache=True)
RFU = 'protc:references-for-use'
parameter_expression, *_ = units.make_unit_parser(units_folder)
error_output = []

# utility

_known_extensions = {}
# FIXME very much a hack on a bad id scheme
# and assumes that get_ is always called first
def get_hypothesis_local(uri):
    if 'hypothesis-local' in uri:
        pp = PurePath(uri)
        _known_extensions[pp.stem] = pp.suffix
        return pp.stem


HLPREFIX = '://hypothesis-local.olympiangods.org/'
def hypothesis_local(hln, s=True):
    # FIXME log.warning on hln not in _known_extensions
    return ('https' if s else 'http') + HLPREFIX + hln + _known_extensions.get(hln, '.pdf')


def extract_links_from_markdown(text):
    def doline(line):
        if line:
            if '](' in line:  # )
                return line.split('](', 1)[-1][:-1] # )
            elif 'http' in line:
                return line
            else:
                error_output.append(f'Bad line:{line}')

    if ' ' in text:
        text = text.replace(' ', '\n')
    if '\n' not in text:
        text += '\n'
    for line in text.split('\n'):
        url = doline(line)
        if url:
            yield url.rstrip().rstrip(')')

# stats

def getUri(anno):
    return anno._anno.uri if isinstance(anno, HypothesisHelper) else anno.uri


def citation_triples(annos, all=False):
    p = RFU
    for anno in annos:
        hl = get_hypothesis_local(getUri(anno))
        if hl:
            s = hl
            if p in anno.tags or all and any('protc:references-for-' in t for t in anno.tags):
                urls = extract_links_from_markdown(anno.text)
                for url in urls:
                    o = get_hypothesis_local(url)
                    o = o if o else url
                    yield s, p, o


def citation_tree(annos, html_head='', all=False):
    t = citation_triples(annos, all)
    PREFIXES = {'protc':'https://protc.olympiangods.org/curation/tags/',
                'hl':'https://hypothesis-local.olympiangods.org/'}
    PREFIXES.update(makePrefixes('rdfs'))
    OntCuries(PREFIXES)
    graph = OntGraph()
    OntCuries.populate(graph)
    for s, p, o in t:
        if 'http' in s:
            su = s
        else:
            su = hypothesis_local(s)
        if 'http' in o:
            ou = o
        else:
            ou = hypothesis_local(o)

        su = rdflib.URIRef(su)
        p = graph.namespace_manager.expand(p)
        ou = rdflib.URIRef(ou)
        s = rdflib.Literal(s)
        o = rdflib.Literal(o)

        [graph.add(t) for t in
         ((su, p, ou),
          (su, rdfs.label, s),
          (ou, rdfs.label, o))]

    ref_graph = graph.asOboGraph(RFU, restriction=False)
    if list(graph):
        tree, extra = creatTree('hl:ma2015.pdf', RFU, 'OUTGOING', 10, json=ref_graph, prefixes=PREFIXES, html_head=html_head)
        return tree, extra
    else:
        return None, None


def papers(annos):
    idents = {}

    def hasTag(hl, tag):
        return tag in idents[hl]

    def add_tag_text(hl, anno, tag):
        if tag in anno.tags:
            idents[hl][tag] = anno.text.strip()
            return True

    for anno in annos:
        hl = get_hypothesis_local(getUri(anno))
        if hl:
            if hl not in idents:
                idents[hl] = {}
            #print(hl)
            #print(anno.exact)
            #print(anno.tags)
            #print(anno.text)
            #print(anno.user)
            #print('---------------------')

            if add_tag_text(hl, anno, 'DOI:') and hasTag(hl, 'ISBN:'):
                idents[hl].pop('ISBN:')
            add_tag_text(hl, anno, 'protc:parent-doi')
            add_tag_text(hl, anno, 'PMID:')
            if not hasTag(hl, 'DOI:'):
                add_tag_text(hl, anno, 'ISBN:')

    return idents


def statistics(annos):
    stats = {}
    for anno in annos:
        hl = get_hypothesis_local(getUri(anno))
        if hl not in stats:
            stats[hl] = 0
        stats[hl] += 1

    return stats


def ast_statistics(ast):
    stats = {}
    for a in ast:
        hl = get_hypothesis_local(a.uri)
        if hl not in stats:
            stats[hl] = 0
        if a.isAstNode:
            stats[hl] += 1

    return stats


def splitLines(text):
    for line in text.split('\n'):
        yield line


def inputRefs(annos):
    for anno in annos:
        if anyMembers(anno.tags, 'protc:input', 'protc:*measure', 'protc:symbolic-measure'):
            for line in splitLines(anno.text):
                if line:
                    id_ = idFromShareLink(line)
                    if id_:
                        yield id_


class Hybrid(HypothesisHelper):
    """ Base class for building abstract syntax trees
        from hypothes.is annotations. """
    control_tags = tuple()  # tags controlling how tags from a reply affect the parent's tags
    prefix_skip_tags = tuple()  # pattern for tags that should not be exported to the ast
    ignore_tags = tuple()  # drop these tags if encountered
    prefix_ignore_unless_mapped = tuple()  # pattern for tags that should be ignored unless mapped
    text_tags = tuple()  # tags controlling how the text of the current affects the parent's text
    children_tags = tuple()  # tags controlling how links in the text of the parent annotation are affected
    CURATOR_NOTE_TAG = 'ilxtr:curatorNote'  # FIXME
    namespace = None  # the tag prefix for this ast, ONE PREFIX PER CLASS, use NamespaceTranslators for multiple
    tag_translators = {}
    additional_namespaces = {}
    _map_tags = MappingProxyType({})
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}
    _astParentIndex = {}

    def __new__(cls, anno, annos):
        """ namespace with the colon to make it simple to allow
            namespaces that are substrings of other namespaces """
        cls.prefix_ast = None
        if cls.namespace is not None:
            cls.prefix_ast = cls.namespace + ':'
            if hasattr(cls, 'skip_lu'):
                cls.nolu = set(OntId(c).u for c in cls.skip_lu)

        return HypothesisHelper.__new__(cls, anno, annos)

    def __init__(self, anno, annos):
        super().__init__(anno, annos)
        if len(self.objects) == len(self._annos):  # all loaded
            # populate annotation links from the text field to catch issues early
            log.debug(f'populating children {anno.id}')  # share link many not exist
            [c for p in self.objects.values() for c in p.children]
            [p._addAstParent() for p in self.objects.values() if tuple(p.children)]  # handles updates
            # TODO deletes still an issue as always

    @property
    def document(self):
        cls = self.__class__
        class Document:
            def __init__(self, inst):
                self.inst = inst
            @property
            def otherVersionUri(self):
                for a in cls.byIri(self.inst.uri):
                    if 'ilxtr:otherVersionUri' in a._anno.tags:
                        return a.text.strip()

        return Document(self)

    def ontLookup(self, *args, **kwargs):
        # XXX the ontology lookup produces very bad results
        # so cut it out for now
        # this also avoids having network dependency at this phase
        return None, None

    def _ontLookup(self, value, rank=('NCBITaxon', 'CHEBI', 'GO', 'UBERON', 'ilxtr', 'PATO')):

        # TODO OntTerm
        # extend input to include black_box_component, aspect, etc
        # WARNING sometime socket can hang in sgv here !?
        if self.classn == 'protc' or self.predicate not in self.nolu:  # FIXME
            data = sgv.findByTerm(value, searchSynonyms=False, searchAbbreviations=False)  # TODO could try the annotate endpoint? FIXME _extremely_ slow so skipping
            if not data:
                data = sgv.findByTerm(value, searchSynonyms=True, searchAbbreviations=False)
            if len(value) > 1 and not data:  # skip raw numbers
                data = sgv.findByTerm(value, searchSynonyms=True, searchAbbreviations=True)
        else:
            data = None

        #data = list(OntTerm.query(value))  # can be quite slow if hitting interlex
        #data = None
        if not data:
            return None, None

        if rank:
            ld = len(data)
            lr = len(rank)
            maxr = ld if ld > lr else lr
            def byRank(json, max=maxr):
                if 'curie' in json:
                    curie = json['curie']
                    prefix, suffix = curie.split(':', 1)
                    try:
                        return rank.index(prefix)
                    except ValueError:
                        return max + 1
                else:
                    return max + 2

            data = sorted(data, key=byRank)

        subset = [d for d in data if value in d['labels']]
        if subset:
            data = subset[0]
        else:
            data = data[0]  # TODO could check other rules I have used in the past

        if 'curie' not in data:  # avoid unknown id sources
            return None, None

        id = data['curie']  # if 'curie' in data else data['iri']
        if id.split(':')[0] not in rank:  # avoid unknown curie prefixes i.e. likely garbage
            return None, None

        if data['labels']:
            label = data['labels'][0]
        else:
            label = f'WARNING NO LABEL FOR {id} FIXME'

        return id, label

    def _fix_implied_input(self):
        if ': ' in self.text and 'hyp.is' in self.text:
            value_children_text = self.text.split(':', 1)[1]
            value, children_text = value_children_text.split('\n', 1)
            return value.strip(), children_text.strip()
        else:
            return '', ''

    @property
    def additional_tags(self):
        if not hasattr(self, '_additional_tags'):
            self._additional_tags = set(t for an in self.additional_namespaces.values()
                                        for t in an(self).supported_tags)

        return self._additional_tags


    @property
    def isAstNode(self):
        additional_tags = self.additional_tags
        return (self.prefix_ast is not None
                and noneMembers(self._tags, *self.control_tags)
                and all(noneMembers(tag, *self.prefix_skip_tags) for tag in self.tags)
                and (any(tag.startswith(self.prefix_ast) for tag in self.tags)
                     or any(tag in additional_tags for tag in self.tags)))

    @property
    def ast_updated(self):
        # FIXME hackish on the sort and -1
        return sorted([self.updated] +
                      [c.updated for c in self.children] +
                      [r.updated for r in self.replies])[-1]

    @property
    def exact(self):
        # FIXME last one wins for multiple corrections? vs first?
        for reply in self.replies:
            correction = reply.text_correction('exact')
            if correction:  # None and ''
                return correction
        return self._exact

    @property
    def text(self):
        for reply in self.replies:
            correction = reply.text_correction('text')
            if correction is not None:
                return correction
            correction = reply.text_correction('annotation-correction')
            if correction is not None:
                return correction

        if self._text.startswith('SKIP'):
            return ''
        elif self.CURATOR_NOTE_TAG in self._text:
            return self._text.split(self.CURATOR_NOTE_TAG, 1)[0]  # NOTE curation notes come last

        return self._text

    @property
    def curatorNotes(self):
        # TODO lift notes from replies as well if they are not ast nodes
        if self.CURATOR_NOTE_TAG in self._text:
            yield from (n.strip()
                        for n in
                        self._text.split(self.CURATOR_NOTE_TAG)[1:])
        if self._text.startswith('SKIP'):  # FIXME legacy
            yield self._text[len('SKIP '):]

    @property
    def feedbackNotes(self):
        "TODO"
        for reply in self.replies:
            if 'PROTCUR:feedback' in reply.tags:
                yield reply.text  # FIXME only goes down 1 level ... ? I think we only want 1 level so it's ok?

    @property
    def value(self):
        value = self._value()
        # normalization
        remove = '\u201c', '\u201d', '\u2019'
        for r in remove:
            value = value.replace(r, '')

        value = (value
                 # HYPHEN MY OLD ENEMEY I SEE WE MEET AGAIN
                 .replace('\u2010', '-')
                 # for a terrifying time check out unicode COMBINING
                 # and wonder how many things mishandle the case where
                 # a string starts with one of those
                 .replace(' \u0334', '~'))

        return value

    def _value(self):
        for reply in self.replies:
            correction = reply.text_correction('value')
            if correction:
                return correction

        if anyMembers(
                self.tags,
                *('protc:implied-' + s
                  # FIXME hardcoded fix
                  for s in ('input', 'output', 'aspect', 'section', 'vary'))):
            value, children_text = self._fix_implied_input()
            if value:
                return value

        if (self.text and
            not self.text.startswith('**https://hyp.is') and  # markdown madness
            not self.text.startswith('https://hyp.is')):
            if 'RRID' not in self.text:
                return self.text

        if self.exact is not None:
            return self.exact
        elif self._type == 'reply':
            return ''
        elif self._type == 'pagenote':
            return ''
        else:
            raise ValueError(f'{self.shareLink} {self.id} has no text and no exact and is not a reply.')

    @property
    def tags(self):
        ignore_tags = list(self.ignore_tags)
        add_tags = []
        for reply in self.replies:
            corrections = reply.tag_corrections
            if corrections is not None:
                op = corrections[0].split(':',1)[1]
                if op in ('add', 'replace'):
                    add_tags.extend(corrections[1:])
                if op == 'replace':
                    ignore_tags.extend(self._cleaned__tags)  # FIXME recursion error
                elif op == 'delete':
                    ignore_tags.extend(corrections[1:])

        out = []
        for tag in self._tags:
            # note that tag being in map tags is not exclusive
            # use case is .e.g being able to run and old and new
            # tag at the same time during a tag renaming cycle
            if tag in self._map_tags:
                out.append(self._map_tags[tag])

            if tag not in ignore_tags:
                out.append(tag)

        # I hate python excpetions, things fail silentlty and
        # you think everything is ok, but no, some error has been caught
        # inadvertently, raising a builtin python error should be and uncatchable
        # python error so that these kinds of mistakes and never happen >_<
        tout = set(self._translate_tags(out + add_tags))
        return tout

    @property
    def _cleaned_tags(self):
        prefixes_to_skip = self.prefix_skip_tags + self.prefix_ignore_unless_mapped 
        ok_tags = self.additional_tags
        for tag in self.tags:
            if not any(tag.startswith(prefix) for prefix in prefixes_to_skip) or tag in ok_tags:
                yield tag

    @property
    def _cleaned__tags(self):
        prefixes_to_skip = self.prefix_skip_tags + self.prefix_ignore_unless_mapped 
        ok_tags = self.additional_tags
        for tag in self._tags:
            if not any(tag.startswith(prefix) for prefix in prefixes_to_skip) or tag in ok_tags:
                yield tag

    def _translate_tags(self, tags):
        if self.tag_translators:
            for tag in tags:
                if ':' in tag and not tag.startswith(self.prefix_ast):  # FIXME use case for OntId
                    prefix, suffix = tag.split(':', 1)
                    if prefix in self.tag_translators:
                        tt = self.tag_translators[prefix]
                        translated = tt(tag).translation
                        if translated:
                            yield translated
                            continue
                        #
                    #
                #
                yield tag
            #

        else:
            #return tags
            # so ... for reasons beyond my understanding calling return tags here
            # instead of yielding results in this function returning None, or maybe
            # actually raising stop iteration rather than returning the list
            # WHAT THE HOW THE
            yield from tags

    @property
    def tag_corrections(self):
        tagset = self._tags
        for ctag in self.control_tags:
            if ctag in self._tags:
                if ctag == 'annotation-correction':
                    ctag = 'annotation-tags:replace'
                return [ctag] + list(self._cleaned_tags)

    def text_correction(self, suffix):  # also handles additions
        if suffix == 'annotation-correction':
            ctag = suffix
        else:
            ctag = 'annotation-text:' + suffix

        if ctag in self.tags:
            return self.text
        #elif suffix == 'children' and self.text.startswith('https://hyp.is'):
            #return self.text

    def children_correction(self, suffix):
        ctag = 'annotation-children:' + suffix
        if ctag in self.tags:
            return self.text

    @property
    def _children_text(self):
        correction = ''
        for reply in self.replies:
            _correction = reply.text_correction('children')
            if _correction is not None:
                correction += '\n' + _correction
        if correction:
            return correction

        if 'protc:implied-input' in self.tags:  # FIXME hardcoded fix
            value, children_text = self._fix_implied_input()
            if children_text:
                return children_text

        if 'hyp.is' not in self.text:
            children_text = ''
        elif anyMembers(self.tags, *self.children_tags):  # FIXME this assumes all tags are :delete
            children_text = ''
        elif (any(tag.startswith('PROTCUR:')
                  for tag in self.tags) and
              noneMembers(self.tags, *self.text_tags)):
            # accidental inclusion of feedback that doesn't start with SKIP eg https://hyp.is/HLv_5G43EeemJDuFu3a5hA
            children_text = ''
        else:
            children_text = self.text
        return children_text

    @staticmethod
    def _get_children_ids(children_text):
        for line in splitLines(children_text):
            if line:
                id_ = idFromShareLink(line)
                if id_ is not None:
                    yield id_

    @property
    def _children_delete(self):  # FIXME not mutex with annotation-text:children... will always override
        delete = ''
        for reply in self.replies:  # FIXME assumes the ordering of replies is in chronological order... validate?
            _delete = reply.children_correction('delete')
            if _delete is not None:
                delete = _delete
        if delete:
            for id_ in self._get_children_ids(delete):
                yield id_

    @property
    def _children_ids(self):
        # running this masks a number of orphaned replies
        # because getObjectById is never called
        #if 'annotation-children:delete' in self._tags:
            #return

        skip = set(self._children_delete)
        if skip:
            for id_ in self._get_children_ids(self._children_text):
                if id_ not in skip:
                    yield id_
                #else:
                    #printD('deleted', id_)  # FIXME this seems like it is called too many times...
        else:
            yield from self._get_children_ids(self._children_text)

    @property
    def rchildren(self):
        for child in self.children:
            yield child
            yield from child.children

    @property
    def children(self):  # TODO various protc:implied- situations...
        #if anyMembers(self.tags, *('protc:implied-' + s for s in ('input', 'output', 'aspect'))):  # FIXME hardcoded fix
            #if self.parent is None:
                #breakpoint()
                #raise ValueError(f'protc:implied-* does not have a parrent? Did you mistag?')
        if 'protc:aspect' in self.tags:
            for reply in self.replies:
                if 'protc:implied-vary' in reply.tags:
                    reply.hasAstParent = True
                    yield reply
                    break
            else:
                yield from self.direct_children

        elif 'protc:implied-vary' in self.tags:
            # this is an example of how to inject a reply as a child
            yield from self.parent.direct_children

        elif 'protc:implied-aspect' in self.tags:# or 'protc:implied-context' in self.tags:
            # this is an example of how to inject a reply as a parent
            if self.parent is not None:
                yield self.parent
            else:
                logd.warning(f'protc:implied-aspect not used in a reply! {self.htmlLink}')

        else:
            yield from self.direct_children

    @property
    def direct_children(self):
        for id_ in self._children_ids:
            child = self.getObjectById(id_)
            if child is None:
                if 'annotation-children:delete' not in self._tags:
                    logd.warning(f'child of {self._repr} {id_} does not exist!')
                continue
            for reply in child.replies:  # because we cannot reference replies directly in the client >_<
                if 'protc:implied-aspect' in reply.tags:# or 'protc:implied-context' in reply.tags:
                    self.hasAstParent = True  # FIXME called every time :/
                    yield reply
                    child = None  # inject the implied aspect between the input and the parameter
                    break

            if child is not None: # sanity
                # FIXME FIXME FIXME XXX do not call this on in sparc cases
                # everything runs backwards there (oops) can fix it in the data
                child.hasAstParent = True  # FIXME called every time :/
                yield child  # buildAst will have a much eaiser time operating on these single depth childs

    def _addAstParent(self):
        if self.isAstNode:
            for child in self.children:
                if child.id not in self.__class__._astParentIndex:
                    self.__class__._astParentIndex[child.id] = set()
                self.__class__._astParentIndex[child.id].add(self)

    def _delAddParent(self):
        if self.isAstNode:
            for child in self.children:
                self.__class__._astParentIndex[child.id].remove(self)

    @property
    def astParents(self):
        # note that all children have to be run first so that the connections are present
        if self.isAstNode:
            # TODO handle cases where non-top forms have no parent
            # should probably warn
            if self.id in self.__class__._astParentIndex:
                return self.__class__._astParentIndex[self.id]

    @property
    def needsParent(self):
        return self.isAstNode and not self.hasAstParent and self.astType in self._needParent

    _repr_join = '\n'

    def __repr__(self, depth=0, nparens=0, cycle=tuple(), html=False, number='*', ind=4):
        #SPACE = '&nbsp;' if html else ' '
        SPACE = '\xA0' if html else ' '
        NL = '<br>\n' if html else '\n'
        if self in cycle:
            log.warning(f'CYCLE DETECTED {self.shareLink} {self._repr}')
            return f'{NL}{SPACE * ind * (depth + 1)}* {cycle[0].id} has a circular reference with this node {self.id}'
            return ''  # prevent loops
        else:
            cycle += self,
        start = '|' if depth else ''
        #t = SPACE * ind * depth + start
        t = ((SPACE * ind) + start) * depth

        # prefix test
        lct = list(self._cleaned_tags)

        children = sorted(self.children)
        #children = set(c for c in self.children if c != self.parent)
        # avoid accidental recursion with replies of depth 1 TODO WE NEED TO GO DEEPER
        #and not print(cycle.id, self.id, self.shareLink))
        #if self in children:
            #print(f'WARNING {self.shareLink} is its own child what is going on?!')
            #children.remove(self)
            #f'\n{SPACE * ind * (depth + 1)}* {c.id} has a circular reference with this node {self.id}'  # avoid recursion
        _replies = [r for r in self.replies if r not in children]
        lenchilds = len(children) + len(_replies)
        more = f' {lenchilds} ...' if lenchilds else ' ...'
        childs = ''.join(c.__repr__(depth + 1, nparens=nparens, cycle=cycle, html=html) for c in children)
                         #if not print(c.id))
        #if childs: childs += '\n'
        notes = '\n\n'.join(self.curatorNotes)
        prefixes = {f'{self.classn}:':True,
                    'parent:':self.parent,
                    'value:':self.value,
                    'AstN?:':True,
                    'exact:':self.exact,
                    'text:':self.text,
                    'tags:':self.tags,
                    'cleaned tags:':(self.references and lct and lct != self.tags),
                    'tag_corrs:':self.tag_corrections,
                    'children:':childs,
                    'replies:':_replies,
                    'notes:':notes,}
        spacing = max(len(p) for p, test in prefixes.items() if test) + 1
        align = (ind + len(start)) * depth + spacing  # TODO max(len(things)) instead of 14 hardcoded
        def align_prefix(prefix):
            return NL + t + prefix + SPACE * (spacing - len(prefix))
        #parent_id =  (f"{NL}{t}parent_id:{SPACE * (spacing - len('parent_id:'))}"
        def row(prefix, rest):
            # if thunking this works for deferring if it will be a miracle
            return (align_prefix(prefix) + rest()) if prefixes[prefix] else ''

        startn = NL if not isinstance(number, int) or number > 1 else ''

        details = '<details>' if html else ''
        _details = '</details>' if html else ''

        summary = '<summary>' if html else ''
        _summary = f'</summary>' if html else '\n'  # </summary> has an implicit <br> for reasons know only to w3c

        value_text = row('value:', lambda:linewrap(self.value, align,
                                                   space=SPACE, nl=NL,
                                                   depth=depth, ind=ind))

        parent_id = row('parent:',
                        lambda:f"{'' if html else self.parent.id + SPACE}{self.parent._repr}")


        startbar = f'{startn}{SPACE * (((ind + len(start)) * depth) - 1)}{number:-<10}{more:->10}'

        link = atag(self.shareLink, self.id, new_tab=True) if html else self.shareLink
        title_text = row(f'{self.classn}:', lambda:f"{link}{SPACE}{self._repr}")

        #ast_text = '' if html else row('AstN?:', lambda:str(self.isAstNode))[1:]

        exact_text = row('exact:', lambda:linewrap(self.exact, align, space=SPACE, depth=depth, ind=ind))
        #exact_text = exact_text[1:] if html else exact_text  # switching places with ast_text -> no \n

        text_text = row('text:', lambda:linewrap(self.text, align, space=SPACE, depth=depth, ind=ind))

        tag_text = row('tags:', lambda:str(self.tags))

        ct = row('cleaned tags:', lambda:str(lct))

        tagcor = row('tag_corrs:', lambda:str(self.tag_corrections))

        replies = ''.join(r.__repr__(depth + 1, cycle=cycle, html=html)
                          for r in _replies)
                          #if not print(cycle.id, self.id, self.shareLink))
        rep_ids = row('replies:', lambda:' '.join(r._repr for r in _replies))
        replies_text = row('replies:', lambda:replies) if self.reprReplies else rep_ids

        childs_text = row('children:', lambda:childs)

        notes_text = row('notes:', lambda:linewrap(notes, align, space=SPACE, depth=depth, ind=ind))

        endbar = f'\n{t:_<80}{NL}'

        def rm_n(*args):
            """ because </summary> is evil """
            out = ''.join(args)
            if html:
                return out[len(NL):]
            else:
                return out.strip('\n')

        return ''.join((details, summary,
                        startbar,
                        title_text,
                        parent_id,
                        value_text,
                        notes_text,
                        _summary,
                        #ast_text,  # not used in hybrid and not useful in protc anymore
                        rm_n(exact_text,
                             text_text,
                             tag_text,
                             ct,
                             tagcor,
                             replies_text,
                             childs_text,
                             _details),
                        endbar))


class AstGeneric(Hybrid):
    """ Base class that implements the core methods needed for parsing various namespaces """
    generic_tags = 'TODO',
    control_tags = 'annotation-correction', 'annotation-tags:replace', 'annotation-tags:add', 'annotation-tags:delete'
    prefix_skip_tags = 'PROTCUR:', 'annotation-'  # reminder that these zap the anno from the ast
    prefix_ignore_unless_mapped = 'sparc:',
    text_tags = ('annotation-text:exact',
                 'annotation-text:text',
                 'annotation-text:value',
                 'annotation-text:children',
                 'annotation-correction')
    children_tags = 'annotation-children:delete',
    lang_line = ''
    #indentDepth = 2
    #objects = {}
    _tagIndex = {}  # this needs to be here to prevent accidental leaks through to Hybrid (I think)?
    #_order = tuple()
    #_topLevel = tuple()
    linePreLen = 0

    def _dispatch(self):
        """START HERE
        This is the code that dispatches by tag to functions that match the name of
        the tag suffix."""

        type_ = self.astType
        if type_ is None:
            if isinstance(self, HypothesisHelper):
                # it would seem that this happens sporadically because
                # of the set ordering of tags
                print('===========================================================')
                breakpoint()
                raise TypeError(f'Cannot dispatch on NoneType!\n{super()!r}')
            else:
                raise TypeError('Cannot dispatch on NoneType!\n'
                                f'{HypothesisHelper.__repr__(self.target_instance)}')

        if ':' in type_:
            namespace, dispatch_on = type_.split(':', 1)

        else:  # generic
            return self.additional_namespaces[None](self).astValue

        if namespace != self.namespace:
            if namespace in self.additional_namespaces:
                # there is not a 1:1 mapping of controlled tags to annotation groups
                # HOWEVER sometimes we may want to _remap_ the semantics of that
                # namespace into the current handler
                return self.additional_namespaces[namespace](self).astValue
            else:
                breakpoint()
                raise TypeError(f'{self.classn} does not dispatch on types from '
                                f'another namespace ({namespace}) ({self.tags}).')
        dispatch_on = dispatch_on.replace('*', '').replace('-', '_')
        return getattr(self, dispatch_on, self._default_astValue)()

    @staticmethod
    def _value_escape(value):
        return json.dumps(value.strip())
        #return '"' + value.strip().replace('"', '\\"') + '"'

    def _default_astValue(self):
        return self._value_escape(self.value)

    @classmethod
    def parsed(cls, ids=tuple()):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o)
                               for o in cls.objects.values()
                               if o is not None and o.isAstNode
                               and (not ids or o.id in ids))))

    @classmethod
    def protcurLang(cls, ids=tuple()):
        """ A clean output for consumption by #lang protc/ur """
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o)
                               for o in cls.objects.values()
                               if o is not None and o.is_protcur_lang()
                               and (not ids or o.id in ids))))

    def is_protcur_lang(self):
        return self.isAstNode and not self.hasAstParent

    @classmethod
    def topLevel(cls, ids=tuple()):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and o.is_top_level()
                               and (not ids or o.id in ids))))

    def is_top_level(self):
        return (self.isAstNode and
                not self.hasAstParent and
                self.astType in self._topLevel)

    @classmethod
    def flatall(cls, ids=tuple()):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and o.isAstNode
                               and (not ids or o.id in ids))))

    @classmethod
    def parentneed(cls, ids=tuple()):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and o.needsParent
                               and (not ids or o.id in ids))))

    @property
    def astType(self):
        if self.isAstNode:
            tags = self.tags  # compute once since tags is a property
            for suffix in self._order:
                tag = self.prefix_ast + suffix
                if tag in tags:
                    if (suffix in ('input', 'implied-input') and
                        not self.hasAstParent and
                        list(self.children)):
                        self._was_input = True  # FIXME make this clearer
                        return 'protc:output'
                    else:
                        return tag
            if len(tags) == 1:
                return next(iter(tags))
            elif len(list(self._cleaned_tags)) == 1:
                return next(iter(self._cleaned_tags))
            elif (('TODO' in tags or 'PROTCUR:review' in tags) and
                  len(tags) == 2):  # FIXME remove hardcoding
                return next(t for t in tags
                            if t != 'TODO' and t != 'PROTCUR:review')
            elif 'protc:back-box' in tags:
                breakpoint()
            else:
                tl = ' '.join(f"'{t}" for t in sorted(tags))
                logd.warning(f'something weird is going on with (annotation-tags {tl}) '
                             f'and self._order {self._order}')

    @property
    def astValue(self):
        if self.isAstNode:
            return self._dispatch()

    def __gt__(self, other):
        #if type(self) == type(other):
        if not self.isAstNode:
            return False
        elif not other.isAstNode:
            return True
        else:
            type_ = self.astType
            type_ = type_ if type_ is not None else 'zzzzzzzzzzzzzzzzzzzzzzz'
            oat = other.astType
            oat = oat if oat is not None else 'zzzzzzzzzzzzzzzzzzzzzzz'
            try:
                #return self.astType + self.astValue >= other.astType + other.astValue
                return type_ + self.value >= oat + other.value
            except TypeError as e:
                breakpoint()
                raise e

    def __lt__(self, other):
        #if type(self) == type(other) and self.isAstNode and other.isAstNode:
        return not self.__gt__(other)
        #else:
            #return False

    _repr_join = ''

    def __repr__(self, depth=1, nparens=1, plast=True, top=True, cycle=tuple(), html=False, number='*'):
        debug = f'  ; {depth}'
        out = ''
        NL = '<br>\n' if html else '\n'
        SPACE = '\xA0' if html else ' '
        CLOSE = '</span>)</span>' if html else ')'
        if html:
            def OPEN(from_here=0, *args, d=depth):
                total = from_here + d
                if total > 9:
                    total = total % 9
                cls_ = f'paren{total}'
                return f'<span class={cls_!r}>(<span class="default">'
        else:
            def OPEN(asdf=None, *args, d=depth):
                return '('

        def color(string, o=OPEN, c=CLOSE, s=SPACE, count=0):
            return ''.join(color_pda(string, o, c, s, count=count))
            
        if self.astType is None:
            if self in cycle:
                cyc = ' '.join(c.id for c in cycle)
                log.warning(f'Circular link in {self._repr} cycle {cyc}')
                out = f"{OPEN()}circular-link no-type {OPEN(1)}cycle {cyc}{CLOSE}{CLOSE}" + CLOSE * nparens + debug
                return out
            else:
                if ('ilxtr:technique' not in self.tags and
                    'PROTCUR:feedback' not in self.tags):
                    log.warning(f'unhandled type for {self._repr} {self.tags}')

                close = CLOSE * (nparens - 1)
                out = super().__repr__(html=html, number=number, depth=depth, nparens=0)
                mnl = '\n' if depth == 1 else ''
                here_string_marker = '----'
                _use_hstr = (not self.astType and (self.parent and self.parent.astType
                                                   or not self.parent))
                _os = f'#<<{here_string_marker}' if _use_hstr else ''
                _cs = f'\n{here_string_marker}\n' if _use_hstr else ''
                return out if html else (mnl +
                                         _os +
                                         out.rstrip() +
                                         _cs +
                                         close)

        self.linePreLen = self.indentDepth * (depth - 1) + len('(') + len(str(self.astType)) +  len(' ')
        _av = self.astValue
        if isinstance(_av, ParameterValue) and html:
            _av.SPACE = SPACE
            _av.NL = NL

        value = (_av.replace(' ', SPACE).replace('\n', NL)
                 if html and not isinstance(_av, ParameterValue) else
                 _av) # astValue depends on linePreLen
        if html:
            value = color(value, count=1)
        self.linePreLen += self.indentDepth  # doing the children now we bump back up
        link = self.shareLink
        if html: link = atag(link, link, new_tab=True)

        csuf = f';{SPACE}{link}'
        if html: csuf = f'<span class="comment">{csuf}</span>'
        comment = f'{SPACE}{SPACE}' + csuf

        children = sorted(self.children)  # better to run the generator once up here
        if children:
            indent = SPACE * self.indentDepth * depth
            linestart = NL + indent
            nsibs = len(children)
            cs = []
            _cycles = []  # FIXME this is a hack solution, figure out why there is more than one
            for i, c in enumerate(children):
                new_plast = i + 1 == nsibs
                # if we are at the end of multiple children the child node needs to add one more paren
                if new_plast:
                    new_nparens = nparens + 1
                else:
                    new_nparens = 1  # new children start their own tree, nparens only tracks the last node
                try:
                    if self not in cycle:
                        s = c.__repr__(depth=depth + 1,
                                       nparens=new_nparens,
                                       plast=new_plast,
                                       top=False,
                                       cycle=cycle + (self,),
                                       html=html)
                    elif cycle not in _cycles:
                        _cycles.append(cycle)
                        #print('Circular link in', self.shareLink)
                        cyc = f'{SPACE}'.join(c.id for c in cycle)
                        log.warning(f'Circular link in {self._repr} cycle {cyc}')
                        s = ((f"{OPEN(1)}circular-link{SPACE}"
                              f"no-type{SPACE}{OPEN(2)}cycle{SPACE}"
                              f"{cyc}{CLOSE}{CLOSE}") + CLOSE * nparens + debug + f'  {i} lol')
                        #s = f"'(circular-link {cycle[0].id})" + ')' * nparens
                    else:
                        logd.warning(f'duplicate cycles in {self._repr}')
                        continue
                except TypeError as e:
                    # do not remove or bypass this error, it means that one of your
                    # dicts like _replies or objects has members of some other class
                    # and is actually probably being inherited from that class
                    # you should give this class its own dictionary for that
                    raise TypeError(f'{c} is not an {self.classn}') from e  # XXX
                linejoin = NL if c.astType is None else linestart
                cs.append(linejoin + s)
            childs = comment + ''.join(cs)
        else:
            childs = CLOSE * nparens + comment


        _notes = '\n'.join(self.curatorNotes)  # the \n will be replaced during rewrap
        _fnotes = '\n'.join(self.feedbackNotes)  # TODO PROTCUR:review
        def make_notes(_notes, head='NOTE'):
            # TODO get user per note as well
            #'{SPACE}by{SPACE}{self._anno.user}{NL}'  # FIXME lifting notes from replies => inaccurate
            if _notes:
                # notes = f'{NL}#;{NL}<<--note{NL}' + linewrap(_notes, start=0, end=120, sep='', space=SPACE, nl=NL, ind=0) + f'{NL}--note'
                #_pl = (' ' * self.linePreLen) if depth > 1 else ''
                _npre = NL if depth == 1 else ''
                _nindent = self.indentDepth * (depth - 1)
                _cur = f'{_npre};{SPACE}{head}' + NL + SPACE * _nindent
                notes = f'{_cur};{SPACE}' + linewrap(_notes, start=0, end=120,
                                                    sep=f';{SPACE}', space=SPACE, nl=NL,
                                                    #ind=0 if depth == 1 else self.linePreLen,
                                                    ind=_nindent,
                                                    depth=1) + (NL if depth > 1 else '') + SPACE * _nindent
                if html: notes = f'<span class="comment">{notes}</span>'
            else:
                notes = ''

            return notes
        notes = make_notes(_notes) + make_notes(_fnotes, 'FEEDBACK')

        start = f'{NL}{OPEN()}' if top else OPEN()  # ))
        #print('|'.join(''.join(str(_) for _ in range(1,10)) for i in range(12)))

        id_ = f"'{self.id}"
        if html: id_ = color(id_)
        _pk = '#:prov'
        if html: _pk = color(_pk)
        prov = f"{_pk}{SPACE}{OPEN(1)}hyp:{SPACE}{id_}{CLOSE}"

        if isinstance(value, ParameterValue):
            value.SPACE = SPACE

        if (isinstance(value, ParameterValue) and
            not value.success and not children and
            value.v[0] == 'param:parse-failure'):
            # invert the nodes in the graph so that the parse failure wraps
            # we can only do this when there are no children unfortunately
            car = value.v[0]
            value.linePreLen += (len(car) - len(self.astType))
            value.v = ((f"#:node-type{SPACE}'{self.astType}\n"
                        f'{SPACE * value.linePreLen}'
                        f"#:failed-input{SPACE}") +
                       json.dumps(value.v[1]).replace(' ', SPACE))
            value.rest = ''  # already in the failed input
            # FIXME TODO do we need to bump the other line prelens?
            # I don't think we do because this would be the end of the line (hohoho)
        else:
            car = self.astType
            prov = SPACE + prov

        return f'{notes}{start}{car}{SPACE}{value}{prov}{childs}'


class protc(AstGeneric):
    namespace = 'protc'
    translators = {}
    tag_translators = {}
    ignore_tags = 'protc:process', 'protc:repeat', 'PROTCUR:review', 'protc:telos'
    additional_namespaces = {}  # filled in by the child classes
    lang_line = '#lang protc/ur'
    indentDepth = 2
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}  # without this Hybrid replies will creep in
    _astParentIndex = {}
    _order = (  # ordered based on dependence and then by frequency of occurence for performance
              'structured-data-record',  # needs to come first since record contents also have a type (e.g. protc:parameter*)
              'parameter*',
              'input',
              'invariant',
              'references-for-use',
              'references-for-data',
              'references-for-evidence',
              'vary',  # TODO
              'context',  # TODO
              'aspect',
              'black-box-component',
              'has-part',  # FIXME these are not being incorporated properly when they have parents...
              '*measure',  # under represented
              'output',
              'objective*',
              'order',
              'repeat',  # XXX bad
              'implied-vary',  # TODO
              #'implied-context',  # TODO
              'implied-aspect',
              '*make*',  # FIXME output?? also yay higher order functions :/
              'symbolic-measure',  # DEPRECATED
              'calculate',
              'implied-input',
              'result',
              'output-spec',
              'structured-data-header',
              'telos',
              'executor-verb',
              'how',
              'no-how-error',
              'var',
            )
    _topLevel = tuple('protc:' + t for t in ('input',
                                             # FIXME pretty sure that inputs should have parents?
                                             # but they should also have their own sections where they are
                                             # specced independently
                                             'output',
                                             'implied-input',
                                             'implied-output',
                                             '*measure',
                                             'symbolic-measure',
                                             'black-black-component',
                                             'executor-verb',
                                             'references-for-use',
                                             'references-for-data',
                                             'references-for-evidence',
                                            ))
    _needParent = tuple('protc:' + t for t in ('aspect',
                                               'black-box-component',
                                               'parameter*',
                                               'invariant',
                                               'telos',
                                               #TODO we need more here...
                                               ))
    _manual_fix = {
        # FIXME param: vs protc: for fuzzy-quantity, the fuz is not handled
        # by the param parser atm so keeping it in protc for now ...
        'rt': ('protc:fuzzy-quantity', '"room temperature"', '"temperature"'),
        'roomtemperature':('protc:fuzzy-quantity', '"room temperature"', '"temperature"'),
        'room temperature':('protc:fuzzy-quantity', '"room temperature"', '"temperature"'),

        'ice-cold':('protc:fuzzy-quantity', '"ice cold"', '"temperature"'),

        'overnight':('protc:fuzzy-quantity', '"overnight"', '"duration"'),
        'over night':('protc:fuzzy-quantity', '"overnight"', '"duration"'),

        'water':('protc:fuzzy-quantity', '"water"', '"immersion-type"'),
        'oil':('protc:fuzzy-quantity', '"oil"', '"immersion-type"'),

        'several thousand':('protc:fuzzy-quantity', '"several thousand"', '"ammount"'),  # FIXME vs count
        'unk':('protc:fuzzy-quantity', '"unknown"', '"unknown"'),
    }
    _map_tags = {'sparc:Tool': 'protc:input',
                 'sparc:Reagent': 'protc:input',
                 'sparc:Sample': 'protc:input',
                 #'sparc:OrganismSubject': '',
                 'sparc:AnatomicalLocation': 'protc:black-box-component',

                 # FIXME any PROTCUR: tag isn't just skipped
                 # it triggers a complete skip of the annotation
                 'PROTCUR:review': 'TODO',
                 }

    def __new__(cls, anno, annos):
        if not hasattr(cls, 'pyru'):
            from pysercomb.pyr import units as pyru
            pyru.Hyp.bindImpl(None, HypothesisAnno=cls.byId)
            cls.pyru = pyru
            ParameterValue.pyru = pyru

        self = super().__new__(cls, anno, annos)
        return self

    @property
    def tags(self):
        return super().tags

    def objective(self):
        if self.value in (' room  temperature'):
            return "'room-temperature"
        else:
            return self._value_escape(self.value)

    def asPython(self):
        """ generate the protcur expression for this node
            and parse it node back to python """
        if self.isAstNode:
            return self.pyru.RacketParser(repr(self))
        else:
            pass  # TODO ?

    def parameter(self):
        success, v, rest = getattr(self, '_parameter', (None, None, None))  # memoization of the parsed form

        if self.value.strip().lower() in self._manual_fix:  # ICK
            v = self._manual_fix[self.value.strip().lower()]
            rest = ''
        elif v is None:
            value = self.value
            if value == '':  # breaks the parser :/
                return ''
            cleaned = value.strip()
            cleaned = value.replace('\xA0', ' ')
            for child in self.children:  # TODO
                if child.astType == 'protc:unit':
                    cleaned += child.value
            cleaned_orig = cleaned

            # ignore gargabe at the start
            success = False
            front = ''
            v_orig = None

            def contains_dash_thing(s):
                for dt in chain(_raw_dash_things, ('-to-',)):
                    if dt in s:
                        return dt

            while cleaned and not success:
                try:
                    _, v, rest = parameter_expression(cleaned)
                    if v_orig is None:
                        v_orig = v
                    if rest.strip():
                        _strv = str(v)
                        _dt = contains_dash_thing(cleaned[1:])
                        if _dt and '^' in _strv:
                            #log.info(cleaned)
                            cleaned = cleaned.replace(_dt, ' - ')
                            continue

                except TypeError as e:
                    log.critical(f'{cleaned!r} {self.htmlLink}')
                    raise e
                success = v[0] != 'param:parse-failure'
                if not success:
                    # jump one char down and try again
                    cleaned = cleaned[1:]

            if not success:
                rest = cleaned_orig

            if not success:
                v = v_orig

            self._parameter = success, v, rest
            test_params.append((value, (success, v, rest)))

        if v and False:
            v = self.pyru.SExpr.format_value(v, self.linePreLen)
            #v = format_value(v, self.linePreLen)#, LID=' ' * self.linePreLen)

        return ParameterValue(success, v, rest, linePreLen=self.linePreLen)  # TODO implement as part of processing the children?

    def invariant(self):
        return self.parameter()

    def result(self):
        return self.parameter()

    def input(self):  # TODO reinstate rank as an arg
        value = self.value.strip()
        ont = ''
        if '(ont' in value:
            before, ont_after = value.split('(ont', 1)
            ont, after = ont_after.rsplit(')', 1)  # FIXME bad parsing
            ont = ' #:ont (ont' + ont + ')'
            value = before + after
            #print(ont)

        def manual_corrections(v):
            if v == 'PB':
                v = 'phosphate buffer'
            elif v == 'PBS':
                v = 'phosphate buffered saline'
            elif v == 'Phosphate Buffered Saline Solution':
                v = 'phosphate buffered saline'
            elif v == 'APs':
                v = 'action potential'  # plural ...
            return v
        value = manual_corrections(value)

        id, label = self.ontLookup(value)

        if ('sparc:AnatomicalLocation' in self._tags and
            [t for t in self._tags if t.startswith('UBERON:')]):
            id = [t for t in self._tags if t.startswith('UBERON:')][0]

        if id:
            #value += f" ({id_}, {data['labels'][0]})"
            value = (f"(term {id} {json.dumps(label) if label else '#f'} "
                     f"#:original {json.dumps(value)}{ont})")
            #value = ("term", id_, data['labels'][0], "#:original", value)
            #raise ValueError(value)
            return value
        else:
            test_input.append(value)
            #print(value, ont)
            return self._value_escape(value) + ont

    def implied_input(self):
        return self.input()

    def output(self):
        return self.input()

    def aspect(self):
        return self.input()

    def implied_aspect(self):
        return self.input()

    def vary(self):
        # FIXME format value as racket identifier
        return self.input()

    def implied_vary(self):
        # FIXME format value as racket identifier
        if not self.value:
            # treat the enclosing aspect annotation value as if it were a vary annotation
            return self.parent.vary()
        else:
            return self.input()

    def black_box(self):
        return self.input()

    def black_box_component(self):
        return self.input()

    #def structured_data(self):

    def structured_data_header(self):
        return "'(\"" + '" "'.join(self.value.split('\n')) + '")'

    def structured_data_record(self):
        return "'(\"" + '" "'.join(self.value.split('\n')) + '")'

    def references_for_use(self):
        esc_comment = r'\;'
        quote = "'"

        to_join = []
        for i, link in enumerate(sorted(extract_links_from_markdown(self.value))):
            link = link.replace(';', esc_comment)
            qlink = quote + link
            record = ((' ' * self.linePreLen if i else '') + (qlink if HLPREFIX in link else "(TODO " + qlink + ")"))
            to_join.append(record)

        return '\n'.join(to_join)

        return '\n'.join(f'''{" " * self.linePreLen if i else ""}{"'" + link.replace(";", esc) if HLPREFIX in link else "(TODO '" + link.replace(";", esc) + ")"}'''
                         for i, link in enumerate(sorted(extract_links_from_markdown(self.value))))
    #def implied_input(self): return value
    #def structured_data(self): return self.value
    #def measure(self): return self.value
    #def symbolic_measure(self): return self.value

#
# utility

class ParameterValue:

    NL = '\n'
    SPACE = ' '

    def __init__(self, success, v, rest, linePreLen=1):
        self.success = success
        self.v = v
        self.rest = rest
        self.linePreLen = linePreLen

    def __len__(self):
        if not hasattr(self, '_value'):
            self._value = repr(self)

        return len(self._value)

    def __iter__(self):
        if not hasattr(self, '_value'):
            self._value = repr(self)

        yield from self._value

    def __getitem__(self, idx):
        if not hasattr(self, '_value'):
            self._value = repr(self)

        return self._value[idx]

    def __repr__(self):
        success, v, rest = self.success, self.v, self.rest
        indent = self.SPACE * self.linePreLen
        if isinstance(v, tuple):
            # ParameterValue.pyru set in protc.__new__
            v = self.pyru.SExpr.format_value(v, self.linePreLen, SPACE=self.SPACE)

        if rest:
            rest = json.dumps(rest)  # slower but at least correct :/
        else:
            rest = ''  # in the even that None shows up

        if not success:
            out = f'{v}{self.NL}{indent}{rest}'
        else:
            out = v + (f'{self.NL}{indent}(rest{self.SPACE}{rest})' if rest else '')

        return out


test_params = []
test_input = []

def test_annos(annos):
    annos.append(HypothesisAnnotation({'id':'deadbeef0',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       'text':'MAGIC',
                                       'target':[{'selector':[{'type':'TextQuoteSelector', 'prefix':'', 'exact':'MAGIC', 'suffix':''}]}],
                                       #'exact':'MAGIC',
                                       'text':(
                                           'https://hyp.is/deadbeef1\n'
                                           'https://hyp.is/deadbeef3\n'
                                           'https://hyp.is/deadbeef4\n'
                                           'https://hyp.is/deadbeef5\n'
                                              ),
                                       'tags':['protc:input']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef1',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       'text':'https://hyp.is/deadbeef2',
                                       'tags':['protc:aspect']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef1.5',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       'references':['deadbeef1'],
                                       'text':'cookies m8',
                                       'tags':['annotation-text:value']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef2',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       'text':'10x10x10m/kg',
                                       #'text':'+- 3.5 - 6 MR',  # this has error
                                       #'text':'~ 3.5 - 6 MR',  # this has error
                                       #'text':'3.5 - 6 MR',  # this does not... HRM
                                       'tags':['protc:parameter*']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef3',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       #'text':'10x10x10m/kg',
                                       'text':'10 +- 3.5 - 6 MR/kg',  # this has error
                                       #'text':'~ 3.5 - 6 MR',  # this has error
                                       #'text':'3.5 - 6 MR',  # this does not... HRM
                                       'tags':['protc:parameter*']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef4',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       #'text':'10x10x10m/kg',
                                       #'text':'+- 3.5 - 6 MR',  # this has error
                                       'text':'~ 3.5 - 6 MR/kg/s^2',  # this has error
                                       #'text':'3.5 - 6 MR',  # this does not... HRM
                                       'tags':['protc:parameter*']}))
    annos.append(HypothesisAnnotation({'id':'deadbeef5',
                                       'user':'tgbugs',
                                       'created':'NOW',
                                       'updated':'LOL',
                                       #'text':'10x10x10m/kg',
                                       #'text':'+- 3.5 - 6 MR',  # this has error
                                       #'text':'~ 3.5 - 6 MR/kg/s^2',  # this has error
                                       #'text':'3.5 - 6 MR',  # this does not... HRM
                                       'text':'4.7 +- 0.6 x 10^7 / mm^3',  # TODO without long newline this has a formatting error
                                       #'text':'0.6 x 10^7 / mm^3',
                                       'tags':['protc:parameter*']}))


def main():
    from time import sleep, time
    from pprint import pformat
    import requests
    from hyputils.hypothesis import group, group_to_memfile
    from protcur.analysis import protc, Hybrid, HypothesisHelper  # __main__ bug hack
    from protcur.core import annoSync

    try:
        from desc.prof import profile_me
    except ModuleNotFoundError:
        profile_me = lambda x:x

    from docopt import docopt
    args = docopt(__doc__)

    global annos  # this is now only used for making embed sane to use
    get_annos, annos, stream_thread, exit_loop = annoSync(group_to_memfile(group),
                                                          helpers=(HypothesisHelper, Hybrid, protc))

    problem_child = 'KDEZFGzEEeepDO8xVvxZmw'
    #test_annos(annos)
    tree, extra = citation_tree(annos)
    i = papers(annos)

    [HypothesisHelper(a, annos) for a in annos]
    [Hybrid(a, annos) for a in annos]
    #printD('protcs')
    #@profile_me
    def rep():
        repr(hybrids)
    #rep()

    @profile_me
    def perftest():
        [protc(a, annos) for a in annos]
    start = time()
    perftest()
    pc = protc.byId(problem_child)
    stop = time()
    log.debug(f'BAD TIME {stop - start}')

    @profile_me
    def perfsort():
        sorted(protc)

    @profile_me
    def perflist():
        list(protc)

    @profile_me
    def perfhtml():
        [a.__repr__(html=True, number=n + 1) for n, a in enumerate(protc)]

    start = time()
    perflist()
    stop = time()
    log.debug(f'List BAD TIME {stop - start}')

    start = time()
    perfhtml()
    stop = time()
    log.debug(f'HTML BAD TIME {stop - start}')

    start = time()
    perfsort()
    stop = time()
    log.debug(f'Sort BAD TIME {stop - start}')

    #@profile_me  # a note that trying ot get pref data when there are lots of function calls nearly doubles actual time...
    def text():
        # NOTE if this is running slow, make sure you aren't using a remote SciGraph
        t = protc.parsed()
        with open('/tmp/protcur.rkt', 'wt') as f: f.write(t)
        # don't return to avoid accidentally repring these fellows :/
    #p = protc.byId('nofnAgwtEeeIoHcLZfi9DQ')  # serialization error due to a cycle
    #print(repr(p))
    start = time()
    text()
    stop = time()
    log.debug(f'BAD TIME {stop - start}')
    def more():
        tl = protc.topLevel()
        with open('/tmp/top-protcur.rkt', 'wt') as f: f.write(tl)
        #pl = protc.parentless()  # parentless was removed awhile back ?
        #with open('/tmp/pl-protcur.rkt', 'wt') as f: f.write(pl)
        pn = protc.parentneed()
        with open('/tmp/pn-protcur.rkt', 'wt') as f: f.write(pn)
        lang_output = protc.protcurLang()
        with open('/tmp/protcur-lang.rkt', 'wt') as f: f.write(lang_output)
    more()
    def update():
        protc.objects = {}
        protc._tagIndex = {}
        perftest()
        text()
        more()
    if args['--sync']:
        stream_thread.start()  # need this to be here to catch deletes

    breakpoint()
    exit_loop()
    if args['--sync']:
        stream_thread.join()


def _more_main():
    input_text_args = [(basic_start(a).strip(),) for a in annos if 'protc:input' in a.tags or 'protc:output' in a.tags]
    async_getter(sgv.findByTerm, input_text_args)  # prime the cache FIXME issues with conflicting loops...

    irs = sorted(inputRefs(annos))

    trees = makeAst()
    writeTrees(trees)

    test_inputs = sorted(set(test_input))
    def check_inputs():
        with open(Path('~/files/bioportal_api_keys').expanduser().as_posix(), 'rt') as f:
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

    breakpoint()
    # HOW DO I KILL THE STREAM LOOP!??! << answered, though quite a bit more complicated than expected

## end imports

#from protcur import namespace_mappings as nm

if __name__ == '__main__':
    main()
