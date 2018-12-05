#!/usr/bin/env python3.6
"""Run protcur analaysis
Usage:
    analysis [options]

Options:
    -s --sync   sync
"""

import os
import re
import ast
import csv
import json
import inspect
from io import StringIO
from pathlib import PurePath, Path
from datetime import datetime
from itertools import chain
from collections import Counter, defaultdict
from urllib.parse import quote
import rdflib
import ontquery as oq
from pyontutils import combinators as cmb
from pyontutils.core import makeGraph, makePrefixes, OntId, simpleOnt
from pyontutils.utils import async_getter, noneMembers, allMembers, anyMembers
from pyontutils.utils import TermColors as tc, byCol
from pyontutils.config import devconfig
from pyontutils.sheets import get_sheet_values
from pyontutils.htmlfun import atag
from pyontutils.annotation import AnnotationMixin
from pyontutils.namespaces import ilxtr, TEMP, definition, editorNote, OntCuries
from pyontutils.hierarchies import creatTree
from pyontutils.scigraph_client import Vocabulary
from pyontutils.closed_namespaces import rdf, rdfs, owl
from pysercomb.parsers import racket, units
#from pysercomb import parsing_parsec
from hyputils.hypothesis import HypothesisAnnotation, HypothesisHelper, idFromShareLink, shareLinkFromId, iterclass
from protcur.core import linewrap
from desc.prof import profile_me
from IPython import embed

try:
    from misc.debug import TDB
    tdb=TDB()
    printD=tdb.printD
    #printFuncDict=tdb.printFuncDict
    #tdbOff=tdb.tdbOff
except ImportError:
    print('WARNING: you do not have tgbugs misc on this system')
    printD = print

sgv = Vocabulary(cache=True)
RFU = 'protc:references-for-use'
__script_folder__ = os.path.dirname(os.path.realpath(__file__))
parameter_expression, *_ = units.make_unit_parser(Path(__script_folder__,
                                                       '../../protc-lib/protc/units'))
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

def url_doi(doi):
    return 'https://doi.org/' + doi

def url_pmid(pmid):
    return 'https://www.ncbi.nlm.nih.gov/pubmed/' + pmid.split(':')[-1]

#
# docs

class TagDoc:
    _depflags = 'ilxtr:deprecatedTag', 'typo'
    def __init__(self, doc, parents, types=tuple(), **kwargs):
        self.types = types
        self.parents = parents if isinstance(parents, tuple) else (parents,)
        for k, v in kwargs.items():
            setattr(self, k, v)

        if anyMembers(self.parents, *self._depflags):
            self.doc = '**DEPRECATED** ' + doc
            self.deprecated = True
        else:
            self.doc = doc
            self.deprecated = False


def readTagDocs():
    with open(f'{__script_folder__}/../../protc-tags.rkt', 'rt') as f:
        text = f.read()
    with open(f'{__script_folder__}/../../anno-tags.rkt', 'rt') as f:
        text += f.read()
    success, docs, rest = racket.tag_docs(text)
    if rest:
        raise SyntaxError(f'tag docs did not parse everything!\n{rest}')
    tag_lookup = {tag:TagDoc(doc, parent) for _, tag, parent, doc in docs}
    return tag_lookup

tag_prefixes = 'ilxtr:', 'protc:', 'mo:', 'annotation-'
def justTags(tag_lookup=None):
    if tag_lookup is None:
        tag_lookup = readTagDocs()
    for tag, doc in sorted(tag_lookup.items()):
        if anyMembers(tag, *tag_prefixes) and not doc.deprecated:
            yield tag

def addDocLinks(base_url, doc):
    prefix = base_url + '/'
    return re.sub(r'`((?:protc|mo|sparc):[^\s]+)`', rf'[\1]({prefix}\1)', doc)

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
                    yield p, s, o

def citation_tree(annos, html_head='', all=False):
    t = citation_triples(annos, all)
    PREFIXES = {'protc':'https://protc.olympiangods.org/curation/tags/',
                'hl':'https://hypothesis-local.olympiangods.org/'}
    PREFIXES.update(makePrefixes('rdfs'))
    g = makeGraph('', prefixes=PREFIXES)
    for p, s, o in t:
        if 'http' in s:
            su = s
        else:
            su = hypothesis_local(s)
        if 'http' in o:
            ou = o
        else:
            ou = hypothesis_local(o)
        g.add_trip(su, p, ou)
        g.add_trip(su, 'rdfs:label', s)  # redundant
        g.add_trip(ou, 'rdfs:label', o)  # redundant
    ref_graph = g.make_scigraph_json(RFU, direct=True)
    tree, extra = creatTree('hl:ma2015.pdf', RFU, 'OUTGOING', 10, json=ref_graph, prefixes=PREFIXES, html_head=html_head)
    return tree, extra

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
    text_tags = tuple()  # tags controlling how the text of the current affects the parent's text
    children_tags = tuple()  # tags controlling how links in the text of the parent annotation are affected
    CURATOR_NOTE_TAG = 'ilxtr:curatorNote'  # FIXME
    namespace = None  # the tag prefix for this ast, ONE PREFIX PER CLASS, use NamespaceTranslators for multiple
    tag_translators = {}
    additional_namespaces = {}
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
            printD(f'populating children {anno.id}')  # share link many not exist
            [c for p in self.objects.values() for c in p.children]
            [p._addAstParent() for p in self.objects.values() if tuple(p.children)]  # handles updates
            # TODO deletes still an issue as always

    def ontLookup(self, value, rank=('NCBITaxon', 'CHEBI', 'GO', 'UBERON', 'ilxtr', 'PATO')):

        # TODO OntTerm
        # extend input to include black_box_component, aspect, etc
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
            def byRank(json, max=len(data)):
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

        id = data['curie'] if 'curie' in data else data['iri']
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
        elif self._text.startswith(self.CURATOR_NOTE_TAG):
            return self._text.split(self.CURATOR_NOTE_TAG, 1)[0]  # NOTE curation notes come last

        return self._text

    @property
    def curatorNotes(self):
        if self.CURATOR_NOTE_TAG in self._text:
            yield from (n.strip()
                        for n in
                        self._text.split(self.CURATOR_NOTE_TAG)[1:])
        if self._text.startswith('SKIP'):  # FIXME legacy
            yield self._text[len('SKIP '):]

    @property
    def value(self):
        for reply in self.replies:
            correction = reply.text_correction('value')
            if correction:
                return correction

        if anyMembers(self.tags, *('protc:implied-' + s for s in ('input', 'output', 'aspect', 'section'))):  # FIXME hardcoded fix
            value, children_text = self._fix_implied_input()
            if value:
                return value

        if self.text and not self.text.startswith('https://hyp.is'):
            if 'RRID' not in self.text:
                return self.text

        if self.exact is not None:
            return self.exact
        elif self._type == 'reply':
            return ''
        else:
            raise ValueError(f'{self.shareLink} {self.id} has no text and no exact and is not a reply.')

    @property
    def tags(self):
        skip_tags = []
        add_tags = []
        for reply in self.replies:
            corrections = reply.tag_corrections
            if corrections is not None:
                op = corrections[0].split(':',1)[1]
                if op in ('add', 'replace'):
                    add_tags.extend(corrections[1:])
                if op == 'replace':
                    skip_tags.extend(self._cleaned__tags)  # FIXME recursion error
                elif op == 'delete':
                    skip_tags.extend(corrections[1:])

        out = []
        for tag in self._tags:
            if tag not in skip_tags:
                out.append(tag)

        # I hate python excpetions, things fail silentlty and
        # you think everything is ok, but no, some error has been caught
        # inadvertently, raising a builtin python error should be and uncatchable
        # python error so that these kinds of mistakes and never happen >_<
        tout = set(self._translate_tags(out + add_tags))
        return tout

    @property
    def _cleaned_tags(self):
        for tag in self.tags:
            #print(self.prefix_skip_tags, self.tags)
            if not any(tag.startswith(prefix) for prefix in self.prefix_skip_tags):
                yield tag

    @property
    def _cleaned__tags(self):
        for tag in self._tags:
            if not any(tag.startswith(prefix) for prefix in self.prefix_skip_tags):
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
        elif any(tag.startswith('PROTCUR:') for tag in self.tags) and noneMembers(self.tags, *self.text_tags):
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
    def children(self):  # TODO various protc:implied- situations...
        #if anyMembers(self.tags, *('protc:implied-' + s for s in ('input', 'output', 'aspect'))):  # FIXME hardcoded fix
            #if self.parent is None:
                #embed()
                #raise ValueError(f'protc:implied-* does not have a parrent? Did you mistag?')
        if 'protc:implied-aspect' in self.tags:
            yield self.parent
            return
        for id_ in self._children_ids:
            child = self.getObjectById(id_)
            if child is None:
                if 'annotation-children:delete' not in self._tags:
                    print(f"WARNING: child of {self._repr} {id_} does not exist!")
                continue
            for reply in child.replies:  # because we cannot reference replies directly in the client >_<
                if 'protc:implied-aspect' in reply.tags:
                    self.hasAstParent = True  # FIXME called every time :/
                    yield reply
                    child = None  # inject the implied aspect between the input and the parameter
                    break

            if child is not None: # sanity
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
            print(tc.red('CYCLE DETECTED'), self.shareLink, self._repr)
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
                    'replies:':_replies,}
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
    text_tags = ('annotation-text:exact',
                 'annotation-text:text',
                 'annotation-text:value',
                 'annotation-text:children',
                 'annotation-correction')
    children_tags = 'annotation-children:delete',
    lang_line = ''
    #indentDepth = 2
    #objects = {}
    #_tagIndex = {}
    #_order = tuple()
    #_topLevel = tuple()
    linePreLen = 0

    @staticmethod
    def _value_escape(value):
        return json.dumps(value.strip())
        #return '"' + value.strip().replace('"', '\\"') + '"'

    def _default_astValue(self):
        return self._value_escape(self.value)

    def _dispatch(self):
        type_ = self.astType
        if type_ is None:
            if isinstance(self, HypothesisHelper):
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
                embed()
                raise TypeError(f'{self.classn} does not dispatch on types from '
                                f'another namespace ({namespace}) ({self.tags}).')
        dispatch_on = dispatch_on.replace('*', '').replace('-', '_')
        return getattr(self, dispatch_on, self._default_astValue)()

    @classmethod
    def parsed(cls):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o)
                               for o in cls.objects.values()
                               if o is not None and o.isAstNode)))

    @classmethod
    def protcurLang(cls):
        """ A clean output for consumption by #lang protc/ur """
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o)
                               for o in cls.objects.values()
                               if o is not None and o.isAstNode)))

    @classmethod
    def topLevel(cls):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and
                               o.isAstNode and
                               not o.hasAstParent and
                               o.astType in cls._topLevel)))

    @classmethod
    def parentless(cls):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and o.isAstNode and not o.hasAstParent)))

    @classmethod
    def parentneed(cls):
        return (cls.lang_line + '\n' +
                ''.join(sorted(repr(o) for o in cls.objects.values()
                               if o is not None and o.needsParent)))

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
            elif 'TODO' in tags and len(tags) == 2:  # FIXME remove hardcoding
                return next(t for t in tags if t != 'TODO')
            else:
                tl = ' '.join(f"'{t}" for t in sorted(tags))
                printD(f'Warning: something weird is going on with (annotation-tags {tl}) and self._order {self._order}')

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
                embed()
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
        if self.astType is None:
            if self in cycle:
                cyc = ' '.join(c.id for c in cycle)
                print('Circular link in', self._repr, 'cycle', cyc)
                out = f"'(circular-link no-type (cycle {cyc}))" + ')' * nparens + debug
                return out
            else:
                printD(tc.red('WARNING:'), f'unhandled type for {self._repr} {self.tags}')
                out = super().__repr__(html=html, number=number, depth=depth, nparens=0)
                close = ')' * (nparens - 1)
                mnl = '\n' if depth == 1 else ''
                here_string_marker = '----'
                return out if html else (mnl +
                                         f'#<<{here_string_marker}' +
                                         out.rstrip() +
                                         f'\n{here_string_marker}\n' +
                                         close)

        self.linePreLen = self.indentDepth * (depth - 1) + len('(') + len(str(self.astType)) +  len(' ')
        value = (self.astValue.replace(' ', SPACE).replace('\n', NL)
                 if html else
                 self.astValue) # astValue depends on linePreLen
        self.linePreLen += self.indentDepth  # doing the children now we bump back up
        link = self.shareLink
        if html: link = atag(link, link, new_tab=True)
        comment = f'{SPACE}{SPACE};{SPACE}{link}'

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
                        print('Circular link in', self._repr, 'cycle', cyc)
                        s = f"'(circular-link{SPACE}no-type{SPACE}(cycle{SPACE}{cyc}))" + ')' * nparens + debug + f'  {i} lol'
                        #s = f"'(circular-link {cycle[0].id})" + ')' * nparens
                    else:
                        printD(tc.red('WARNING:'), f'duplicate cycles in {self._repr}')
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
            childs = ')' * nparens + comment

        start = f'{NL}(' if top else '('  # ))
        #print('|'.join(''.join(str(_) for _ in range(1,10)) for i in range(12)))

        prov = f"(hyp:{SPACE}'{self.id})"

        return f'{start}{self.astType}{SPACE}{value}{SPACE}{prov}{childs}'


class protc(AstGeneric):
    namespace = 'protc'
    tag_translators = {}
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
              'aspect',
              'black-box-component',
              'has-part',  # FIXME these are not being incorporated properly when they have parents...
              '*measure',  # under represented
              'output',
              'objective*',
              'no-how-error',
              'order',
              'repeat',
              'implied-aspect',
              'how',
              '*make*',  # FIXME output?? also yay higher order functions :/
              'symbolic-measure',
              'implied-input',
              'result',
              'output-spec',
              'structured-data-header',
              'telos',
              'executor-verb',
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
                                            ))
    _needParent = tuple('protc:' + t for t in ('aspect',
                                               'black-box-component',
                                               'parameter*',
                                               'invariant',
                                               'telos',
                                               #TODO we need more here...
                                               ))
    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'

    format_nl_long =  '^'

    _manual_fix = {
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

    @property
    def tags(self):
        return super().tags | set(self.mapped_tags)

    @property
    def mapped_tags(self):
        if self._done_loading and SparcMI._done_loading:
            try:
                s = SparcMI.byId(self.id)
                if s.protc_unit_mapping:
                    yield 'protc:parameter*'
            except Warning:
                pass

    def objective(self):
        if self.value in (' room  temperature'):
            return "'room-temperature"
        else:
            return self._value_escape(self.value)

    def parameter(self):
        def isLongNL(tuple_):
            if tuple_[0] in self.format_nl_long:
                t1 = type(tuple_[1]) is tuple
                t2 = type(tuple_[2]) is tuple
                if t1 and t2:
                    if len(tuple_[1]) > 2 or len(tuple_[2]) > 2:
                        return True
                elif t1 and len(tuple_[1]) > 3:
                    return true
                elif t2 and len(tuple_[2]) > 3:
                    return true
            return False

        def format_value(tuple_, localIndent=0, depth=0):#, LID=''):
            out = ''
            if tuple_:
                newline = tuple_[0] in self.format_nl or isLongNL(tuple_)
                indent_for_this_loop = localIndent + len('(') + len(tuple_[0]) + len(' ')  # vim fail )
                indent_for_next_level = indent_for_this_loop
                #indent_this = LID + '(' + tuple_[0] + ' '  # vim fail )
                #indent_next = indent_this
                for i, v in enumerate(tuple_):
                    if newline and i > 1:
                        out += '\n' + ' ' * indent_for_this_loop
                        #out += '\n' + indent_this
                    if type(v) is tuple:
                        v = format_value(v, indent_for_next_level, depth + 1)#, LID=indent_next)
                    if v is not None:
                        v = str(v)
                        if out and out[-1] != ' ':
                            out += ' ' + v
                            if i > 1 or not newline:
                                indent_for_next_level += len(' ') + len(v) # unlike at the top v already has ( prepended if it exists
                                #indent_next += ' ' + v
                        else:  # we are adding indents
                            out += v
            if out:
                return '(' + out + ')'

        success, v, rest = getattr(self, '_parameter', (None, None, None))  # memoization of the parsed form

        if self.value.strip().lower() in self._manual_fix:  # ICK
            v = self._manual_fix[self.value.strip().lower()]
            rest = ''
        elif v is None:
            value = self.value
            if value == '':  # breaks the parser :/
                return ''
            cleaned = value.strip()
            cleaned_orig = cleaned

            # ignore gargabe at the start
            success = False
            front = ''
            while cleaned and not success:
                _, v, rest = parameter_expression(cleaned)
                success = v[0] != 'param:parse-failure'
                if not success:
                    cleaned = cleaned[1:]
            if not success:
                rest = cleaned_orig
            self._parameter = success, v, rest
            test_params.append((value, (success, v, rest)))

        if v:
            v = format_value(v, self.linePreLen)#, LID=' ' * self.linePreLen)
        return repr(ParameterValue(success, v, rest, indent=self.linePreLen))  # TODO implement as part of processing the children?

    def invariant(self):
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
                v = 'buffered phosphate saline'
            elif v == 'APs':
                v = 'action potential'  # plural ...
            return v
        value = manual_corrections(value)

        id, label = self.ontLookup(value)
        if id:
            #value += f" ({id_}, {data['labels'][0]})"
            value = f"(term {id} \"{label}\" #:original \"{value}\"{ont})"
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


class NamespaceAstValueTranslator:
    """ Base class for defining translators that operate on the astValue of
        some other namespace. These cannot be used 
    """


class RegNVT(type):
    def __init__(self, *args, **kwargs):
        self.target_type.additional_namespaces[self.namespace] = self
        super().__init__(*args, **kwargs)


class NamespaceValueTranslator:
    """ Base class used to translate naming convetions from one namespace
        into another at the raw value stage rather than the astValue stage.

        Subclasses of this class should never access the astValue of
        self.base since these classes are used to generate self.base.astValue
        and this will cause a recursion error.

        NamespaceHelper.target_type should be set to the class that implements
        the semantics that this namespace should translate into.

    """

    class BaseNamespaceMismatchError:
        pass

    target_type = None
    classn  = HypothesisHelper.classn
    _value_escape = AstGeneric._value_escape
    _dispatch = AstGeneric._dispatch
    astValue = AstGeneric.astValue
    namespace = None
    _order = tuple()
    isAstNode = True
    additional_namespaces = tuple()  # intentionally not a dict because dont want this

    def __init__(self, target_instance):
        if not hasattr(self, 'prefix_ast'):
            self.prefix_ast = None if self.namespace is None else self.namespace + ':'
        self.target_instance = target_instance
        if not isinstance(self.target_instance, self.target_type):
            raise self.TargetNamespaceMismatchError('{type(self.target_instance)} is not a {self.targt_type}')

    @property
    def supported_tags(self):
        for suffix in self._order:
            yield self.prefix_ast + suffix

    @property
    def value(self):
        return self.target_instance.value

    @property
    def tags(self):
        return set(t for t in self.target_instance.tags if t.startswith(self.prefix_ast))

    @property
    def astType(self):
        tags = self.tags
        for suffix in self._order:
            tag = self.prefix_ast + suffix
            if tag in tags:
                return tag

    def _default_astValue(self):
        #type_ = self.astType
        #predicate = type_ if type_ is not None else '/'.join(self.tags)
        return self.target_instance._default_astValue()
        #return f'({predicate} {self.target_instance._default_astValue()})'
        # we don't have to set the prefix here because
        # in order to get this far the parent has to know about this
        # tag, otherwise it will abort


class order_deco:  # FIXME make it so we dont' have to init every time
    """ define functions in order to get order! """
    def __init__(self):
        self.order = tuple()

    def mark(self, cls):
        if not hasattr(cls, '_order'):
            cls._order = self.order
        else:
            cls._order += self.order

        return cls

    def __call__(self, function):
        self.order += function.__name__,
        return function


od = order_deco()
@od.mark
class protc_generic(NamespaceValueTranslator, metaclass=RegNVT):
    """ tags without namespaces """
    target_type = protc
    namespace = None
    prefix_ast = ''  # hack to fool dispatch, these are NO namespace, not empty namespace
    _order = 'TODO',
    #target_type.additional_namespaces[namespace] = protc_generic  # FIXME

    def __init__(self, generic_instance):
        self.namespace = ''  # hack so we can reuse _dispatch
        super().__init__(generic_instance)

    @property
    def tags(self):
        return set(t for t in self.target_instance.tags if ':' not in t)

    @property
    def astType(self):
        for tag in self._order:
            if tag in self.target_instance.tags:
                return ':' + tag  # hack to fool _dispatch into thinking there is an empty namespace

    #def TODO(self):
        #return '(TODO {self.target_instace._default_astValue())})'
    

od = order_deco()
@od.mark
class protc_ilxtr(NamespaceValueTranslator, metaclass=RegNVT):
    target_type = protc
    namespace = 'ilxtr'
    def __init__(self, protc_instance):
        super().__init__(protc_instance)

    @od
    def technique(self):
        # techniques lacking sections are converted to impls
        # since they are likely to involve many steps and impl
        # is the correct place holder for things with just words
        return f'(protc:impl {json.dumps(self.value)})'

    @od
    def participant(self):
        # FIXME ambiguous
        return f'(protc:input {json.dumps(self.value)})'


class RegTT(type):
    def __init__(self, *args, **kwargs):
        for target_type in self.target_types:
            target_type.tag_translators[self.namespace] = self

        super().__init__(*args, **kwargs)


class TagTranslator:
    """ Use for pre ast translation of tags """

    _tag_lookup = readTagDocs()
    order_ = tuple(t for t in justTags(_tag_lookup) if t.startswith('mo:'))
    namespace = None
    target_namespace = None

    def __init__(self, tag):
        self.tag = tag

    @property
    def translation(self):
        if self.tag in self._tag_lookup:
            tagdoc = self._tag_lookup[self.tag]
            return next(t for t in tagdoc.parents if t.startswith(self.target_namespace + ':'))
        else:
            return self.tag


class mo_to_ilxtr(TagTranslator, metaclass=RegTT):
    target_types = protc,
    namespace = 'mo'
    target_namespace = 'ilxtr'


# sparc translation

from pyontutils.core import Source

class GraphOutputClass(iterclass):
    """ TODO accumulate triples as we go """

    def __init__(self, *args, **kwargs):
        self._n = -1
        super().__init__(*args, **kwargs)

    def __iter__(self):
        # FIXME this should be implemented somewhere else
        for obj in super().__iter__():
            if obj.astType:
                yield obj

    @property
    def n(self):
        self._n += 1
        return self._n

    @property
    def metadata(self):
        """ ontology metadata ala interlex """
        nowish = datetime.utcnow()  # request doesn't have this
        epoch = nowish.timestamp()
        iso = nowish.isoformat()
        ontid = TEMP['sparc/all-annotations']  # FIXME abstract this
        ver_ontid = rdflib.URIRef(ontid + f'/version/{epoch}/all-annotations')

        yield ontid, rdf.type, owl.Ontology
        yield ontid, owl.versionIRI, ver_ontid
        yield ontid, owl.versionInfo, rdflib.Literal(iso)
        yield ontid, rdfs.comment, rdflib.Literal('All annotations for SPARC metadata.')

    def ttl(self):
        return self.serialize().decode()

    def html(self):
        return self.serialize(format='htmlttl').decode()

    def _report(self, format='tsv'):
        # TODO actual format
        #obj = next(iter(self.objects.values()))
        graph = self.populate_graph()
        strio = StringIO(newline='\n')
        writer = csv.writer(strio, delimiter='\t')
        #writer.writerows()
        xc = set()
        xp = set()
        for s, p, o in sorted(graph):
            p = OntId(p)
            if p.prefix != self.namespace:
                continue
            else:
                p = p.curie
            if isinstance(s, rdflib.URIRef):
                s = OntId(s).curie
            else:
                s = ''
            if isinstance(o, rdflib.URIRef):
                o = OntId(o).curie

            xc.add(s)
            xp.add(p)
            row = [s, p, o]
            writer.writerow(row)
        
        for p in sorted(self.all_properties() - xp):
            writer.writerow(['', p, ''])
        for s in sorted(self.all_classes() - xc):
            writer.writerow([s, '', ''])

        return strio.getvalue()

    def report(self, format='tsv'):
        ptags = {t:len([p for p in v if p.isAstNode]) for t, v in self._tagIndex.items()}
        def pcount(tag):
            return ptags.get(tag, 0)

        tag_docs = self.makeTagDocs()
        skip = ('protc:', 'RRID:', 'NIFORG:', 'CHEBI:', 'SO:', 'PROCUR:', 'mo:', 'annotation-')
        atags = {t:0 for t in tag_docs}
        atags.update({t:len(v) for t, v in self._tagIndex.items()})
        _tags = [[t, d, pcount(t), ','.join(tag_docs[t].types) if t in tag_docs else '']
                 for t, d in atags.items()
                 if all(p not in t for p in skip)
        ]
        tags = sorted(_tags, key=lambda t:t[3])  # sort by type

        strio = StringIO(newline='\n')
        writer = csv.writer(strio, delimiter='\t')
        writer.writerow(['tag', 'annos', 'converted', 'type'])
        for row in tags:
            writer.writerow(row)

        return strio.getvalue()

    def populate_graph(self):
        self._n = -1
        graph = rdflib.Graph()
        for t in self.metadata:
            graph.add(t)

        if hasattr(self, 'class_extra_triples'):
            for t in self.class_extra_triples():
                graph.add(t)

        for obj in self.objects.values():
            if obj.isAstNode:
                for t in obj.triples:
                    graph.add(t)

        [graph.bind(p, n) for p, n in self.graph.namespaces()]

        if hasattr(self, 'queries'):
            for query in self.queries(graph):
                for t in query():
                    graph.add(t)

        return graph 

    def serialize(self, format='nifttl'):
        graph = self.populate_graph()
        # TODO add and remove triples on websocket update
        return graph.serialize(format=format)


def _make_sparc_domain_mapping():
    # property by domain
    OntCuries({'sparc':'http://uri.interlex.org/tgbugs/uris/readable/sparc/'})
    mapping = {
        'ephys':{
            OntId('sparc:ExperimentOnLiveOrganism'),
            OntId('sparc:BioelectronicNerveBlockingModulation'),
            OntId('sparc:ElectricalAcquisition'),
            OntId('sparc:ElectricalModulation'),
            OntId('sparc:ChemicalAcquisition'),
            OntId('sparc:MechanicalAcquisition'),
            OntId('sparc:IREnergyModulation'),
            OntId('sparc:Modulation'),
            OntId('sparc:ThermalEnergyModulation'),
        },
        'microscopy':{
            OntId('sparc:MicroscopyAcquisition'),
            OntId('sparc:TissuePreparationForMicroscopy'),
            OntId('sparc:EnvironmentForTissueDerivatives'),
            OntId('sparc:ExperimentOnTissueDerivatives'),
        },
        'radiology':{
            OntId('sparc:RadiologcalAcquistion'),
            OntId('sparc:RadiologicalImagingProtocol'),
            OntId('sparc:FunctionalMRIFeatures'),
            OntId('sparc:GeneralMRISequence'),
        },
        'transcriptomics':{
            OntId('sparc:RNASeqSpecimen'),
            OntId('sparc:TranscriptomicsAcquisition'),
            OntId('sparc:TranscriptomicsExperiment'),
            OntId('sparc:BulkRNASeqSpecimen'),
            OntId('sparc:SingleCellRNASeqSpecimen'),
        },
        'optical':{
            OntId('sparc:OpticalAcquisition'),  # FIXME vs Microscopy?
        },

        # feeders that are not required but might be relevant if one of these was used upstream
        'various':{
            OntId('sparc:EngineeredTissue'),
            OntId('sparc:Extraction'),
            OntId('sparc:ExperimentOnTissueDerivatives'),
            OntId('sparc:Specimen'),
            OntId('sparc:EnvironmentForLiveOrganism'),
        },
        'cell culture':{
            OntId('sparc:CellCulture'),
            OntId('sparc:StemCells'),
            OntId('sparc:StemCellExperiment'),
            OntId('sparc:CellCultureExperiment'),
        },
        'histology':{
            OntId('sparc:IDISCO'),
            OntId('sparc:Histochemistry'),
            OntId('sparc:TissuePreservation'),
            OntId('sparc:TissuePreparationSteps'),
            OntId('sparc:TissueSample'),
            OntId('sparc:Embedding'),
            OntId('sparc:CounterStaining'),
            OntId('sparc:FreezingVsChemicalFixation'),
            OntId('sparc:Immunohistochemistry'),
            OntId('sparc:MountVsSections'),
            OntId('sparc:TissueExperiment'),
            OntId('sparc:SectionThickness'),  # FIXME really a data property not a class?
            OntId('sparc:Staining'),
            OntId('sparc:EndogenousReporters'),
            OntId('sparc:Sectioning'),
            OntId('sparc:TissueMounting'),
            OntId('sparc:TissueClearing'),
            OntId('sparc:TissueClearance'),
            OntId('sparc:SlidesVsGrid'),
            OntId('sparc:EmbeddingMedia'),
            OntId('sparc:SectioningDevice'),
        },
        'general':{
            OntId('sparc:Environment'),
            OntId('sparc:RRIDs'),
            OntId('sparc:Analysis'),
            OntId('sparc:AnatomicalLocation'),
            OntId('sparc:OrganismSubject'),
            OntId('sparc:Experiment'),
            OntId('sparc:Acquisition'),
            OntId('sparc:Anesthesia'),
            OntId('sparc:ChemicalInSolution'),
            OntId('sparc:Procedure'),
            OntId('sparc:Protocol'),
            OntId('sparc:Measurement'),
            OntId('sparc:Resource'),
            OntId('sparc:Organization'),
            OntId('sparc:Researcher'),
            OntId('sparc:Sedation'),
            OntId('sparc:AnimalExperiment'),
            OntId('sparc:AnimalSubject'),
            OntId('sparc:HumanSubject'),
            OntId('sparc:ClinicalExperiment'),
        },
    }
    return {v:k for k, vs in mapping.items() for v in vs}


def _make_sparc_range_mapping():
    OntCuries({'xsd':str(rdflib.XSD)})
    mapping = {
        'ephys':{
            OntId('sparc:Modulation'),
        },
        'microscopy':{
            OntId('sparc:TissuePreparationForMicroscopy'),
            OntId('sparc:TissuePreparationSteps'),
            OntId('sparc:MicroscopyAcquisition'),
        },
        'radiology':{
            OntId('sparc:RadiologicalImagingProtocol'),
            OntId('sparc:GeneralMRISequence'),
            OntId('sparc:FunctionalMRIFeatures'),
        },
        'transcriptomics':{
        },
        'optical':{
        },
        'various':{
            OntId('sparc:Specimen'),
            OntId('sparc:TissueSample'),
            OntId('sparc:Extraction'),
        },
        'cell culture':{
            OntId('sparc:CellCulture'),
            OntId('sparc:StemCells'),
        },
        'general':{
            OntId('sparc:Analysis'),
            OntId('sparc:Anesthesia'),
            OntId('sparc:Procedure'),
            OntId('sparc:Protocol'),
            OntId('sparc:Sedation'),
            OntId('sparc:Acquisition'),
            OntId('sparc:Environment'),
            OntId('sparc:Organization'),
            OntId('sparc:Experiment'),
            OntId('sparc:Researcher'),
            OntId('sparc:Resource'),
            OntId('sparc:AnatomicalLocation'),
            OntId('sparc:ChemicalInSolution'),
            OntId('sparc:Measurement'),
        },
        None:{
            OntId('xsd:string'),
            OntId('owl:real')
        },
    }
    return {v:k for k, vs in mapping.items() for v in vs}


class SparcMI(AstGeneric, metaclass=GraphOutputClass):
    """ Class for transforming Hypothes.is annotations
        into sparc datamodel rdf"""
    namespace = 'sparc'
    prefix_skip_tags = 'PROTCUR:', 'annotation-'

    generic_tags = tuple()
    tag_translators = {}

    indentDepth = 2
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}  # without this Hybrid replies will creep in
    _astParentIndex = {}

    graph = None  # TODO set at
    domain_mapping = _make_sparc_domain_mapping()
    range_mapping = _make_sparc_range_mapping()
    skip_lu = ('sparc:isOfFileType',
               'sparc:isOfAge',
               'sparc:firstName',
               'sparc:rnaSeqSpecimenHasSampleNumber',
               'sparc:rnaSeqSpecimenHasTotalCellNumber',
               'sparc:softwareEnvironmentForAcquisition',
               'sparc:lastName')

    dfr = 'sparc-data-file-registry'
    _bids_id = -1
    # TODO trigger add to graph off websocket
    # to do this modify HypothesisHelper so that
    # classes can define a class method for
    # add, update, and delete that will fire additional actions

    def __init__(self, *args, **kwargs):
        self._subject = None
        self.extra_triples = tuple()
        super().__init__(*args, **kwargs)
        self.ttl = self._instance_ttl
        self.html = self._instance_html
        self.serialize = self._instance_serialize
        self.n = self._instance_n

    def _instance_n(self):  # FIXME not clear why we need this
        return self.__class__.n

    @classmethod
    def _new_inst_id(cls):  # FIXME (obvs)
        return TEMP[f'sparc/instances/{cls.n}']

    @classmethod
    def _new_bids_id(cls):
        cls._bids_id += 1
        return TEMP[f'sparc/bids/{cls._bids_id}']  # FIXME (obvs)

    @classmethod
    def class_extra_triples(cls):
        """ GraphOutputClass check this function for class level extra triples """
        for t in cls.registry():
            #printD(t)
            yield t
        return
        yield from cls.registry()

    @classmethod
    def registry(cls):
        uris = set(a.uri for a in SparcMI if 'rcont' not in a.uri)  # rcont remove temp iris w/ bad annos

        data, notes_index = get_sheet_values(cls.dfr, 'Data', False)
        ml = max(len(r) for r in data)
        normalized = [r + ([''] * (ml - len(r))) for r in data]
        prot, pnotes_index = get_sheet_values(cls.dfr, 'Protocols', False)
        pml = max(len(r) for r in prot)
        pnorm = [r + ([''] * (pml - len(r))) for r in prot]

        d = byCol(normalized)
        p = byCol(pnorm, to_index=('Protocol_Identifier',))
        bidsf = set(d.BIDs_file_name)

        cls._bids_id = -1
        for fn in sorted(bidsf):
            subject = cls._new_bids_id()
            yield subject, rdf.type, owl.NamedIndividual
            yield subject, rdf.type, ilxtr.BIDSFile
            yield subject, rdfs.label, rdflib.Literal(fn)
            for r in d:
                if r.BIDs_file_name == fn:
                    if r.Protocol and r.Protocol != 'None':
                        # FIXME hasProtocol not entirely correct
                        # data produced from process partially documented by ...
                        pr = p.searchIndex('Protocol_Identifier', r.Protocol)
                        piri = rdflib.URIRef(r.Protocol)
                        asl = rdflib.URIRef(pr.Annotation_Substrate_Link)
                        yield subject, ilxtr.hasProtocol, piri
                        yield piri, rdf.type, owl.NamedIndividual
                        yield piri, rdf.type, ilxtr.protocolArtifact
                        yield piri, ilxtr.hasAnnotationSubstrate, asl  # NOTE this is our link to uris
                    annosubstr = rdflib.URIRef(r.annotation_substrate) if r.annotation_substrate else None
                    fileiri = rdflib.URIRef(r.file_id) if r.file_id else annosubstr
                    yield subject, ilxtr.hasFile, fileiri # XXX NOTE this flattens everything
                    yield fileiri, rdf.type, owl.NamedIndividual
                    yield fileiri, rdf.type, ilxtr.FlatFile
                    yield fileiri, rdfs.label, rdflib.Literal(r.file_name)
                    yield fileiri, ilxtr.fileType, rdflib.Literal(r.file_type)
                    if annosubstr and annosubstr != fileiri:
                        yield fileiri, ilxtr.hasAnnotationSubstrate, annosubstr
    @staticmethod
    def format_data_query(object):
        query_prefix = 'https://neuinfo.org/data/search?q='
        if isinstance(object, rdflib.URIRef):
            return rdflib.URIRef(query_prefix + OntId(object).curie)  # FIXME TODO
        elif isinstance(object, rdflib.Literal):
            return rdflib.URIRef(query_prefix + quote(object))

    @classmethod
    def queries(cls, graph):
        """ queries that should be run to expand the graph """
        # TODO chaining could be achieved by having queries return functions
        # that are generators that are called consecutively here or
        # consecutively via the metaclass

        def dodataquery(p, o):
            return (p != rdf.type
                    and (not (isinstance(o, rdflib.Literal) and
                              (o.datatype == TEMP['protc:unit'] or
                               o.isdigit()))
                         or isinstance(o, rdflib.URIRef)))

        def protocols():
            # protocols
            q = graph.query('''
                select distinct ?file ?inst where {

                ?file rdf:type ilxtr:BIDSFile .
                ?file ilxtr:hasProtocol ?prot .

                ?prot ilxtr:hasAnnotationSubstrate ?substr .
                ?substr ilxtr:hasAnnotation ?anno .

                ?blank rdf:type owl:Axiom .
                ?blank owl:annotatedSource ?inst .
                ?blank ilxtr:literatureReference ?anno .
                }''')
            for result in q.bindings:
                file = result['file']
                inst = result['inst']

                linker = rdflib.BNode()
                yield file, ilxtr.metaLocal, linker
                #yield linker, rdf.type, ilxtr.fromProt  # FIXME not quite correct
                for p, o in graph[inst]:
                    yield linker, p, o
                    if dodataquery(p, o):
                        no = cls.format_data_query(o)
                        #yield linker, ilxtr.dataQuery, no
                        yield file, ilxtr.dataQuery, no
                        if isinstance(o, rdflib.URIRef):
                            yield no, rdfs.label, next(graph[o:rdfs.label])

                if isinstance(inst, rdflib.URIRef):
                    yield file, ilxtr.metaFromProtocol, inst  # FIXME naming
                    o = next(o for o in graph[inst:rdf.type]
                            if o != owl.NamedIndividual)
                    continue
                    #yield file, ilxtr.metaFromProtocolTypes, o
                #elif isinstance(inst, rdflib.BNode):
                    #for p, o in graph[inst]:
                        #yield file, ilxtr.rawTextTODO, o
                        #yield file, p, o

        def explogs():
            # experiment logs

            q = graph.query('''
                select distinct ?file ?inst where {

                ?file rdf:type ilxtr:BIDSFile .
                ?file ilxtr:hasFile ?flat .

                { ?flat ilxtr:hasAnnotation ?anno . }
                UNION
                { ?flat ilxtr:hasAnnotationSubstrate ?substr .
                  ?substr ilxtr:hasAnnotation ?anno . }

                ?blank rdf:type owl:Axiom .
                ?blank owl:annotatedSource ?inst .
                ?blank ilxtr:literatureReference ?anno .
            }''')

            done = set()
            for result in q.bindings:
                file = result['file']
                inst = result['inst']

                # because we are flattening we only need one occurance, even if
                # the there are distinct annotations that copies were sourced from
                # the full list of instances is retained under metaFromProv
                linker = rdflib.BNode()
                new = False
                for p, o in graph[inst]:
                    if o not in done or p == rdf.type:
                        yield linker, p, o
                        done.add(o)
                        new = True
                        if dodataquery(p, o):
                            no = cls.format_data_query(o)
                            #yield linker, ilxtr.dataQuery, no
                            yield file, ilxtr.dataQuery, no
                            if isinstance(o, rdflib.URIRef):
                                yield no, rdfs.label, next(graph[o:rdfs.label])

                if new:
                    yield file, ilxtr.metaLocal, linker
                    #yield linker, rdf.type, ilxtr.fromLogs
                    #yield linker, ilxtr.instIri, inst

                if isinstance(inst, rdflib.URIRef):
                    yield file, ilxtr.metaFromProv, inst  # FIXME naming
                    #continue
                #elif isinstance(inst, rdflib.BNode):
                    #for p, o in graph[inst]:
                        #yield file, ilxtr.rawTextTODO, o
                        #yield file, p, o

        return protocols, explogs

    @classmethod
    def all_domains(cls):
        return {p:set(cls.domain_mapping[OntId(d)]
                      for d in cls._domain(cls.graph, p))
                for p in cls.all_properties()}

    @classmethod
    def all_ranges(cls):
        out = {}
        for p in cls.all_properties():
            ranges = cls._range(cls.graph, p)
            for range in ranges:
                if range is not None:
                    out[p] = cls.range_mapping[OntId(range)]
                else:
                    out[p] = 'Parent Property'

        return dict(out)

    @classmethod
    def all_modalities(cls):
        out = {}
        for p in cls.all_properties():
            domains = set(cls._domain(cls.graph, p))
            ranges = set(cls._range(cls.graph, p))
            if not ranges:
                ranges.add(None)

            modality = cls._modality(p, domains, ranges)
            out[p] = modality

        for o in cls.all_classes():
            modality = cls._modality(o, {o}, {o})
            out[o] = modality

        return dict(out)


    @classmethod
    def domain_properties(cls):
        out = defaultdict(set)
        for predicate, domains in cls.all_domains().items():
            for domain in domains:
                out[domain].add(predicate)

        return dict(out)

    @classmethod
    def modality_tags(cls):
        out = defaultdict(set)
        for predicate, modality in cls.all_modalities().items():
            out[modality].add(predicate)

        return dict(out)

    @property
    def modality(self):
        return self._modality(self.astType, self.domain, self.range)

    @classmethod
    def _modality(cls, tag, domains, ranges):
        domain = sorted(domains)[0]  # FIXME all first by accident
        range = sorted(ranges)[0]  # FIXME better logic
        if domain is None:
            if OntId(tag) in cls.all_properties():
                print(f'WARNING: no domain for {cls.astType}')
            return None

        dmodality = cls.domain_mapping[OntId(domain)]

        if range is None:
            return dmodality

        try:
            rmodality = cls.range_mapping[OntId(range)]
        except KeyError:
            if tag != range:
                print('WARNING: mapping for range', tag, range)
            return dmodality

        if rmodality is None:
            return dmodality
        elif OntId(range) == OntId('sparc:Measurement'):
            return dmodality
        elif dmodality in ('general', 'various'):
            return rmodality
        elif rmodality in ('general', 'various'):
            return dmodality
        elif rmodality != dmodality:
            print(f'WARNING: Modality mismatch! {dmodality} {rmodality} {domain} {range} {tag}')
            return rmodality  # assume that the range gives more specifcity
        else:
            return dmodality

    @property
    def domain(self):
        d = set(self._domain(self.graph, self.astType))
        if not d:
            d.add(None)

        return d

    @staticmethod
    def _domain(graph, property):
        for object in graph[OntId(property).u:rdfs.domain]:
            yield from graph.transitive_subjects(rdfs.subClassOf, object)

    @property
    def range(self):
        r = set(self._range(self.graph, self.astType))
        print('aaaaaaaaaaaa', r)
        if not r:
            r.add(None)

        return r

    @staticmethod
    def _range(graph, property):
        # TODO byPredicate? I swear I did this already ...
        yield from graph[OntId(property).u:rdfs.range]

    @property
    def only_tag(self):
        """ just in case """
        try:
            return next(t for t in self.tags if self.prefix_ast in t)
        except StopIteration:
            return None

    @property
    def complex_tags(self):
        tags = set(t for t in self.tags if self.prefix_ast in t)
        if len(tags) == 2:
            return tags
        else:
            return None
            # NOTE as implemented at the moment if there is more than 2 sparc tags
            # then only the first will be selected
            # TODO warn on > 2 tags?
            return self.only_tag

    #@property
    #def isAstNode(self):
        #bool([t for t in self.tags if self.prefix_ast in t])

    @property
    def astType(self):
        return self.only_tag

    @property
    def isClass(self):
        """ the tag used is a class and implies the start of a named individual """
        classes = self.all_classes()
        complex = self.complex_tags
        if complex is not None:
            return any(t in classes for t in complex)
        return self.only_tag in classes

    @property
    def isProperty(self):
        properties = self.all_properties()
        complex = self.complex_tags
        if complex is not None:
            return any(t in properties for t in complex)
        return self.only_tag in properties

    @classmethod
    def all_properties(cls):
        if cls.graph:
            if not hasattr(cls, '_all_properties'):
                # FIXME OntId duplicates rdflib qname
                cls._all_properties = set(OntId(s).curie for s, o in cls.graph[:rdf.type:]
                                          if #not print(s, o) and
                                          isinstance(s, rdflib.URIRef) and
                                          OntId(s).prefix == cls.namespace and
                                          o in (owl.ObjectProperty,
                                                owl.DatatypeProperty,
                                                owl.AnnotationProperty))

            return cls._all_properties
        else:
            pass

    @classmethod
    def all_classes(cls):
        if cls._done_loading:
            if not hasattr(cls, '_all_classes'):
                cls._all_classes = set(OntId(s).curie for s, o in cls.graph[:rdf.type:]
                                       if isinstance(s, rdflib.URIRef) and
                                       OntId(s).prefix == cls.namespace and
                                       o == owl.Class)
            return cls._all_classes

    @classmethod
    def all_tags(cls):
        return cls.all_classes() | cls.all_properties()

    @classmethod
    def _graph(cls):
        local_version = Path(devconfig.ontology_local_repo, 'ttl/sparc-methods.ttl')  # FIXME hardcoded
        if local_version.exists():  # on the fly updates from local
            graph = rdflib.Graph().parse(local_version.as_posix(), format='turtle')
        else:
            graph = cls.graph
        return graph

    @classmethod
    def _docs(cls, graph=None, comments=True):
        if graph is None:
            graph = cls._graph()
        mods = cls.all_modalities()
        ad = defaultdict(set)
        for s, o in graph[:rdfs.domain:]:
            ad[OntId(s).curie].add(OntId(o).curie)
        ar = defaultdict(set)
        for s, o in graph[:rdfs.range:]:
            ar[OntId(s).curie].add(OntId(o).curie)

        for tag in sorted(cls.all_classes() | cls.all_properties()):
            uri = OntId(tag).u  # FIXME inefficient
            types = sorted(OntId(_type).curie for _type in graph[uri:rdf.type])
            subThingOf = rdfs.subPropertyOf if any('Property' in t for t in types) else rdfs.subClassOf
            parents = [OntId(p).curie for p in graph[uri:subThingOf]]
            edNote = '\n'.join([o for o in graph[uri:editorNote]])
            mod = mods[tag]  # if tag in mods else ''

            try:
                _def = ' ' + next(graph[uri:definition])
            except StopIteration:
                _def = ' No definition.'

            doc = f'**{" ".join(types) if types else ""}**{_def}'
            kwargs = {'editorNote':edNote if comments else '',
                      'domain':ad[tag] if tag in ad and ad[tag] else {''},
                      'range':ar[tag] if tag in ar and ar[tag] else {''},
                      'modality':mod,}
            yield types, tag, parents, doc, kwargs

    @classmethod
    def makeTagDocs(cls, comments=True):
        if cls._done_loading:
            graph = cls._graph()
            if (not hasattr(cls, '_tag_lookup') or
                not cls._tag_lookup or
                cls.graph != graph):
                cls._tag_lookup = {tag:TagDoc(doc, parents, types, **kwargs)
                                   for types, tag, parents, doc, kwargs in
                                   cls._docs(graph, comments=comments)}

            return cls._tag_lookup
        else:
            raise BaseException('why are you erroring here?')

    @property
    def protc_unit_mapping(self):
        return self.astType in self.um()

    @classmethod
    def um(cls):  # FIXME this needs to be class level
        substrings = (
            'Weight',
            'temperature',
            'Age',
            'concentration',
            'Dose',
            'Frequency',
            'Rate',
            'CStim',   # FIXME may not work in long run
        )
        if cls.graph is not None:
            if not hasattr(cls, '_um'):
                cls._um = set(t for t in cls.all_properties() if anyMembers(t, *substrings))

            return cls._um
        else:
            return tuple()

    @property
    def value(self):
        v = super().value
        # FIXME TODO
        if 'hyp.is' in v:
            a, b = v.split('https://hyp.is', 1)  # hyp.is comes second
            v = a
        return v 

    @property
    def subject(self):
        # TODO just get the subject from the 'children'
        # ie just paste the hypothesis link to the subject in!
        # we know what parent object to attach this to
        isClass = self.isClass  # has to be called before _subject
        domain = self.domain
        if self._subject is None:
            if isClass:
                self._subject = self._new_inst_id()
                t = self._subject, rdf.type, OntId(self.astType).u
                self.extra_triples += (t,)
                return self._subject
            for child in self.children:
                if child == self:  # cases where we want to anchor a named individual and a predicate
                    return self._subject
                elif not domain or domain and child.type_object in domain:
                    if child.isClass:
                        self._subject = child.subject
                        return self._subject
                else:
                    printD([OntId(d).curie for d in domain],
                           child.type_object,
                           list(self.tags),
                           self.value,
                           self._repr)
                    #embed()

            self._subject = rdflib.BNode()

        return self._subject

    @property
    def predicate(self):
        # FIXME yield to allow multiple branches
        if self.isClass and self.isProperty:
            complex = self.complex_tags
            self.extra_triples += ((self.subject, rdf.type, owl.NamedIndividual),
                                   (self.subject, rdf.type, self.type_object))
            p = next(t for t in self.tags if t in self.all_properties())
            return OntId(p).u
        elif self.isClass:
            return rdf.type
        elif self.isProperty:
            p = next(t for t in self.tags if t in self.all_properties())  # FIXME for now are going with 1 tag
            return OntId(p).u
        else:
            return ilxtr.WHAT

    @property
    def type_object(self):
        if self.isClass:
            if self.isProperty:
                c = next(t for t in self.tags if t in self.all_classes())
                return OntId(c).u

            return OntId(self.astType).u

    @property
    def object(self):
        if self.isClass and not self.isProperty:
            return owl.NamedIndividual
        else:
            if self.astType in self.um():
                v = protc.byId(self.id).astValue
                if '(rest' in v:
                    v, junk = v.rsplit('(rest', 1)
                    v = v.strip()
                return rdflib.Literal(v, datatype=TEMP['protc:unit'])
            id, label = self.ontLookup(self.value)
            if id is None:
                return rdflib.Literal(self.value)
            else:
                oid = OntId(id)
                if oid.prefix:
                    if self.graph.namespace_manager.store.prefix(oid.prefix) is None:
                        self.graph.bind(oid.prefix, oid.namespace)
                o = oid.u
                if label:
                    #et = self.extra_triples
                    t = o, rdfs.label, rdflib.Literal(label)
                    self.extra_triples += (t,) #(_ for _ in chain(et, (t,)))
                    #self.extra_triples = (_ for _ in chain(et, (t,)))
                return o

    @property
    def triples(self):
        t = self.subject, self.predicate, self.object
        sl = rdflib.URIRef(self.shareLink)
        po = ilxtr.literatureReference, sl
        av = (((ilxtr.annotationValue, rdflib.Literal(self.value)),)
              if self.value != self.object else tuple())
        notes = [(OntId(self.CURATOR_NOTE_TAG), rdflib.Literal(n)) for n in self.curatorNotes]
        yield t
        yield from self.extra_triples
        yield from cmb.annotation(t, po, *av, *notes)()
        yield rdflib.URIRef(self.uri), ilxtr.hasAnnotation, sl  # FIXME normalize self.uri
        # TODO any additional stuff

    def _instance_ttl(self):
        """ instance ttl """
        return self.serialize()

    def _instance_html(self):
        return self.serialize(format='htmlttl')

    def _instance_serialize(self, format='nifttl'):
        """ instance serialize """
        graph = rdflib.Graph()
        for t in self.triples:
            graph.add(t)

        if self.isClass:
            for p, o in self.graph[self.subject::]:
                if p != rdf.type:
                    t = self.subject, p, o
                    graph.add(t)
            # TODO proper transitive closure
            #for t in self.graph.transitive_objects(self.subject, None):
                #graph.add(t)

        # FIXME slooow
        [graph.bind(p, n) for p, n in self.graph.namespaces()]  # FIXME not quite right?
        # TODO add and remove triples on websocket update
        return graph.serialize(format=format)

    _repr_join = '\n\n'

    def __repr__(self, html=False, number=''):
        """ turtle repr of class leaving prefixes implicit """
        # when its empty all you get is the anno > nice
        if html:
            text = self.html()
            sep = b'<br>\n<br>\n'
        else:
            text = self.ttl()
            sep = b'\n\n'

        return sep.join([s for s in text.split(sep) if s.endswith(b'.')][1:]).decode()


def oqsetup():
    import ontquery as oq
    from pyontutils.namespaces import PREFIXES
    paths = ('ttl/sparc-methods.ttl',  # by convention the first path is expected to define all the tags
             'ttl/methods-helper.ttl',
             'ttl/methods-core.ttl',
             'ttl/methods.ttl')
    ghq = oq.plugin.get('GitHub')('SciCrunch', 'NIF-Ontology',
                                  *paths, branch='sparc')
    SparcMI.graph = ghq.graph
    pns = (
        # FIXME decl in class
        ('hyp', 'https://hyp.is/'),
        ('hlf', 'https://hypothesis-local.olympiangods.org/'),  # hlfull since extension is preserved
        ('prots-sparc', 'http://uri.interlex.org/tgbugs/uris/protocols/sparc/'),
        ('inst', 'http://uri.interlex.org/temp/uris/sparc/instances/'),
        ('bidsf', 'http://uri.interlex.org/temp/uris/sparc/bids/'),
        ('bf-sun', 'https://app.blackfynn.io/N:organization:4827d4ca-6f51-4a4e-b9c5-80c7bf8e5730/datasets/'),
        ('bf-mvp', 'https://app.blackfynn.io/N:organization:89dfbdad-a451-4941-ad97-4b8479ed3de4/datasets/'))
    [SparcMI.graph.bind(p, n) for p, n in pns]
    query = oq.OntQuery(ghq)
    oq.OntCuries(ghq.curies)
    oq.OntCuries(PREFIXES)
    oq.OntTerm.query = query
    oq.query.QueryResult._OntTerm = oq.OntTerm
    OntTerm = oq.OntTerm
    return OntTerm, ghq


class protcur_to_technique:
    """ protocolExecution, techniqueExecution, as well as high level techinques """
    def __init__(self):
        pass


class technique_to_sparc(AnnotationMixin):
    # we have 3 options for how to do this, neurons style, methods style, or parcellation style
    def __init__(self):
        olr = Path(devconfig.ontology_local_repo)
        g = (rdflib.Graph()
            .parse((olr / 'ttl/sparc-methods.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods-core.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods-helper.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods.ttl').as_posix(),
                    format='turtle'))

        rq = oq.plugin.get('rdflib')(g)
        self.rq = rq
        query = oq.OntQuery(rq)
        oq.OntCuries({p:i for p, i in g.namespaces()})
        oq.OntTerm.query = query
        oq.query.QueryResult._OntTerm = oq.OntTerm
        OntTerm = oq.OntTerm


        # if you don't set this QueryResult will switch to pyontutils and hit interlex so very slow
        sparc_ents = OntTerm.search(None, prefix='sparc')
        ontids = sorted(OntId(u) for u in
                        set(e for t in g for e in t
                            if isinstance(e, rdflib.URIRef) and 'sparc/' in e))

        {
            'protc:*measure': 'sparc:Acquisition',
            ('protc:aspect', ()): 'sparc:Anesthesia',
            ('protc:aspect',): 'sparc:Acquisition',   # not actualized probably
            None: 'sparc:perfusionProtocol',
            }
        # first pass
        # # do a first pass to have 0 or 1 on all the edges

        # collapse
        # link all parts of protocls into experiments
        embed()
        self._triples = tuple()  # TODO

        protocol = list(self.protocols)
        inputs = [p for p in protocol if p.astType == 'protc:input']
        aspects = [p for p in protocol if p.astType == 'protc:aspect']
        parameters = [p for p in protocol if p.astType == 'protc:parameter*']
        measure = [p for p in protocol if p.astType == 'protc:*measure' or p.astType == 'protc:symbolic-measure']
        telos = [p for p in protocol if p.astType == 'protc:telos']


    @property
    def onts(self):
        #self.onts = rq.onts  # FIXME this is obscure and indirect sort the imports so it is clear
        yield from self.rq.onts

    @property
    def protocols(self):
        yield from (p for p in protc if '.html' in p.uri)# or (any('.html' in p for p in p.astParents if p is not None) if p.astParents is not None else False))

    @property
    def inputs(self):
        yield from (p for p in protocol if p.astType == 'protc:input')

    @property
    def triples(self):
        yield from self._triples


def sparc_mapping():
    tts = technique_to_sparc()

    protocols = list(tts.protocols)
    metadata_example = simpleOnt(filename=f'sparc-metadata-example',
                                 prefixes=oq.OntCuries._dict,  # FIXME 
                                 imports=[o for o in tts.onts if 'sparc-methods' in o],
                                 triples=tts.triples,
                                 comment='example converstion to sparc metadata',
                                 path='ttl/',
                                 branch='sparc',
                                 fail=False,
                                 _repo=True,
                                 write=False)

    embed()


#
# utility

class ParameterValue:
    def __init__(self, success, v, rest, indent=1):
        self.value = success, v, rest
        self.indent = ' ' * indent
    def __repr__(self):
        success, v, rest = self.value
        if rest:
            rest = json.dumps(rest)  # slower but at least correct :/
        if not success:
            out = f'{v}\n{self.indent}{rest}'
        else:
            out = v + (f'\n{self.indent}(rest {rest})' if rest else '')
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
    from pprint import pformat
    from time import sleep, time
    from core import annoSync
    from docopt import docopt
    import requests
    args = docopt(__doc__)

    global annos  # this is now only used for making embed sane to use
    get_annos, annos, stream_thread, exit_loop = annoSync('/tmp/protcur-analysis-annos.pickle',
                                                          helpers=(HypothesisHelper, Hybrid, protc, SparcMI))

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

    #@profile_me
    def perftest():
        [protc(a, annos) for a in annos]
    start = time()
    perftest()
    pc = protc.byId(problem_child)
    stop = time()
    print('BAD TIME', stop - start)
    #@profile_me  # a note that trying ot get pref data when there are lots of function calls nearly doubles actual time...
    def text():
        t = protc.parsed()
        with open('/tmp/protcur.rkt', 'wt') as f: f.write(t)
        # don't return to avoid accidentally repring these fellows :/
    #p = protc.byId('nofnAgwtEeeIoHcLZfi9DQ')  # serialization error due to a cycle
    #print(repr(p))
    start = time()
    text()
    stop = time()
    print('BAD TIME', stop - start)
    def more():
        tl = protc.topLevel()
        with open('/tmp/top-protcur.rkt', 'wt') as f: f.write(tl)
        pl = protc.parentless()
        with open('/tmp/pl-protcur.rkt', 'wt') as f: f.write(pl)
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

    #sparc_mapping()
    OntTerm, ghq = oqsetup()
    SparcMI.graph = ghq.graph
    smi = [SparcMI(a, annos) for a in annos]
           #if any(t.startswith('sparc:') for t in a.tags)]
    s = smi[0]
    embed()
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
    # HOW DO I KILL THE STREAM LOOP!??! << answered, though quite a bit more complicated than expected

if __name__ == '__main__':
    main()
