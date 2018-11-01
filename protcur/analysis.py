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
import json
import inspect
from pathlib import PurePath, Path
from collections import Counter
import rdflib
import ontquery as oq
from pyontutils.core import makeGraph, makePrefixes, OntId, simpleOnt
from pyontutils.utils import async_getter, noneMembers, allMembers, anyMembers, TermColors as tc
from pyontutils.config import devconfig
from pyontutils.htmlfun import atag
from pyontutils.namespaces import ilxtr
from pyontutils.hierarchies import creatTree
from pyontutils.scigraph_client import Vocabulary
from pyontutils.closed_namespaces import rdf, rdfs, owl
from pysercomb import parsing
#from pysercomb import parsing_parsec
from hyputils.hypothesis import HypothesisAnnotation, HypothesisHelper, idFromShareLink, shareLinkFromId
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

error_output = []

# utility

def get_hypothesis_local(uri):
    if 'hypothesis-local' in uri:
        return PurePath(uri).stem

HLPREFIX = '://hypothesis-local.olympiangods.org/'
def hypothesis_local(hln, s=True):
    return ('https' if s else 'http') + HLPREFIX + hln + '.pdf'

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
    def __init__(self, doc, parents):
        self.parents = parents if isinstance(parents, tuple) else (parents,)
        if anyMembers(self.parents, *self._depflags):
            self.doc = '**DEPRECATED** ' + doc
            self.deprecated = True
        else:
            self.doc = doc
            self.deprecated = False


def readTagDocs():
    with open(f'{__script_folder__}/../protc-tags.rkt', 'rt') as f:
        text = f.read()
    with open(f'{__script_folder__}/../anno-tags.rkt', 'rt') as f:
        text += f.read()
    success, docs, rest = parsing.tag_docs(text)
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
    return re.sub(r'`((?:protc|mo):[^\s]+)`', rf'[\1]({prefix}\1)', doc)

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
    namespace = None  # the tag prefix for this ast, ONE PREFIX PER CLASS, use NamespaceTranslators for multiple
    tag_translators = {}
    additional_namespaces = {}
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}
    _astParentIndex = {}

    def __init__(self, anno, annos):
        super().__init__(anno, annos)
        if len(self.objects) == len(self._annos):  # all loaded
            # populate annotation links from the text field to catch issues early
            printD(f'populating children {anno.id}')  # share link many not exist
            [c for p in self.objects.values() for c in p.children]
            [p._addAstParent() for p in self.objects.values() if tuple(p.children)]  # handles updates
            # TODO deletes still an issue as always

    @property
    def prefix_ast(self):
        """ namespace with the colon to make it simple to allow
            namespaces that are substrings of other namespaces """
        if self.namespace is not None:
            return self.namespace + ':'
        else:
            return self.namespace

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

        return self._text

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

        return sorted(set(self._translate_tags(out + add_tags)))

    @property
    def _cleaned_tags(self):
        for tag in self.tags:
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
                        yield tt(tag).translation
                        continue

                yield tag

        else:
            return tags

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

    def __repr__(self, depth=0, cycle=tuple(), html=False, number='*', ind=4):
        #SPACE = '&nbsp;' if html else ' '
        SPACE = '\xA0' if html else ' '
        if self in cycle:
            print(tc.red('CYCLE DETECTED'), self.shareLink, self._repr)
            return f'\n{SPACE * ind * (depth + 1)}* {cycle[0].id} has a circular reference with this node {self.id}'
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
        childs = ''.join(c.__repr__(depth + 1, cycle=cycle, html=html) for c in children)
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
            return '\n' + t + prefix + SPACE * (spacing - len(prefix))
        #parent_id =  (f"\n{t}parent_id:{SPACE * (spacing - len('parent_id:'))}"
        def row(prefix, rest):
            # if thunking this works for deferring if it will be a miracle
            return (align_prefix(prefix) + rest()) if prefixes[prefix] else ''

        startn = '\n' if not isinstance(number, int) or number > 1 else ''

        details = '<details>' if html else ''
        _details = '</details>' if html else ''

        summary = '<summary>' if html else ''
        _summary = f'</summary>' if html else '\n'  # </summary> has an implicit <br> for reasons know only to w3c

        value_text = row('value:', lambda:linewrap(self.value, align, space=SPACE, depth=depth, ind=ind))

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

        endbar = f'\n{t:_<80}\n'

        def rm_n(*args):
            return ''.join(args).strip('\n')

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
                             endbar,
                             _details)))


class AstGeneric(Hybrid):
    """ Base class that implements the core methods needed for parsing various namespaces """
    generic_tags = 'TODO',
    control_tags = 'annotation-correction', 'annotation-tags:replace', 'annotation-tags:add', 'annotation-tags:delete'
    prefix_skip_tags = 'PROTCUR:', 'annotation-'
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
                tag = 'protc:' + suffix
                if tag in tags:
                    if (suffix in ('input', 'implied-input') and
                        not self.hasAstParent and
                        list(self.children)):
                        self._was_input = True  # FIXME make this clearer
                        return 'protc:output'
                    else:
                        return tag
            if len(tags) == 1:
                return tags[0]
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

    def __repr__(self, depth=1, nparens=1, plast=True, top=True, cycle=tuple(), html=False, number='*'):
        out = ''
        if self.astType is None:
            if self in cycle:
                cyc = ' '.join(c.id for c in cycle)
                print('Circular link in', self._repr, 'cycle', cyc)
                out = f"'(circular-link no-type (cycle {cyc}))" + ')' * nparens
                return out
            else:
                printD(tc.red('WARNING:'), f'unhandled type for {self._repr} {self.tags}')
                return super().__repr__(html=html, number=number)

        self.linePreLen = self.indentDepth * (depth - 1) + len('(') + len(str(self.astType)) +  len(' ')
        value = self.astValue
        self.linePreLen += self.indentDepth  # doing the children now we bump back up
        link = self.shareLink
        if html: link = atag(link, link)
        #SPACE = '&nbsp;' if html else ' '
        SPACE = '\xA0' if html else ' '
        comment = f'{SPACE}{SPACE}; {link}'

        children = sorted(self.children)  # better to run the generator once up here
        if children:
            indent = SPACE * self.indentDepth * depth
            linestart = '\n' + indent
            nsibs = len(children)
            cs = []
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
                    else:
                        #print('Circular link in', self.shareLink)
                        cyc = ' '.join(c.id for c in cycle)
                        print('Circular link in', self._repr, 'cycle', cyc)
                        s = f"'(circular-link no-type (cycle {cyc}))" + ')' * nparens
                        #s = f"'(circular-link {cycle[0].id})" + ')' * nparens
                except TypeError as e:
                    # do not remove or bypass this error, it means that one of your
                    # dicts like _replies or objects has members of some other class
                    # and is actually probably being inherited from that class
                    # you should give this class its own dictionary for that
                    raise TypeError(f'{c} is not an {self.classn}') from e  # XXX
                cs.append(s)
            childs = comment + linestart + linestart.join(cs)
        else:
            childs = ')' * nparens + comment

        start = '\n(' if top else '('  # ))
        #print('|'.join(''.join(str(_) for _ in range(1,10)) for i in range(12)))

        prov = f"(hyp: '{self.id})"

        return f'{start}{self.astType} {value} {prov}{childs}'


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
    }

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
                _, v, rest = parsing.parameter_expression(cleaned)
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

    def input(self, rank=('CHEBI', 'GO', 'UBERON', 'ilxtr', 'PATO')):
        value = self.value.strip()
        ont = ''
        if '(ont' in value:
            before, ont_after = value.split('(ont', 1)
            ont, after = ont_after.rsplit(')', 1)  # FIXME bad parsing
            ont = ' #:ont (ont' + ont
            value = before + after
            print(ont)

        def manual_corrections(v):
            if v == 'PB':
                v = 'phosphate buffer'
            elif v == 'PBS':
                v = 'buffered phosphate saline'
            elif v == 'APs':
                v = 'action potential'  # plural ...
            return v
        value = manual_corrections(value)

        # TODO OntTerm
        # extend input to include black_box_component, aspect, etc
        data = sgv.findByTerm(value, searchSynonyms=True, searchAbbreviations=True)  # TODO could try the annotate endpoint? FIXME _extremely_ slow so skipping
        #data = list(OntTerm.query(value))  # can be quite slow if hitting interlex
        #data = None
        if data:
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
            id_ = data['curie'] if 'curie' in data else data['iri']
            #value += f" ({id_}, {data['labels'][0]})"
            value = f"(term {id_} \"{data['labels'][0]}\" #:original \"{value}\"{ont})"
            #value = ("term", id_, data['labels'][0], "#:original", value)
            #raise ValueError(value)
            return value
        else:
            test_input.append(value)
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
    prefix_ast = Hybrid.prefix_ast
    _value_escape = AstGeneric._value_escape
    _dispatch = AstGeneric._dispatch
    astValue = AstGeneric.astValue
    namespace = None
    _order = tuple()
    isAstNode = True
    additional_namespaces = tuple()  # intentionally not a dict because dont want this

    def __init__(self, target_instance):
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
        return [t for t in self.target_instance.tags if t.startswith(self.prefix_ast)]

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
    _order = 'TODO',
    #target_type.additional_namespaces[namespace] = protc_generic  # FIXME

    def __init__(self, generic_instance):
        self.namespace = ''  # hack so we can reuse _dispatch
        super().__init__(generic_instance)

    @property
    def prefix_ast(self):
        # hack to fool dispatch, these are NO namespace, not empty namespace
        return ''

    @property
    def tags(self):
        return [t for t in self.target_instance.tags if ':' not in t]

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
        return '(protc:impl {self.value})'

    @od
    def participant(self):
        # FIXME ambiguous
        return '(protc:input {self.value})'


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
from pyontutils.annotation import AnnotationMixin


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

        self._triples = tuple()  # TODO


    @property
    def onts(self):
        #self.onts = rq.onts  # FIXME this is obscure and indirect sort the imports so it is clear
        yield from self.rq.onts

    @property
    def protocols(self):
        yield from (p for p in protc if '.html' in p.uri or any('.html' in p for p in p.astParents))

    @property
    def inputs(self):
        yield from (p for p in protocol if p.astType == 'protc:input')

    @property
    def triples(self):
        yield from self._triples


def sparc_mapping():
    tts = technique_to_sparc()

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

    sparc_mapping()
    #embed()
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
    # HOW DO I KILL THE STREAM LOOP!??!

if __name__ == '__main__':
    main()
