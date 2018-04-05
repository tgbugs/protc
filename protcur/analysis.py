#!/usr/bin/env python3.6

import os
import re
import ast
from collections import Counter
from IPython import embed
from pyontutils.hierarchies import creatTree
from pyontutils.utils import async_getter, noneMembers, allMembers, anyMembers
from pyontutils.core import makeGraph, makePrefixes
from pyontutils.scigraph_client import Vocabulary
from pysercomb import parsing
from pysercomb import parsing_parsec
from hyputils.hypothesis import HypothesisAnnotation, HypothesisHelper, idFromShareLink, shareLinkFromId
from desc.prof import profile_me

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
        return os.path.splitext(os.path.basename(uri))[0]

HLPREFIX = 'http://hypothesis-local.olympiangods.org/' 
def hypothesis_local(hln):
    return HLPREFIX + hln + '.pdf'

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
def justTags():
    for tag in sorted(readTagDocs().keys()):
        if anyMembers(tag, *tag_prefixes):
            yield tag

def addDocLinks(base_url, doc):
    prefix = base_url + '/'
    return re.sub(r'`((?:protc|mo):[^\s]+)`', rf'[\1]({prefix}\1)', doc)

# stats

def citation_triples(annos):
    p = RFU
    for anno in annos:
        hl = get_hypothesis_local(anno.uri)
        if hl:
            s = hl
            if p in anno.tags:
                urls = extract_links_from_markdown(anno.text)
                for url in urls:
                    o = get_hypothesis_local(url)
                    o = o if o else url
                    yield p, s, o

def citation_tree(annos):
    t = citation_triples(annos)
    PREFIXES = {'protc':'http://protc.olympiangods.org/curation/tags/',
                'hl':'http://hypothesis-local.olympiangods.org/'}
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
    tree, extra = creatTree('hl:ma2015.pdf', RFU, 'OUTGOING', 10, json=ref_graph, prefixes=PREFIXES)
    return tree, extra

def papers(annos):
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
    prefix_ast = tuple()  # the tag prefix(es) that are part of the ast
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}

    def __init__(self, anno, annos):
        super().__init__(anno, annos)
        if len(self.objects) == len(self._annos):  # all loaded
            # populate annotation links from the text field to catch issues early
            printD('populating children')
            [c for p in self.objects.values() for c in p.children]

    def _fix_implied_input(self):
        if ': ' in self.text and 'hyp.is' in self.text:
            value_children_text = self.text.split(':', 1)[1]
            value, children_text = value_children_text.split('\n', 1)
            return value.strip(), children_text.strip()
        else:
            return '', ''

    @property
    def isAstNode(self):
        return (noneMembers(self._tags, *self.control_tags)
                and all(noneMembers(tag, *self.prefix_skip_tags) for tag in self.tags)
                and any(anyMembers(tag, *self.prefix_ast) for tag in self.tags))

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

        if anyMembers(self.tags, *('protc:implied-' + s for s in ('input', 'output', 'aspect'))):  # FIXME hardcoded fix
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
        return out + add_tags

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

    @property
    def tag_corrections(self):
        tagset = self._tags
        for ctag in self.control_tags:
            if ctag in self._tags:
                if ctag == 'annotation-correction':
                    ctag = 'annotation-tags:replace'
                return [ctag] + list(self._cleaned_tags)

    def text_correction(self, suffix):  # also handles additions
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
                correction = _correction
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
        if 'protc:implied-aspect' in self.tags:
            yield self.parent
            return
        for id_ in self._children_ids:
            child = self.getObjectById(id_)
            if child is None:
                print(f"WARNING: child of {self.__class__.__name__}.byId('{self.id}') {id_} does not exist!")
                continue 
            for reply in child.replies:
                if 'protc:implied-aspect' in reply.tags:
                    self.hasAstParent = True  # FIXME called every time :/
                    yield reply
                    child = None  # inject the implied aspect between the input and the parameter
                    break

            if child is not None: # sanity
                child.hasAstParent = True  # FIXME called every time :/
                yield child  # buildAst will have a much eaiser time operating on these single depth childs

    def __repr__(self, depth=0, cycle=None):
        if cycle == self:
            f'\n{" " * 4 * (depth + 1)}* {cycle.id} has a circular reference with this node {self.id}'
            return ''  # prevent loops
        elif cycle == None:
            cycle = self
        start = '|' if depth else ''
        t = ' ' * 4 * depth + start

        parent_id =  f"\n{t}parent_id:    {self.parent.id} {self.__class__.__name__}.byId('{self.parent.id}')" if self.parent else ''
        exact_text = f'\n{t}exact:        {self.exact}' if self.exact else ''

        text_align = 'text:         '
        lp = f'\n{t}'
        text_line = lp + ' ' * len(text_align)
        text_text = lp + text_align + self.text.replace('\n', text_line) if self.text else ''


        value_text = f'\n{t}value:        {self.value}'
        tag_text =   f'\n{t}tags:         {self.tags}' if self.tags else ''

        lct = list(self._cleaned_tags)
        ct = f'\n{t}cleaned tags: {lct}' if self.references and lct and lct != self.tags else ''
        tc = f'\n{t}tag_corrs:    {self.tag_corrections}' if self.tag_corrections else ''

        replies = ''.join(r.__repr__(depth + 1, cycle=cycle)
                          for r in self.replies)
                          #if not print(cycle.id, self.id, self.shareLink))
        rep_ids = f'\n{t}replies:      ' + ' '.join(f"{self.__class__.__name__}.byId('{r.id}')"
                                                    for r in self.replies)
        replies_text = (f'\n{t}replies:{replies}' if self.reprReplies else rep_ids) if replies else ''

        childs = ''.join(c.__repr__(depth + 1, cycle=cycle)
                         if c != self else
                         f'\n{" " * 4 * (depth + 1)}* {c.id} has a circular reference with this node {self.id}'  # avoid recursion
                         for c in self.children
                         # avoid accidental recursion with replies of depth 1 TODO WE NEED TO GO DEEPER
                         if c != self.parent)
                         #and not print(cycle.id, self.id, self.shareLink))
        childs_text = f'\n{t}children:{childs}' if childs else ''

        return (f'\n{t.replace("|","")}*--------------------'
                f"\n{t}{self.__class__.__name__ + ':':<14}{self.shareLink} {self.__class__.__name__}.byId('{self.id}')"
                f'\n{t}isAstNode:    {self.isAstNode}'
                f'{parent_id}'
                f'{exact_text}'
                f'{text_text}'
                f'{value_text}'
                f'{tag_text}'
                f'{ct}'
                f'{tc}'
                f'{replies_text}'
                f'{childs_text}'
                f'\n{t}____________________')


class AstGeneric(Hybrid):
    """ Base class that implements the core methods needed for parsing various namespaces """
    control_tags = 'annotation-correction', 'annotation-tags:replace', 'annotation-tags:add', 'annotation-tags:delete' 
    prefix_skip_tags = 'PROTCUR:', 'annotation-'
    text_tags = ('annotation-text:exact',
                 'annotation-text:text',
                 'annotation-text:value',
                 'annotation-text:children',
                 'annotation-correction')
    children_tags = 'annotation-children:delete',
    #indentDepth = 2
    #objects = {}
    #_tagIndex = {}
    #_order = tuple()
    #_topLevel = tuple()
    linePreLen = 0
    @staticmethod
    def _value_escape(value):
        return '"' + value.strip().replace('"', '\\"') + '"'

    def _dispatch(self):
        def inner():
            return self._value_escape(self.value)
        type_ = self.astType
        if type_ is None:
            raise TypeError(f'Cannot dispatch on NoneType!\n{super().__repr__()}')
        namespace, dispatch_on = type_.split(':', 1)
        if namespace != self.__class__.__name__:
            raise TypeError(f'{self.__class__.__name__} does not dispatch on types from '
                            f'another namespace ({namespace}).')
        dispatch_on = dispatch_on.replace('*', '').replace('-', '_')
        return getattr(self, dispatch_on, inner)()

    @classmethod
    def parsed(cls):
        return ''.join(sorted(repr(o) for o in cls.objects.values() if o is not None and o.isAstNode))

    @classmethod
    def topLevel(cls):
        return ''.join(sorted(repr(o) for o in cls.objects.values()
                              if o is not None and o.isAstNode and not o.hasAstParent and o.astType in cls._topLevel))

    @classmethod
    def parentless(cls):
        return ''.join(sorted(repr(o) for o in cls.objects.values()
                              if o is not None and o.isAstNode and not o.hasAstParent))

    @property
    def astType(self):
        if self.isAstNode:
            tags = self.tags
            for tag in self._order:
                ctag = 'protc:' + tag
                if ctag in tags:
                    return ctag
            if len(tags) == 1:
                return tags[0]
            elif len(list(self._cleaned_tags)) == 1:
                return next(iter(self._cleaned_tags))
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
            try:
                #return self.astType + self.astValue >= other.astType + other.astValue
                return self.astType + self.value >= other.astType + other.value
            except TypeError as e:
                embed()
                raise e

    def __lt__(self, other):
        #if type(self) == type(other) and self.isAstNode and other.isAstNode:
        return not self.__gt__(other)
        #else:
            #return False

    def __repr__(self, depth=1, nparens=1, plast=True, top=True, cycle=False):
        out = ''
        type_ = self.astType 
        if type_ is None:
            if cycle:
                print('Circular link in', self.shareLink)
                out = f"'(circular-link no-type {cycle.id})" + ')' * nparens
                type_ = 'None'
            else:
                return super().__repr__()

        self.linePreLen = self.indentDepth * (depth - 1) + len('(') + len(type_) +  len(' ')
        value = self.astValue
        self.linePreLen += self.indentDepth  # doing the children now we bump back up
        comment = f'  ; {self.shareLink}'

        children = list(self.children)  # better to run the generator once up here
        if children:
            indent = ' ' * self.indentDepth * depth
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
                    if self in c.children:  # FIXME cannot detect longer cycles
                        if cycle:
                            print('Circular link in', self.shareLink)
                            s = f"'(circular-link {cycle.id})" + ')' * nparens
                        else:
                            s = c.__repr__(depth + 1, new_nparens, new_plast, False, self)
                    else:
                        s = c.__repr__(depth + 1, new_nparens, new_plast, False)
                except TypeError as e:
                    # do not remove or bypass this error, it means that one of your
                    # dicts like _replies or objects has members of some other class
                    # and is actually probably being inherited from that class
                    # you should give this class its own dictionary for that
                    raise TypeError(f'{c} is not an {self.__class__.__name__}') from e
                cs.append(s)
            childs = comment + linestart + linestart.join(cs)
        else:
            childs = ')' * nparens + comment  

        start = '\n(' if top else '('  # ))
        #print('|'.join(''.join(str(_) for _ in range(1,10)) for i in range(12)))
        return f'{start}{type_} {value}{childs}'


class protc(AstGeneric):
    prefix_ast = 'protc:',
    indentDepth = 2
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}  # without this Hybrid replies will creep in
    _order = (  # ordered based on dependence and then by frequency of occurence for performance (TODO tagdefs stats automatically)
              'structured-data-record',  # needs to come first since record contents also have a type (e.g. protc:parameter*)
              'parameter*',
              'input',
              'invariant',
              'references-for-use',
              'aspect',
              'black-box-component',
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
            )
    _topLevel = tuple('protc:' + t for t in ('input',
                                             'output',
                                             'implied-input',
                                             'implied-output',
                                             '*measure',
                                             'symbolic-measure',
                                             'black-black-component',
                                            ))
    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'

    format_nl_long =  '^'
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
        if v is None:
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


    def _parameter_parsec(self):  # more than 2x slower than my version >_<
        #def parameter(self):
        out = getattr(self, '_parameter', None)
        if out is not None:
            return repr(out)
        value = self.value
        if value == '':  # breaks the parser :/
            return ''
        cleaned = value.replace(' mL–1', ' * mL–1').replace(' kg–1', ' * kg–1')  # FIXME temporary (and bad) fix for superscript issues
        cleaned = cleaned.strip()
        cleaned_orig = cleaned

        # ignore gargabe at the start
        success = False
        front = ''
        max_ = len(cleaned)
        v = None
        for i in range(max_):
            Value = parsing_parsec.parameter_expression(cleaned, i)
            if Value.status:
                v = Value.value
                front = cleaned[:i]
                rest = cleaned[Value.index:]
                break

        if v is None:
            rest = cleaned
            front = ''

        def format_value(tuple_):
            out = []
            if tuple_:
                for v in tuple_:
                    if type(v) is tuple:
                        v = format_value(v)
                    if v is not None:
                        out.append(f'{v}')
            if out:
                return '(' + ' '.join(out) + ')'

        if v is not None:
            v = format_value(v)
        test_params.append((value, (Value.status, v, rest)))
        self._parameter = ParameterValue(success, v, rest, front)
        return repr(self._parameter)

    def invariant(self):
        return self.parameter()

    def input(self):
        value = self.value
        #data = sgv.findByTerm(value)  # TODO could try the annotate endpoint? FIXME _extremely_ slow so skipping
        data = None
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
        def manual_corrections(v):
            if v == 'PB':
                v = 'phosphate buffer'
            elif v == 'PBS':
                v = 'phosphate buffered saline'
            return v 
        value = manual_corrections(value)
        return self._value_escape(value)

    def output(self):
        return self.input()

    #def structured_data(self):

    def structured_data_header(self):
        return "'(\"" + '" "'.join(self.value.split('\n')) + '")'

    def structured_data_record(self):
        return "'(\"" + '" "'.join(self.value.split('\n')) + '")'

    def references_for_use(self):
        esc = r'\;'
        return '\n'.join(f'''{" " * self.linePreLen if i else ""}{"'" + link.replace(";", esc) if HLPREFIX in link else "(TODO '" + link.replace(";", esc) + ")"}'''
                         for i, link in enumerate(sorted(extract_links_from_markdown(self.value))))
    #def implied_input(self): return value
    #def structured_data(self): return self.value
    #def measure(self): return self.value
    #def symbolic_measure(self): return self.value

#
# utility

class ParameterValue:
    def __init__(self, success, v, rest, indent=1):
        self.value = success, v, rest
        self.indent = ' ' * indent
    def __repr__(self):
        success, v, rest = self.value
        if not success:
            out = f'{v}\n{self.indent}"{rest}"'
        else:
            out = v + (f'\n{self.indent}(rest "{rest}")' if rest else '')
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
    import requests

    global annos  # this is now only used for making embed sane to use
    get_annos, annos, stream_loop = annoSync('/tmp/protcur-analysis-annos.pickle',
                                             helpers=(Hybrid, protc))

    problem_child = 'KDEZFGzEEeepDO8xVvxZmw'
    #test_annos(annos)
    tree, extra = citation_tree(annos)
    i = papers(annos)

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
    more()
    def update():
        protc.objects = {}
        protc._tagIndex = {}
        perftest()
        text()
        more()
    stream_loop.start()  # need this to be here to catch deletes
    embed()

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
