import re
from hyputils.hypothesis import Memoizer, group, ucd
from hyputils.subscribe import preFilter, AnnotationStream
from hyputils.handlers import helperSyncHandler, filterHandler
from pysercomb.parsers import racket
from pyontutils.utils import anyMembers, makeSimpleLogger
from protcur.config import __anno_tags__ as anno_tags
from protcur.config import __protc_tags__ as protc_tags

log = makeSimpleLogger('protcur')
logd = log.getChild('data')

# set hlog and plog to conform to pyontutils logging conventions
# note that this resets logd handler too since it is inherited
from hyputils.hypothesis import log as _hlog
_hlog.removeHandler(_hlog.handlers[0])
_hlog.addHandler(log.handlers[0])
from pysercomb.utils import log as _plog
_plog.removeHandler(_plog.handlers[0])
_plog.addHandler(log.handlers[0])


def url_doi(doi):
    return 'https://doi.org/' + doi


def url_pmid(pmid):
    return 'https://www.ncbi.nlm.nih.gov/pubmed/' + pmid.split(':')[-1]


def annoSync(memoization_file=None, helpers=tuple(), tags=tuple(), group=group, sync=True):
    if group == '__world__':
        raise ValueError('Group is set to __world__ please run the usual `export HYP_ ...` command.')
    get_annos = Memoizer(memoization_file=memoization_file, group=group)
    yield get_annos
    prefilter = preFilter(groups=[group], tags=tags).export()
    helperSyncHandler.memoizer = get_annos
    helperSyncHandler.helpers = helpers
    #def include_only_tag_prefix():
    #helperSyncHandler.filter = 
    annos = get_annos()
    if tags:
        annos = [a for a in annos if any(any(ft in at for ft in tags) for at in a.tags)]

    yield annos

    if sync:
        stream_thread, exit_loop = AnnotationStream(annos, prefilter, helperSyncHandler)()
        yield stream_thread
        yield exit_loop

    else:
        yield None
        yield None


def linewrap(text, start, end=80, sep='|', space=' ', nl='\n', ind=4, depth=0):
    text = text.replace('\n', ' ')
    if depth:
        pre = ''.join((space * ind) + sep for _ in range(depth))
        post = space * (start - depth * (ind + len(sep)))
        t = pre + post
    else:
        t = space * start
    clen = start - 1
    output = []
    cline = []
    for token in text.split():
        lt = len(token)
        if clen + lt >= end:
            cline.append(token)
            output.append(space.join(cline))
            clen = start
            cline = []
        else:
            clen += lt + 1
            cline.append(token)

    if cline:
        output.append(space.join(cline))

    out = f'{nl}{t}'.join(output)
    return out

def color_pda(string, OPEN, CLOSE,
              space=' ',
              STRING='<span class="string">',
              QUOTE='<span class="quote">',
              NUMBER='<span class="number">',
              KEYWORD='<span class="keyword">',
              count=0):
    """ state machine for adding spans to code """
    STOP = ' ()'  # always keep ' ' around for insurance
    if space not in STOP: STOP += space
    lsm1 = len(string) - 1
    STATE = None
    for i, char in enumerate(string):
        if char == '"':  # TODO escape
            if STATE == 'STRING':
                yield char + '</span>'
                STATE = None
            elif STATE == 'STRING-ESCAPE':
                yield char  # FIXME TODO multiple escapes
                STATE = 'STRING'
            else:
                yield STRING + char
                STATE = 'STRING'
        elif STATE == 'STRING':
            if char == '\\':
                STATE = 'STRING-ESCAPE'
            yield char
        elif char == "'":
            yield QUOTE
            STATE = 'QUOTE'
            yield char
        elif char == "#":
            # have to use lookahead here since we are yielding
            if string[i + 1] == ':':
                yield KEYWORD
                STATE = 'KEYWORD'
            yield char
        else:
            if STATE == 'STRING-ESCAPE':
                STATE = 'STRING'

            if char in STOP:
                if STATE in ('QUOTE', 'KEYWORD', 'NUMBER'):
                    yield '</span>'
                    STATE = None
                # lookahead
                if i < lsm1:
                    nchar = string[i + 1]
                    if nchar in '0123456789':
                        yield char
                        STATE = 'NUMBER'
                        yield NUMBER
                        continue

            if char == '(':
                yield OPEN(count)
                count += 1
            elif char == ')':
                yield CLOSE
                count -= 1
            else:
                yield char

    if STATE is not None:
        # we get to the end and the state is not None
        # terminate the current state because nothing else can
        yield '</span>'

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


def addDocLinks(base_url, doc):
    prefix = base_url + '/'
    return re.sub(r'`((?:protc|mo|sparc):[^\s]+)`', rf'[\1]({prefix}\1)', doc)


def readTagDocs():
    with open(protc_tags, 'rt') as f:
        text = f.read()
    with open(anno_tags, 'rt') as f:
        text += f.read()
    success, docs, rest = racket.tag_docs(text)
    if rest:
        raise SyntaxError(f'tag docs did not parse everything!\n{rest}')
    tag_lookup = {tag[1]:TagDoc(doc, parent) for _, tag, parent, doc in docs}
    return tag_lookup

tag_prefixes = 'ilxtr:', 'protc:', 'mo:', 'annotation-'
def justTags(tag_lookup=None):
    if tag_lookup is None:
        tag_lookup = readTagDocs()
    for tag, doc in sorted(tag_lookup.items()):
        if anyMembers(tag, *tag_prefixes) and not doc.deprecated:
            yield tag
