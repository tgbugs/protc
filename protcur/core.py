from hyputils.hypothesis import Memoizer, group
from hyputils.subscribe import preFilter, AnnotationStream
from hyputils.handlers import helperSyncHandler, filterHandler

def annoSync(memoization_file='/tmp/protc-annotations.pickle', helpers=tuple(), tags=tuple()):
    if group == '__world__':
        raise ValueError('Group is set to __world__ please run the usual `export HYP_ ...` command.')
    get_annos = Memoizer(memoization_file=memoization_file)
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
    stream_thread, exit_loop = AnnotationStream(annos, prefilter, helperSyncHandler)()
    yield stream_thread
    yield exit_loop

def linewrap(text, start, end=80, sep='|', space=' ', ind=4, depth=0):
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
    for token in text.split(' '):
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

    out = f'\n{t}'.join(output)
    return out
