from hyputils.hypothesis import Memoizer, group
from hyputils.subscribe import preFilter, AnnotationStream
from hyputils.handlers import helperSyncHandler, filterHandler

if group == '__world__':
    raise ValueError('Group is set to __world__ please run the usual `export HYP_ ...` command.')

def annoSync(memoization_file='/tmp/protc-annotations.pickle', helpers=tuple()):
    get_annos = Memoizer(memoization_file=memoization_file)
    yield get_annos
    prefilter = preFilter(groups=[group]).export()
    helperSyncHandler.memoizer = get_annos
    helperSyncHandler.helpers = helpers
    annos = get_annos()
    yield annos
    stream_loop = AnnotationStream(annos, prefilter, helperSyncHandler)()
    yield stream_loop

def atag(href, value=None, new_tab=False):
    target = ' target="_blank"' if new_tab else ''
    if value is None:
        value = href
    return f'<a href="{href}"{target}>{value}</a>'

def deltag(text):
    return f'<del>{text}</del>'

