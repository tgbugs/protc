from hyputils.hypothesis import Memoizer, group
from hyputils.subscribe import preFilter, AnnotationStream
from hyputils.handlers import annotationSyncHandler

def annoSync(memoization_file='/tmp/protc-annotations.pickle'):
    get_annos = Memoizer(memoization_file=memoization_file)
    yield get_annos
    prefilter = preFilter(groups=[group]).export()
    annotationSyncHandler.memoizer = get_annos
    annos = get_annos()
    yield annos
    stream_loop = AnnotationStream(annos, prefilter, annotationSyncHandler)()
    yield stream_loop
