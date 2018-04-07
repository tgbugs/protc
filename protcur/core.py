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

def tag(_tag, n=False):
    nl = '\n' if n else ''
    s = f'<{_tag}>{nl}'
    e = f'{nl}</{_tag}>'
    def tagwrap(value):
        return s + value + e
    return tagwrap

def atag(href, value=None, new_tab=False, uriconv=None):
    target = ' target="_blank"' if new_tab else ''
    if value is None:
        value = href
        if uriconv is not None:
            href = uriconv(href)
    return f'<a href="{href}"{target}>{value}</a>'

def deltag(text):
    return f'<del>{text}</del>'

htmltag = tag('html', n=True)
headtag = tag('head', n=True)
titletag = tag('title')
styletag = tag('style', n=True)
scripttag = tag('script', n=True)
bodytag = tag('body', n=True)

def htmldoc(body, title='Spooky Nameless Page', styles=tuple(), scripts=tuple()):
    header = ('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"\n'
              '"http://www.w3.org/TR/html4/loose.dtd">\n')
    styles = '\n'.join((styletag(s) for s in styles))
    scripts = '\n'.join((scripts(s) for s in scripts))
    head = headtag('\n'.join((titletag(title), '<meta charset="UTF-8">', styles, scripts)))
    return header + htmltag('\n'.join((head, bodytag(body))))


