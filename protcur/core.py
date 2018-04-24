from hyputils.hypothesis import Memoizer, group
from hyputils.subscribe import preFilter, AnnotationStream
from hyputils.handlers import helperSyncHandler, filterHandler

def annoSync(memoization_file='/tmp/protc-annotations.pickle', helpers=tuple()):
    if group == '__world__':
        raise ValueError('Group is set to __world__ please run the usual `export HYP_ ...` command.')
    get_annos = Memoizer(memoization_file=memoization_file)
    yield get_annos
    prefilter = preFilter(groups=[group]).export()
    helperSyncHandler.memoizer = get_annos
    helperSyncHandler.helpers = helpers
    annos = get_annos()
    yield annos
    stream_loop = AnnotationStream(annos, prefilter, helperSyncHandler)()
    yield stream_loop

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
