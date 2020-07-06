#!/usr/bin/env python3.7
""" protcur cli
Usage:
    protcur export [options] <group-name> <path>

Options:
    -d --debug
    -o --output-type=OTYPE    output type [default: lang]
                              options: lang top less need
    -t --output-format=OFMT   output format [default: rkt]
                              options: rkt  html

"""

from pathlib import Path
from pyontutils import clifun as clif
from protcur.server import make_ast
from protcur.analysis import protc


class Options(clif.Options):
    """ wheeeeee """

    @property
    def __output_type(self):
        return [self._args['--output-type']]


class Main(clif.Dispatcher):

    def _output(self):
        type_ =self.options.output_type
        fmt = self.options.output_format
        partition = lambda o: o.uri_api_int, o.uri_normalized  # TODO need the parition functions e.g. for per document
        if fmt == 'html':
            ast = protc
            join = ast._repr_join.replace('\n', '<br>\n').join
            selector = dict(
                lang = lambda o: o.isAstNode and not o.hasAstParent,
                top = lambda o: o.is_top_level(),
                flat = lambda o: o.isAstNode,
                need = lambda o: o.isAstNode and not o.needsParent,
            )[type_]
            everything = (o for o in ast if o is not None and selector(o))
            return make_ast(everything, ast)

        elif fmt == 'rkt':
            output_name = {'lang': 'protcurLang',
                           'top': 'topLevel',
                           'flat': 'flatall',
                           'need': 'parentneed',}[type_]
            return getattr(protc, output_name)()

    def export(self):
        from pyontutils.config import auth
        from hyputils.hypothesis import group_to_memfile, Memoizer

        group_id = auth.dynamic_config.secrets('hypothesis', 'group', self.options.group_name)
        cache_file = group_to_memfile(group_id + 'protcur-cli')  # note, caching is not memoization (duh)
        get_annos = Memoizer(memoization_file=cache_file, group=group_id)
        annos = get_annos()
        [protc(a, annos) for a in annos]

        path = Path(self.options.path)
        with open(path, 'wt') as f:
            f.write(self._output())


def main():
    from docopt import docopt, parse_defaults
    args = docopt(__doc__, version='protcur 0.0.0')
    defaults = {o.name:o.value if o.argcount else None for o in parse_defaults(__doc__)}
    options = Options(args, defaults)
    main = Main(options)
    if main.options.debug:
        print(main.options)

    main()


if __name__ == '__main__':
    main()
