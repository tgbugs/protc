import re
import sys
import shutil
from pathlib import Path
from setuptools import setup


def find_version(filename):
    _version_re = re.compile(r"__version__ = '(.*)'")
    for line in open(filename):
        version_match = _version_re.match(line)
        if version_match:
            return version_match.group(1)


__version__ = find_version('protcur/__init__.py')

with open('README.org', 'rt') as f:
    # TODO pandoc conversion
    long_description = f.read()


ru = Path('resources', 'units')
RELEASE = '--release' in sys.argv
WHEEL_FROM_GIT = not ru.exists() and 'bdist_wheel' in sys.argv
if RELEASE or WHEEL_FROM_GIT:
    if RELEASE:
        sys.argv.remove('--release')

    from protcur.config import __anno_tags__, __protc_tags__
    from protcur.config import __units_folder__, __units_test_folder__

    units = [p.name for p in __units_folder__.iterdir()
             if p.is_file() and p.name != 'parsing.rkt' and p.suffix == '.rkt']
    units_test = [p.parent.parent.name + '/' + p.parent.name + '/' + p.name
                  for p in __units_test_folder__.iterdir()
                  if p.is_file() and p.suffix == '.rkt']

    shutil.copytree(__units_folder__, 'resources/units')
    shutil.copy(__anno_tags__, 'resources')
    shutil.copy(__protc_tags__, 'resources')

if ru.exists():  # release or running from sdist not in git
    data_files = [
        ('share/protcur', ['resources/anno-tags.rkt',
                           'resources/protc-tags.rkt']),
        ('share/protcur/units', [p.as_posix() for p in ru.glob('*.rkt')
                                 if p.name != 'parsing.rkt']),
        ('share/protcur/units/test', [p.as_posix() for p in (ru / 'test').glob('*.rkt')]),]
else:
    data_files = []

tests_require = ['pytest']

try:
    setup(name='protcur',
          version=__version__,
          description='Web annotation workflows for protocol curation.',
          long_description=long_description,
          long_description_content_type='text/plain',
          url='https://github.com/tgbugs/protc/tree/master/protcur',
          author='Tom Gillespie',
          author_email='tgbugs@gmail.com',
          license='MIT',
          classifiers=[],
          keywords=('protc protocols dashboard curation '
                    'hypothesis hypothes.is web annotation'),
          packages=['protcur'],
          python_requires='>=3.6',
          install_requires=[
              'flask',
              'htmlfn',
              'hyputils>=0.0.8',
              'idlib>=0.0.1.dev7',
              'Markdown',
              'pyontutils>=0.1.25',
              'pysercomb>=0.0.7',
          ],
          extras_require={'dev':['pytest-cov', 'wheel'],
                          'test': tests_require,
                         },
          entry_points={
              'console_scripts': [
                  'protcur=protcur.cli:main',
                  'protcur-server=protcur.server:main',
                  'protcur-analysis=protcur.analysis:main',
              ],
          },
          data_files=data_files,
         )
finally:
    if RELEASE or WHEEL_FROM_GIT:
        shutil.rmtree('resources/units')
        Path('resources', __anno_tags__.name).unlink()
        Path('resources', __protc_tags__.name).unlink()
