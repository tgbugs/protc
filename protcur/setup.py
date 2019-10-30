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


RELEASE = '--release' in sys.argv
if RELEASE:
    sys.argv.remove('--release')
    from protcur.config import __units_folder__, __units_test_folder__
    units = [p.name for p in __units_folder__.iterdir()
             if p.is_file() and p.name != 'parsing.rkt' and p.suffix == '.rkt']
    units_test = [p.parent.name + '/' + p.name
                  for p in __units_test_folder__.iterdir()
                  if p.is_file() and p.suffix == '.rkt']
    shutil.copytree(__units_folder__, 'units')
    data_files=[('protcur/units', units),
                ('protcur/units/test', units_test)]
else:
    data_files = []

tests_require = ['pytest', 'pytest-runner']

try:
    setup(name='protcur',
          version=__version__,
          description='A dashboard for web annotation workflows for protocol curation.',
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
              'hyputils>=0.0.4',
              'Markdown',
              'pyontutils>=0.1.3',
              'pysercomb',
          ],
          extras_require={'dev':[],
                          'test': tests_require,
                         },
          entry_points={
              'console_scripts': [
                  'protcur-server=protcur.server:main',
                  'protcur-analysis=protcur.analysis:main',
              ],
          },
          data_files=data_files,
         )
finally:
    if RELEASE:
        shutil.rmtree('units')
