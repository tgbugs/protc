import re
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
          'hyputils',
          'Markdown',
          'pyontutils',
          'pysercomb',
      ],
      extras_require={'dev':[]},
      entry_points={
          'console_scripts': [
              'protcur-server=protcur.server:main',
              'protcur-analysis=protcur.analysis:main',
          ],
      },
     )
