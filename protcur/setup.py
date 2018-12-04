from setuptools import setup

with open('README.org', 'rt') as f:
    # TODO pandoc conversion
    long_description = f.read()

setup(name='protcur',
      version='0.0.1',
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
          'hyputils',
          'Markdown',
          'pyontutils',
          'pysercomb',
      ],
      extras_require={'dev':[]},
      entry_points={
          'console_scripts': [
              'protcur-server=protcur.server:main'
          ],
      },
     )
