import unittest
from pathlib import Path
from pyontutils.utils import get_working_dir
from pyontutils.integration_test_helper import _TestScriptsBase, Folders, Repo
import protcur


class TestScripts(Folders, _TestScriptsBase):
    """ woo! """


skip = ('app',)
ci_skip = tuple()

mains = {'analysis': ['protcur-analysis', '--help'],
         'cli': ['protcur', '--help'],
         'server': ['protcur-server', '--help'],
        }

module_parent = Path(__file__).resolve().parent.parent
working_dir = get_working_dir(__file__)
if working_dir is None:
    # python setup.py test will run from the module_parent folder
    # I'm pretty the split was only implemented because I was trying
    # to run all tests from the working_dir in one shot, but that has
    # a number of problems with references to local vs installed packages
    working_dir = module_parent

post_load = lambda : None
post_main = lambda : None

TestScripts.populate_tests(protcur, working_dir, mains,
                           skip=skip, ci_skip=ci_skip,
                           module_parent=module_parent,
                           post_load=post_load, post_main=post_main,
                           only=[], do_mains=True)
