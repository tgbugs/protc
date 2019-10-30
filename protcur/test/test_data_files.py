import unittest


class TestDataFiles(unittest.TestCase):
    def test_exists_units(self):
        from protcur.config import __units_folder__ as units_folder
        assert units_folder.exists(), units_folder

    def test_exists_test_params(self):
        from protcur.config import __units_test_params__ as test_params
        assert test_params.exists(), test_params
