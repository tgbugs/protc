import unittest


class TestDataFiles(unittest.TestCase):
    def test_exists_units(self):
        from protcur.config import __units_folder__ as units_folder
        assert units_folder.exists(), units_folder

    def test_exists_test_params(self):
        from protcur.config import __units_test_params__ as test_params
        assert test_params.exists(), test_params

    def test_exists_tags(self):
        from protcur.config import __anno_tags__ as anno_tags
        from protcur.config import __protc_tags__ as protc_tags
        assert anno_tags.exists()
        assert protc_tags.exists()
