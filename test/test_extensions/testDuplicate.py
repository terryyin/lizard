import unittest
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector

class TestDuplicateExtension(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()

    def test_empty_file(self):
        get_cpp_fileinfo_with_extension('''
        ''', self.detector)
        self.assertEqual([], self.detector.duplicates)

