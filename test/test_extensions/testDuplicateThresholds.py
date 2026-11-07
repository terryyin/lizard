import unittest
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector
from .duplicate_c_function_builder import CFunctionBuilder


class TestDuplicateThresholds(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    def detect(self, code, min_duplicate_tokens = 31):
        list(self.detector.cross_file_process(
                [get_cpp_fileinfo_with_extension(code, self.detector)]
            ))
        return list(self.detector.get_duplicates(min_duplicate_tokens))

    def test_repeating_patterns(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .six_line_function()
                .six_line_function()
                .build()
                )
        self.assertEqual(1, len(duplicates))

    def test_repeating_patterns_multiple_matches(self):
        duplicates = self.detect(
                self.builder
                .five_line_function()
                .six_line_function()
                .five_line_function()
                .six_line_function()
                .six_line_function()
                .build()
                )
        self.assertEqual(2, len(duplicates))

    def test_threshold(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .build(),
                min_duplicate_tokens = 50
                )
        self.assertEqual(0, len(duplicates))

    def test_threshold_with_too_many_repeatings(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .six_line_function()
                .build(),
                min_duplicate_tokens = 50
                )
        self.assertEqual(1, len(duplicates))
        self.assertEqual(3, len(duplicates[0]))
        self.assertEqual(1, duplicates[0][0].start_line)
        self.assertEqual(6, duplicates[0][0].end_line)

    def test_threshold_exceeded(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .five_line_function()
                .six_line_function()
                .five_line_function()
                .build(),
                min_duplicate_tokens = 50
                )
        self.assertEqual(1, len(duplicates))
