import unittest
from unittest.mock import patch
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector
from lizard import analyze_files, get_extensions
from .duplicate_c_function_builder import CFunctionBuilder


class TestDuplicateExtensionAcrossFiles(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    @patch('lizard_core.file_analyzer.auto_read')
    def detect(self, source_files, auto_read):
        auto_read.side_effect = lambda filename: source_files[filename]
        extensions = get_extensions([self.detector])
        list(analyze_files(sorted(source_files.keys()), exts=extensions))
        return list(self.detector.get_duplicates(30))

    def test_basic_duplicate(self):
        duplicates = self.detect({
                'f1.cpp': self.builder
                    .six_line_function()
                    .build(),
                'f2.cpp': self.builder
                    .five_line_function()
                    .six_line_function()
                .build()
                })
        self.assertEqual(1, len(duplicates))
        self.assertEqual("f1.cpp", duplicates[0][0].file_name)
        self.assertEqual("f2.cpp", duplicates[0][1].file_name)
        self.assertEqual(1, duplicates[0][0].start_line)
        self.assertEqual(6, duplicates[0][0].end_line)
        self.assertEqual(6, duplicates[0][1].start_line)
        self.assertEqual(11, duplicates[0][1].end_line)
        self.assertAlmostEqual(0.666666666, self.detector.duplicate_rate())

    def test_more_duplicate(self):
        duplicates = self.detect({
                'f1.cpp': self.builder
                    .six_line_function()
                    .build(),
                'f2.cpp': self.builder
                    .six_line_function()
                    .build(),
                'f3.cpp': self.builder
                    .six_line_function()
                    .build()
                })
        self.assertEqual(1, len(duplicates))
