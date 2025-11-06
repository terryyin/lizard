"""Tests for output format when CogC extension is explicitly loaded"""
import unittest
import sys
from mock import Mock
from test.helper_stream import StreamStdoutTestCase
from lizard import (print_and_save_modules, FunctionInfo, FileInformation,
                   print_clang_style_warning, parse_args, get_extensions, OutputScheme)
from lizard_ext.lizardcogc import LizardExtension as CogC


class Ext(object):
    FUNCTION_INFO = {"max_nesting_depth": {"caption": "  ND  ", "average_caption": " Avg.ND "}}


class TestFunctionOutputWithCogC(StreamStdoutTestCase):
    """Test output format when CogC extension is explicitly loaded"""

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        # Explicitly load CogC extension
        self.extensions = get_extensions([CogC()])
        self.scheme = OutputScheme(self.extensions)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        # Initialize cognitive_complexity attribute
        self.foo.cognitive_complexity = 0

    def test_function_info_header_should_include_cogc(self):
        """When CogC extension is loaded, header should include CogC column"""
        print_and_save_modules([],  self.scheme)
        self.assertEqual("  NLOC    CCN   token  PARAM  length  CogC   location  ",
                        sys.stdout.stream.splitlines()[1])

    def test_function_info_header_with_cogc_and_external_extension(self):
        """Test that CogC and external extensions appear in correct order"""
        external_extension = Mock(FUNCTION_INFO = {"xx": {"caption":"*external_extension*"}}, ordering_index=-1)
        extensions = get_extensions([CogC(), external_extension])
        scheme = OutputScheme(extensions)
        print_and_save_modules([],  scheme)
        # External extension with ordering_index=-1 is inserted before CogC
        self.assertEqual("  NLOC    CCN   token  PARAM  length *external_extension* CogC   location  ",
                        sys.stdout.stream.splitlines()[1])

    def test_print_fileinfo_includes_cogc_value(self):
        """Function info should include CogC value when extension is loaded"""
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        self.foo.cognitive_complexity = 5
        fileStat = FileInformation("FILENAME", 1, [self.foo])
        print_and_save_modules([fileStat],  self.scheme)
        # Column order: NLOC CCN token PARAM length CogC location
        self.assertEqual("       1     16      1      0       1      5 foo@100-100@FILENAME",
                        sys.stdout.stream.splitlines()[3])


class TestWarningOutputWithCogC(StreamStdoutTestCase):
    """Test warning output when CogC extension is explicitly loaded"""

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        # Initialize cognitive_complexity attribute
        self.foo.cognitive_complexity = 0

    def test_clang_format_warning_includes_cogc(self):
        """Warning format should include CogC when extension is loaded"""
        self.foo.cyclomatic_complexity = 30
        self.foo.cognitive_complexity = 12
        self.foo.max_nesting_depth = 10
        fileSummary = FileInformation("FILENAME", 123, [self.foo])
        scheme = OutputScheme([CogC(), Ext()])
        count = print_clang_style_warning([fileSummary], self.option, scheme, None)
        # Should include CogC value in warning
        self.assertIn("FILENAME:100: warning: foo has 1 NLOC, 30 CCN, 1 token, 0 PARAM, 1 length, 12 CogC, 10 ND\n",
                     sys.stdout.stream)
        self.assertEqual(1, count)

    def test_clang_format_warning_with_only_cogc(self):
        """Warning format when only CogC extension is loaded (no ND)"""
        self.foo.cyclomatic_complexity = 30
        self.foo.cognitive_complexity = 8
        fileSummary = FileInformation("FILENAME", 123, [self.foo])
        scheme = OutputScheme([CogC()])
        count = print_clang_style_warning([fileSummary], self.option, scheme, None)
        # Should include CogC but not ND
        self.assertIn("FILENAME:100: warning: foo has 1 NLOC, 30 CCN, 1 token, 0 PARAM, 1 length, 8 CogC\n",
                     sys.stdout.stream)
        self.assertEqual(1, count)


class TestFileInformationOutputWithCogC(StreamStdoutTestCase):
    """Test file information output when CogC extension is loaded"""

    def test_print_and_save_detail_information_with_cogc(self):
        """File summary should include CogC average when extension is loaded"""
        scheme = OutputScheme([CogC()])
        scheme.patch_for_extensions()
        fileSummary = FileInformation("FILENAME", 123, [])
        print_and_save_modules([fileSummary], scheme)
        # CogC average should be included
        self.assertIn("Avg.CogC", sys.stdout.stream)
        self.assertIn("    123       0.0     0.0        0.0       0.0         0     FILENAME",
                     sys.stdout.stream)

    def test_print_with_cogc_and_external_extension(self):
        """File summary with both CogC and external extension"""
        scheme = OutputScheme([CogC(), Ext()])
        scheme.patch_for_extensions()
        fileSummary = FileInformation("FILENAME", 123, [])
        print_and_save_modules([fileSummary], scheme)
        # Both Avg.ND and Avg.CogC should be included
        self.assertIn("Avg.ND", sys.stdout.stream)
        self.assertIn("Avg.CogC", sys.stdout.stream)
        self.assertIn("    123       0.0     0.0        0.0       0.0     0.0         0     FILENAME",
                     sys.stdout.stream)


if __name__ == '__main__':
    unittest.main()
