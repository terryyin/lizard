import unittest
import sys
from mock import Mock, patch
from test.helper_stream import StreamStdoutTestCase
import os
from lizard import print_warnings, FunctionInfo, FileInformation,\
    print_result, print_extension_results, get_extensions, OutputScheme, get_warnings, print_clang_style_warning,\
    parse_args, AllResult
from lizard_ext import xml_output
from lizard_ext.checkstyleoutput import checkstyle_output


def print_result_with_scheme(result, option):
    return print_result(result, option, OutputScheme(option.extensions), AllResult)


class TestFunctionOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.extensions = get_extensions([])
        self.scheme = OutputScheme(self.extensions)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.option = parse_args("app")

    def test_function_info_header_should_have_a_box(self):
        print_result_with_scheme([], self.option)
        self.assertIn("=" * 20, sys.stdout.stream.splitlines()[0])

    def test_function_info_header_should_have_the_captions(self):
        print_result_with_scheme([], self.option)
        self.assertEqual("  NLOC    CCN   token  PARAM  length  location  ", sys.stdout.stream.splitlines()[1])

    def test_function_info_header_should_have_the_captions_of_external_extensions(self):
        external_extension = Mock(FUNCTION_INFO = {"xx": {"caption": "*external_extension*"}}, ordering_index=-1)
        extensions = get_extensions([external_extension])
        scheme = OutputScheme(extensions)
        print_result([], self.option,  scheme, AllResult)
        self.assertEqual("  NLOC    CCN   token  PARAM  length *external_extension* location  ", sys.stdout.stream.splitlines()[1])

    def test_print_fileinfo(self):
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        fileStat = FileInformation("FILENAME", 1, [self.foo])
        print_result_with_scheme([fileStat], self.option)
        self.assertEqual("       1     16      1      0       1 foo@100-100@FILENAME", sys.stdout.stream.splitlines()[3])

    def test_print_result_short_no_warning(self):
        """Test that fileinfo is not printed if `short` option is specified"""
        option = parse_args("app")
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 14
        option = parse_args("app")
        option.CCN = 15
        option.short = True
        filename = "FILENAME"
        fileStat = FileInformation(filename, 1, [self.foo])
        result = print_result_with_scheme([fileStat], option)

        # Test that there is NO filename ("FILENAME") in output as complexity WAS NOT exceeded
        # and we used `--short option`
        # If short option wouldn't be used FILENAME should occur 2 times (1 for functions, 1 for file)
        filename_found_cnt = 0
        for line in sys.stdout.stream.splitlines():
            if filename in line:
                filename_found_cnt += 1
        self.assertEqual(0, filename_found_cnt)

    def test_print_result_short_warning(self):
        """Test that fileinfo is not printed if `short` option is specified"""
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        option = parse_args("app")
        option.CCN = 15
        option.short = True
        filename = "FILENAME"
        fileStat = FileInformation(filename, 1, [self.foo])
        result = print_result_with_scheme([fileStat], option)

        # Test that there is ONLY ONE filename ("FILENAME") in output as complexity WAS exceeded
        # and we used `--short option`
        # If short option wouldn't be used FILENAME should occur 3 times (1 for functions, 1 for file, 1 for warning)
        filename_found_cnt = 0
        for line in sys.stdout.stream.splitlines():
            if filename in line:
                filename_found_cnt += 1
        self.assertEqual(1, filename_found_cnt)


class Ext(object):
    FUNCTION_INFO = {"max_nesting_depth": {"caption": "  ND  ", "average_caption": " Avg.ND "}}


class TestWarningOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.scheme = Mock()

    def test_should_have_header_when_warning_only_is_off(self):
        print_warnings(self.option, self.scheme, [])
        self.assertIn("cyclomatic_complexity > 15", sys.stdout.stream)

    def test_no_news_is_good_news(self):
        count = print_clang_style_warning([], self.option, None, None)
        self.assertEqual('', sys.stdout.stream)
        self.assertEqual(0, count)

    def test_should_use_clang_format_for_warning(self):
        self.foo.cyclomatic_complexity = 30
        self.foo.max_nesting_depth = 10
        fileSummary = FileInformation("FILENAME", 123, [self.foo])
        scheme = OutputScheme([Ext()])
        count = print_clang_style_warning([fileSummary], self.option, scheme, None)
        self.assertIn("FILENAME:100: warning: foo has 1 NLOC, 30 CCN, 1 token, 0 PARAM, 1 length, 10 ND\n", sys.stdout.stream)
        self.assertEqual(1, count)

    def test_sort_warning(self):
        self.option.sorting = ['cyclomatic_complexity']
        self.foo.cyclomatic_complexity = 30
        bar = FunctionInfo("bar", '', 100)
        bar.cyclomatic_complexity = 40
        fileSummary = FileInformation("FILENAME", 123, [self.foo, bar])
        warnings = get_warnings([fileSummary], self.option)
        self.assertEqual('bar', warnings[0].name)

    def test_sort_warning_with_generator(self):
        self.option.sorting = ['cyclomatic_complexity']
        print_warnings(self.option, self.scheme, (x for x in []))

    def test_warning_when_max_nesting_depth_missing(self):
        self.foo.cyclomatic_complexity = 30
        # Intentionally not setting max_nesting_depth
        fileSummary = FileInformation("FILENAME", 123, [self.foo])
        scheme = OutputScheme([Ext()])
        count = print_clang_style_warning([fileSummary], self.option, scheme, None)
        self.assertIn("FILENAME:100: warning: foo has 1 NLOC, 30 CCN, 1 token, 0 PARAM, 1 length", sys.stdout.stream)
        self.assertEqual(1, count)


class TestFileInformationOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")

    def test_print_and_save_detail_information(self):
        scheme = OutputScheme([])
        fileSummary = FileInformation("FILENAME", 123, [])
        print_result([fileSummary], self.option,  scheme, AllResult)
        self.assertIn("    123       0.0     0.0        0.0         0     FILENAME\n", sys.stdout.stream)

    def test_print_and_save_detail_information_with_ext(self):
        scheme = OutputScheme([Ext()])
        fileSummary = FileInformation("FILENAME", 123, [])
        print_result([fileSummary], self.option,  scheme, AllResult)
        self.assertIn("Avg.ND", sys.stdout.stream)
        self.assertIn("    123       0.0     0.0        0.0     0.0         0     FILENAME", sys.stdout.stream)

    def test_print_file_summary_only_once(self):
        scheme = OutputScheme([])
        print_result([FileInformation("FILENAME1", 123, []),
                      FileInformation("FILENAME2", 123, [])],
                     self.option,
                     scheme,
                     AllResult)
        self.assertEqual(1, sys.stdout.stream.count("FILENAME1"))


class TestAllOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)

    def test_print_extension_results(self):
        extension = Mock(FUNCTION_INFO = {})
        print_extension_results([extension])
        self.assertEqual(1, extension.print_result.call_count)

    def test_should_not_print_extension_results_when_not_implemented(self):
        file_infos = []
        option = Mock(CCN=15, number = 0, thresholds={}, extensions = [object()], whitelist='')
        print_result_with_scheme(file_infos, option)

    def test_print_result(self):
        file_infos = [FileInformation('f1.c', 1, []), FileInformation('f2.c', 1, [])]
        option = Mock(CCN=15, thresholds={}, number=0, extensions=[], whitelist='')
        self.assertEqual(0, print_result_with_scheme(file_infos, option))

    @patch.object(os.path, 'isfile')
    @patch('lizard.open', create=True)
    def check_whitelist(self, script, mock_open, mock_isfile):
        mock_isfile.return_value = True
        mock_open.return_value.read.return_value = script
        file_infos = [FileInformation('f1.c', 1, [self.foo])]
        option = Mock(thresholds={'cyclomatic_complexity': 15, 'length': 1000}, CCN=15, number = 0, arguments=100, length=1000, extensions=[])
        return print_result_with_scheme(file_infos, option)

    def test_exit_with_non_zero_when_more_warning_than_ignored_number(self):
        self.foo.cyclomatic_complexity = 16
        self.assertEqual(1, self.check_whitelist(''))

    def test_whitelist(self):
        self.foo.cyclomatic_complexity = 16
        self.check_whitelist('foo')
        self.assertEqual(0, self.check_whitelist('foo'))

    def test_null_result(self):
        self.check_whitelist('')


class TestXMLOutput(unittest.TestCase):
    foo = FunctionInfo("foo", '', 100)
    foo.cyclomatic_complexity = 16
    file_infos = [FileInformation('f1.c', 1, [foo])]
    xml = xml_output(AllResult(file_infos), True)

    def test_xml_output(self):
        self.assertIn('''foo at f1.c:100''', self.xml)

    def test_xml_stylesheet(self):
        self.assertIn('''<?xml-stylesheet type="text/xsl" href="https://raw.githubusercontent.com/terryyin/lizard/master/lizard.xsl"?>''', self.xml)

    def test_xml_output_on_empty_folder(self):
        xml_empty = xml_output(AllResult([]), True)
        self.assertIn('''<sum label="NCSS" value="0"/>''', xml_empty)
        self.assertIn('''<sum label="CCN" value="0"/>''', xml_empty)
        self.assertIn('''<sum label="Functions" value="0"/>''', xml_empty)


class TestCheckstyleOutput(unittest.TestCase):
    foo = FunctionInfo("foo", '', 100)
    foo.cyclomatic_complexity = 16
    file_infos = [FileInformation('f1.c', 1, [foo])]
    from lizard_ext.checkstyleoutput import checkstyle_output
    checkstyle_xml = checkstyle_output(AllResult(file_infos), True)

    def test_checkstyle_output(self):
        self.assertIn('<checkstyle', self.checkstyle_xml)
        self.assertIn('<file', self.checkstyle_xml)
        self.assertIn('<error', self.checkstyle_xml)
        self.assertIn('foo has', self.checkstyle_xml)

    def test_checkstyle_output_on_empty_folder(self):
        xml_empty = checkstyle_output(AllResult([]), True)
        self.assertIn('<checkstyle', xml_empty)
