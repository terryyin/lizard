import unittest
from test.mock import Mock, patch
import sys
import os
from lizard import print_warnings, print_and_save_modules, FunctionInfo, FileInformation,\
    print_result, get_extensions, OutputScheme
from lizard_ext import xml_output

class StreamStdoutTestCase(unittest.TestCase):
    def setUp(self):
        self.savedStdout = sys.stdout 
        sys.stdout = self.StreamForTest()

    def tearDown(self):
        sys.stdout = self.savedStdout

    class StreamForTest:

        def __init__(self):
            self.stream = ""

        def write(self, x):
            self.stream += str(x)

        def __getattr__(self, attr):
            return getattr(self.stream, attr)

class TestFunctionOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.extensions = get_extensions([])
        self.scheme = OutputScheme(self.extensions)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)

    def test_function_info_header_should_have_a_box(self):
        print_and_save_modules([], self.extensions, self.scheme)
        self.assertIn("=" * 20, sys.stdout.stream.splitlines()[0])

    def test_function_info_header_should_have_the_captions(self):
        print_and_save_modules([], self.extensions, self.scheme)
        self.assertEquals("  NLOC    CCN   token  PARAM  location  ", sys.stdout.stream.splitlines()[1])

    def test_function_info_header_should_have_the_captions_of_external_extensions(self):
        external_extension = Mock(FUNCTION_CAPTION = "*external_extension*", ordering_index=-1)
        extensions = get_extensions([external_extension])
        scheme = OutputScheme(extensions)
        print_and_save_modules([], extensions, scheme)
        self.assertEquals("  NLOC    CCN   token  PARAM *external_extension* location  ", sys.stdout.stream.splitlines()[1])

    def test_print_fileinfo(self):
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        fileStat = FileInformation("FILENAME", 1, [self.foo])
        print_and_save_modules([fileStat], self.extensions, self.scheme)
        self.assertEquals("       1     16      1      0 foo@100-100@FILENAME", sys.stdout.stream.splitlines()[3])

class TestWarningOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = Mock(warnings_only=False, CCN=15, extensions = [])
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.scheme = Mock()

    def test_should_have_header_when_warning_only_is_off(self):
        print_warnings(self.option, self.scheme, [])
        self.assertIn("Warnings (CCN > 15)", sys.stdout.stream)

    def test_no_news_is_good_news(self):
        self.option.warnings_only = True
        print_warnings(self.option, self.scheme, [])
        self.assertEqual('', sys.stdout.stream)

    def test_should_not_have_header_when_warning_only_is_on(self):
        self.option = Mock(warnings_only=True, CCN=15)
        print_warnings(self.option, self.scheme, [])
        self.assertNotIn("Warnings (CCN > 15)", sys.stdout.stream)

    def test_should_use_clang_format_for_warning(self):
        self.option = Mock(display_fn_end_line = False, extensions = get_extensions([]))
        print_warnings(self.option, self.scheme, [self.foo])
        self.assertIn("FILENAME:100: warning: foo has 1 CCN and 0 params (1 NLOC, 1 tokens)\n", sys.stdout.stream)

    def test_sort_warning(self):
        self.option.sorting = ['cyclomatic_complexity']
        self.foo.cyclomatic_complexity = 10
        bar = FunctionInfo("bar", '', 100)
        bar.cyclomatic_complexity = 15
        print_warnings(self.option, self.scheme, [self.foo, bar])
        self.assertEqual('bar', self.scheme.function_info.call_args_list[0][0][0].name)

    def test_sort_warning_with_generator(self):
        self.option.sorting = ['cyclomatic_complexity']
        print_warnings(self.option, self.scheme, (x for x in []))


class TestFileOutput(StreamStdoutTestCase):

    def test_print_and_save_detail_information(self):
        fileSummary = FileInformation("FILENAME", 123, [])
        print_and_save_modules([fileSummary], [], Mock())
        self.assertIn("    123      0    0.0         0         0     FILENAME", sys.stdout.stream)

    def test_print_file_summary_only_once(self):
        print_and_save_modules(
                            [FileInformation("FILENAME1", 123, []), 
                             FileInformation("FILENAME2", 123, [])], [], Mock())
        self.assertEqual(1, sys.stdout.stream.count("FILENAME1"))


class TestAllOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)

    def test_print_extension_results(self):
        file_infos = []
        extension = Mock()
        option = Mock(CCN=15, number = 0, extensions = [extension], whitelist='')
        print_result(file_infos, option)
        self.assertEqual(1, extension.print_result.call_count)

    def test_should_not_print_extension_results_when_not_implemented(self):
        file_infos = []
        option = Mock(CCN=15, number = 0, extensions = [object()], whitelist='')
        print_result(file_infos, option)

    @patch.object(sys, 'exit')
    def test_print_result(self, mock_exit):
        file_infos = [FileInformation('f1.c', 1, []), FileInformation('f2.c', 1, [])]
        option = Mock(CCN=15, number = 0, extensions=[], whitelist='')
        print_result(file_infos, option)
        self.assertEqual(0, mock_exit.call_count)

    @patch.object(os.path, 'isfile')
    @patch('lizard.open', create=True)
    def check_whitelist(self, script, mock_open, mock_isfile):
        mock_isfile.return_value = True
        mock_open.return_value.read.return_value = script
        file_infos = [FileInformation('f1.c', 1, [self.foo])]
        option = Mock(CCN=15, number = 0, arguments=100, extensions=[])
        print_result(file_infos, option)

    @patch.object(sys, 'exit')
    def test_exit_with_non_zero_when_more_warning_than_ignored_number(self, mock_exit):
        self.foo.cyclomatic_complexity = 16
        self.check_whitelist('')
        mock_exit.assert_called_with(1)

    @patch.object(sys, 'exit')
    def test_whitelist(self, mock_exit):
        self.foo.cyclomatic_complexity = 16
        self.check_whitelist('foo')
        self.assertEqual(0, mock_exit.call_count)

    def test_null_result(self):
        self.check_whitelist('')


import xml.etree.ElementTree as ET
class TestXMLOutput(unittest.TestCase):
    foo = FunctionInfo("foo", '', 100)
    foo.cyclomatic_complexity = 16
    file_infos = [FileInformation('f1.c', 1, [foo])]
    xml = xml_output(file_infos, True)

    def test_xml_output(self):
        root = ET.fromstring(self.xml)
        item = root.findall('''./measure[@type="Function"]/item[0]''')[0]
        self.assertEqual('''foo at f1.c:100''', item.get("name"))

    def test_xml_stylesheet(self):
        self.assertIn('''<?xml-stylesheet type="text/xsl" href="https://raw.github.com/terryyin/lizard/master/lizard.xsl"?>''', self.xml)
