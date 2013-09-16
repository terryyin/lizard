import unittest
from mock import Mock, patch
import sys
from lizard import print_warnings, print_and_save_detail_information, FunctionInfo, FileInformation,\
    print_result, XMLFormatter

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

class TestWarningOutput(StreamStdoutTestCase):
        
    def test_should_have_header_when_warning_only_is_off(self):
        option = Mock(warnings_only=False, CCN=15)
        print_warnings(option, [])
        self.assertIn("Warnings (CCN > 15)", sys.stdout.stream)

    def test_should_say_no_warning_when_warning_only_is_off(self):
        option = Mock(warnings_only=False, CCN=15)
        print_warnings(option, [])
        self.assertIn("No warning found. Excellent!\n", sys.stdout.stream)

    def test_should_not_have_header_when_warning_only_is_on(self):
        option = Mock(warnings_only=True, CCN=15)
        print_warnings(option, [])
        self.assertNotIn("Warnings (CCN > 15)", sys.stdout.stream)

    def test_should_use_clang_format_for_warning(self):
        fun = FunctionInfo("foo", 100)
        fun.cyclomatic_complexity = 16
        fileStat = FileInformation("FILENAME", 1, [fun])
        option = Mock(CCN=15)
        print_warnings(option, [fileStat])
        self.assertIn("FILENAME:100: warning: foo has 16 CCN and 0 params (0 NLOC, 0 tokens)\n", sys.stdout.stream)


class TestFileOutput(StreamStdoutTestCase):
    
    def test_print_and_save_detail_information(self):
        fileSummary = FileInformation("FILENAME", 123, [])
        print_and_save_detail_information([fileSummary], Mock(warnings_only=False))
        self.assertIn("    123      0      0         0         0     FILENAME", sys.stdout.stream)

    
class TestAllOutput(StreamStdoutTestCase):
        
    @patch.object(sys, 'exit')
    def test_print_result(self, mock_exit):
        file_infos = [FileInformation('f1.c', 1, []), FileInformation('f2.c', 1, [])]
        option = Mock(CCN=15, number = 0)
        print_result(file_infos, option)
        self.assertEqual(0, mock_exit.call_count)

    @patch.object(sys, 'exit')
    def test_exit_with_non_zero_when_more_warning_than_ignored_number(self, mock_exit):
        fun = FunctionInfo("foo", 100)
        fun.cyclomatic_complexity = 16
        file_infos = [FileInformation('f1.c', 1, [fun])]
        option = Mock(CCN=15, number = 0)
        print_result(file_infos, option)
        mock_exit.has_called_with(1)
        
class TestXMLOutput(unittest.TestCase):
    
    def test_xml_output(self):
        fun = FunctionInfo("foo", 100)
        fun.cyclomatic_complexity = 16
        file_infos = [FileInformation('f1.c', 1, [fun])]
        option = Mock(CCN=15, number = 0)
        xml = XMLFormatter().xml_output(file_infos, option)
        self.assertIn('''<item name="foo at f1.c:100">''', xml)
