import unittest
from mock import Mock
import sys
import hfcca
from hfcca import print_warnings, print_and_save_detail_information, FunctionInfo, FileInformation

class StreamForTest:
    
    def __init__(self):
        self.stream = ""
        
    def write(self, x):
        self.stream += str(x)
        
    def __getattr__(self, attr):
        return getattr(self.stream, attr)    

class TestWarningOutput(unittest.TestCase):
    
    def setUp(self):
        self.savedStdout = sys.stdout 
        sys.stdout = StreamForTest()
    
    def tearDown(self):
        sys.stdout = self.savedStdout
        
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
        fileStat = FileInformation("FILENAME")
        fileStat.append(fun)
        option = Mock(CCN=15)
        print_warnings(option, [fileStat])
        self.assertIn("FILENAME:100: warning: foo has 16 CCN and 0 params (0 NLOC, 0 tokens)\n", sys.stdout.stream)


class TestFileOutput(unittest.TestCase):
    
    def setUp(self):
        self.savedStdout = sys.stdout 
        sys.stdout = StreamForTest()
    
    def tearDown(self):
        sys.stdout = self.savedStdout
        
    def test_print_and_save_detail_information(self):
        fileSummary = FileInformation("FILENAME")
        fileSummary.summarize(123)
        print_and_save_detail_information([fileSummary], Mock(warnings_only=False))
        self.assertIn("    123      0      0         0         0     FILENAME", sys.stdout.stream)
        
        