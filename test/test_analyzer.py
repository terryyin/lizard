#
# Unit Test
#
import unittest
import sys
from mock import patch, Mock
from lizard import CLikeReader, map_files_to_analyzer, FunctionInfo, analyze_file, CodeInfoContext


def analyzer_mock(filename):
    return filename

class Test_analyze_files(unittest.TestCase):
    def test_NoFiles(self):
        call_count = 0
        def analyzer(filename):
            call_count += 1
        map_files_to_analyzer([], analyzer, 1)
        self.assertEqual(0, call_count)

    def test_NoFilesMultipleThread(self):
        call_count = 0
        def analyzer(filename):
            call_count += 1
        map_files_to_analyzer([], analyzer, 2)
        self.assertEqual(0, call_count)
        
    def test_OneFile(self):
        analyzer = analyzer_mock
        r = map_files_to_analyzer(["filename"], analyzer, 1)
        self.assertEqual(["filename"], [x for x in r])
        
    def test_OneFileMultipleThread(self):
        analyzer = analyzer_mock
        r = map_files_to_analyzer(["filename"], analyzer, 2)
        self.assertEqual(["filename"], [x for x in r])
    
    def test_MoreFiles(self):
        analyzer = analyzer_mock
        r = map_files_to_analyzer(["f1", "f2"], analyzer, 1)
        self.assertEqual(["f1", "f2"], [x for x in r])

    def test_MoreFilesMultipleThread(self):
        analyzer = analyzer_mock
        r = map_files_to_analyzer(["f1", "f2"], analyzer, 2)
        self.assertSetEqual(set(["f1", "f2"]), set(x for x in r))


@patch('lizard.open', create=True)
class Test_FileAnalyzer(unittest.TestCase):
    
    def setUp(self):
        self.analyzer = analyze_file
        
    def test_analyze_c_file(self, mock_open):
        file_handle = mock_open.return_value.read.return_value = "int foo(){haha();\n}"
        r = map_files_to_analyzer(["f1.c"], self.analyzer, 1)
        self.assertEqual(1, len([x for x in r]))
        
    def test_analyze_c_file_with_multiple_thread(self, mock_open):
        file_handle = mock_open.return_value.read.return_value = "int foo(){haha();\n}"
        r = map_files_to_analyzer(["f1.c"], self.analyzer, 2)
        self.assertEqual(1, len([x for x in r]))
    
    def test_fileInfomation(self, mock_open):
        mock_open.return_value.read.return_value = "int foo(){haha();\n}"
        r = map_files_to_analyzer(["f1.c"], self.analyzer, 1)
        fileInfo = list(r)[0]
        self.assertEqual(2, fileInfo.nloc)
        self.assertEqual(2, fileInfo.average_NLOC)
        self.assertEqual(1, fileInfo.average_CCN)
        self.assertEqual(9, fileInfo.average_token)

    @patch.object(sys, 'stderr')
    def test_should_report_when_having_other_problem_and_continue(self, mock_stderr, mock_open):
        mock_open.side_effect = IOError("[Errno 2] No such file or directory")
        analyze_file("f1.c")
        self.assertEqual(1, mock_stderr.write.call_count)
        error_message = mock_stderr.write.call_args[0][0]
        self.assertEqual("Error: Fail to read source file 'f1.c'\n", error_message)

class Test_Picklability(unittest.TestCase):

    def test_FunctionInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(FunctionInfo("a", '', 1))

    def test_FileInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(CodeInfoContext("a"))


from lizard import warning_filter, FileInformation, whitelist_filter

class TestWarningFilter(unittest.TestCase):

    def setUp(self):
        complex_fun = FunctionInfo("complex", '', 100)
        complex_fun.cyclomatic_complexity = 16
        simple_fun = FunctionInfo("simple", '', 100)
        simple_fun.cyclomatic_complexity = 15
        self.fileStat = FileInformation("FILENAME", 1, [complex_fun, simple_fun])

    def test_should_filter_the_warnings(self):
        option = Mock(CCN=15, arguments=10)
        warnings = list(warning_filter(option, [self.fileStat]))
        self.assertEqual(1, len(warnings))
        self.assertEqual("complex", warnings[0].name)

class TestWarningFilterWithWhitelist(unittest.TestCase):

    WARNINGS = [FunctionInfo("foo", 'filename'),
             FunctionInfo("bar", 'filename'),
             FunctionInfo("foo", 'anotherfile')]

    def test_should_filter_out_the_whitelist(self):
        warnings = whitelist_filter(self.WARNINGS, "foo")
        self.assertEqual(1, len(list(warnings)))

    def test_should_filter_function_in_the_right_file_when_specified(self):
        warnings = whitelist_filter(self.WARNINGS, 'filename:foo')
        self.assertEqual(2, len(list(warnings)))

    def test_should_work_with_class_member(self):
        warnings = whitelist_filter([FunctionInfo("class::foo", 'filename')], 'class::foo')
        self.assertEqual(0, len(list(warnings)))

    def test_should_filter_mutiple_functions_defined_on_one_line(self):
        warnings = whitelist_filter(self.WARNINGS, 'foo, bar')
        self.assertEqual(0, len(list(warnings)))

    def test_should_filter_mutiple_lines_of_whitelist(self):
        warnings = whitelist_filter(self.WARNINGS, 'foo\n bar')
        self.assertEqual(0, len(list(warnings)))

    def test_should_ignore_comments_in_whitelist(self):
        warnings = whitelist_filter(self.WARNINGS, 'foo  #,bar\ni#,bar')
        self.assertEqual(1, len(list(warnings)))


