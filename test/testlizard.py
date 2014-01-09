#
# Unit Test
#
import unittest
import sys
from test.mock import patch
from lizard import FileAnalyzer, CLikeReader, mapFilesToAnalyzer, FunctionInfo, analyze_file



def analyzer_mock(filename):
    return filename

class Test_analyze_files(unittest.TestCase):
    def test_NoFiles(self):
        call_count = 0
        def analyzer(filename):
            call_count += 1
        mapFilesToAnalyzer([], analyzer, 1)
        self.assertEqual(0, call_count)

    def test_NoFilesMultipleThread(self):
        call_count = 0
        def analyzer(filename):
            call_count += 1
        mapFilesToAnalyzer([], analyzer, 2)
        self.assertEqual(0, call_count)
        
    def test_OneFile(self):
        analyzer = analyzer_mock
        r = mapFilesToAnalyzer(["filename"], analyzer, 1)
        self.assertEqual(["filename"], [x for x in r])
        
    def test_OneFileMultipleThread(self):
        analyzer = analyzer_mock
        r = mapFilesToAnalyzer(["filename"], analyzer, 2)
        self.assertEqual(["filename"], [x for x in r])
    
    def test_MoreFiles(self):
        analyzer = analyzer_mock
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 1)
        self.assertEqual(["f1", "f2"], [x for x in r])

    def test_MoreFilesMultipleThread(self):
        analyzer = analyzer_mock
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 2)
        self.assertEqual(["f1", "f2"], [x for x in r])


@patch('lizard.open', create=True)
class Test_FileAnalyzer(unittest.TestCase):
    
    def setUp(self):
        self.analyzer = FileAnalyzer()
        
    def test_analyze_c_file(self, mock_open):
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        self.assertEqual(1, len([x for x in r]))
        
    def test_analyze_c_file_with_multiple_thread(self, mock_open):
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 2)
        self.assertEqual(1, len([x for x in r]))
    
    def test_fileInfomation(self, mock_open):
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        fileInfo = list(r)[0]
        self.assertEqual(1, fileInfo.nloc)
        self.assertEqual(2, fileInfo.average_NLOC)
        self.assertEqual(1, fileInfo.average_CCN)
        self.assertEqual(9, fileInfo.average_token)

    @patch.object(sys, 'stderr')
    @patch.object(CLikeReader, '_GLOBAL')
    def test_should_report_when_having_problem_parsing_the_source(self, mock_GLOBAL, mock_stderr, mock_open):
        def fake_reader(token):
            if token == "exception":
                raise KeyError("")
        file_handle = mock_open.return_value.__enter__.return_value
        mock_GLOBAL.side_effect = fake_reader
        file_handle.read.return_value = '''line1
                                           line2
                                           exception lala
                                           line4'''
        analyze_file("f1.c")
        self.assertEqual(1, mock_stderr.write.call_count)
        error_message = mock_stderr.write.call_args[0][0]
        self.assertIn("f1.c", error_message)
        self.assertIn("3", error_message)
        self.assertIn("exception lala", error_message)
        self.assertIn("terry.yinzhe@gmail.com", error_message)
        self.assertIn("https://github.com/terryyin/lizard", error_message)
        
    @patch.object(sys, 'stderr')
    def test_should_report_when_having_other_problem_and_continue(self, mock_stderr, mock_open):
        mock_open.side_effect = IOError("[Errno 2] No such file or directory")
        analyze_file("f1.c")
        self.assertEqual(1, mock_stderr.write.call_count)
        error_message = mock_stderr.write.call_args[0][0]
        self.assertIn("[Errno 2]", error_message)
        self.assertIn("terry.yinzhe@gmail.com", error_message)
        
         
class Test_FunctionInfo(unittest.TestCase):
    def test_FunctionInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(FunctionInfo("a", 1))

from lizard import FilesFilter
import os

class Test_Exclude_Patterns(unittest.TestCase):
    
    def getSourceFiles(self, SRC_DIRs, exclude_patterns, check_duplicates = False):
        return FilesFilter(exclude_patterns, check_duplicates).getFileNames(SRC_DIRs)
    
    @patch.object(os, "walk")
    def test_no_matching(self, mock_os_walk):
        mock_os_walk.return_value = []
        files = self.getSourceFiles(["dir"], [])
        self.assertEqual(0, len(list(files)))
    
    @patch.object(os.path, "isfile")
    def test_explicit_file_names(self, mock_isfile):
        mock_isfile.return_value = True
        files = self.getSourceFiles(["dir/file.c"], [])
        self.assertEqual(["dir/file.c"], list(files))
    
    @patch.object(os.path, "isfile")
    def test_specific_filenames_should_not_be_excluded(self, mock_isfile):
        mock_isfile.return_value = True
        files = self.getSourceFiles(["dir/file.log"], [])
        self.assertEqual(["dir/file.log"], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_file_name(self, mock_os_walk):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['temp.log', 'useful.cpp']],)
        files = self.getSourceFiles(["dir"], ["*.log"])
        self.assertEqual(["./useful.cpp"], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_folder(self, mock_os_walk):
        mock_os_walk.return_value = (['ut', 
                                      None,
                                      ['useful.cpp']],)
        files = self.getSourceFiles(["dir"], ["ut/*"])
        self.assertEqual([], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_folder_recursively(self, mock_os_walk):
        mock_os_walk.return_value = (['ut/something', 
                                      None,
                                      ['useful.cpp']],)
        files = self.getSourceFiles(["dir"], ["ut/*"])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_none_supported_files(self, mock_os_walk):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['useful.txt']],)
        files = self.getSourceFiles(["dir"],['exclude_me'])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    @patch("lizard.open", create=True)
    def test_duplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        files = self.getSourceFiles(["dir"], [], True)
        self.assertEqual(['./f1.cpp'], list(files))

    @patch.object(os, "walk")
    @patch("lizard.open", create=True)
    def test_nonduplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        outs = ["int foo(){{haha({param});\n}}".format(param=i) for i in range(2)]
        file_handle.read.side_effect = lambda: outs.pop()
        files = self.getSourceFiles(["dir"], [], True)
        self.assertEqual(["./f1.cpp", "./f2.cpp"], list(files))

from lizard import LanguageChooser
class TestLanguageChooser(unittest.TestCase):
    
    def test_not_case_sensitive(self):
        self.assertEqual("c/c++", LanguageChooser().get_language_by_filename("a.Cpp"));
    
    def test_java(self):
        self.assertEqual("Java", LanguageChooser().get_language_by_filename("a.java"));
    
    def test_objectiveC(self):
        self.assertEqual("objC", LanguageChooser().get_language_by_filename("a.m"));
    
    def test_c_cpp(self):
        for name in ("a.cpp", ".cxx", ".h", ".hpp"):
            self.assertEqual("c/c++", LanguageChooser().get_language_by_filename(name),
                             "File name '%s' is not recognized as c/c++ file" % name);
    
