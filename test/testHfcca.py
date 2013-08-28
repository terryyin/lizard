#
# Unit Test
#
import unittest
from mock import patch
from hfcca import FileAnalyzer, UniversalAnalyzer, generate_tokens, ObjCTokenTranslator, generate_tokens_from_code, CTokenTranslator, mapFilesToAnalyzer, FunctionInfo

class Test_generate_tonken(unittest.TestCase):

    def test_empty_string(self):
        result = [t for t in generate_tokens_from_code("")]
        self.assertEqual(0, len(result))

    def test_with_one_return(self):
        result = [t for t in generate_tokens_from_code("\n")]
        self.assertEqual(1, len(result))

    def test_with_two_returns(self):
        result = [t for t in generate_tokens_from_code("\n\n")]
        self.assertEqual(1, len(result))

    def test_comment_count_as_a_token_when_generating(self):
        result = [t for t in generate_tokens_from_code("/**/")]
        self.assertEqual(1, len(result))

class Test_objc_hfcca(unittest.TestCase):
    def create_objc_hfcca(self, source_code):
        return UniversalAnalyzer().analyze(ObjCTokenTranslator().getFunctions(generate_tokens(source_code)) , "")
    def test_empty(self):
        result = self.create_objc_hfcca("")
        self.assertEqual(0, len(result))
    def test_no_function(self):
        result = self.create_objc_hfcca("#import <unistd.h>\n")
        self.assertEqual(0, len(result))
    def test_one_c_function(self):
        result = self.create_objc_hfcca("int fun(int a, int b) const{}")
        self.assertTrue("fun" in result)
    def test_one_objc_function(self):
        result = self.create_objc_hfcca("-(void) foo {}")
        self.assertTrue("foo" in result)
    def test_one_objc_function_with_param(self):
        result = self.create_objc_hfcca("-(void) replaceScene: (CCScene*) scene {}")
        self.assertEqual("replaceScene:", result[0].name)
        self.assertEqual("replaceScene:( CCScene * )", result[0].long_name())
    def test_one_objc_functio_nwith_two_param(self):
        result = self.create_objc_hfcca("- (BOOL)scanJSONObject:(id *)outObject error:(NSError **)outError {}")
        self.assertTrue("scanJSONObject: error:" in result)
        self.assertEqual("scanJSONObject:( id * ) error:( NSError ** )", result[0].long_name())
    def test_one_objc_function_with_three_param(self):
        result = self.create_objc_hfcca("- (id)initWithRequest:(NSURLRequest *)request delegate:(id <NSURLConnectionDelegate>)delegate startImmediately:(BOOL)startImmediately{}")
        self.assertEqual("initWithRequest: delegate: startImmediately:", result[0].name)
        self.assertEqual("initWithRequest:( NSURLRequest * ) delegate:( id < NSURLConnectionDelegate > ) startImmediately:( BOOL )", result[0].long_name())
    def test_implementation(self):
        code = """
            @implementation classname(xx)
            + (return_type)classMethod
            {
                if (failure){

                     //wefailed

                 }
            }
            - (return_type)instanceMethod
            {
                // implementation
            }
            @end
            """
        result = self.create_objc_hfcca(code)
        self.assertEqual(2, len(result))
        self.assertEqual("classMethod", result[0].name)
        
class Test_parser_token(unittest.TestCase):
    def get_tokens(self, source_code):
        return [x for x, l in generate_tokens(source_code)]
    def get_tokens_and_line(self, source_code):
        return [x for x in generate_tokens(source_code)]
    def test_empty(self):
        tokens = self.get_tokens("")
        self.assertEqual(0, len(tokens))
    def test_one_digit(self):
        tokens = self.get_tokens("1")
        self.assertEqual(['1'], tokens)
    def test_operators(self):
        tokens = self.get_tokens("-;")
        self.assertEqual(['-', ';'], tokens)
    def test_operators1(self):
        tokens = self.get_tokens("-=")
        self.assertEqual(['-='], tokens)
    def test_operators2(self):
        tokens = self.get_tokens(">=")
        self.assertEqual(['>='], tokens)

    def test_more(self):
        tokens = self.get_tokens("int a{}")
        self.assertEqual(['int', "a", "{", "}"], tokens)
    def test_or(self):
        tokens = self.get_tokens("||")
        self.assertEqual(['||'], tokens)
    def test_comment(self):
        tokens = self.get_tokens("/***\n**/")
        self.assertEqual([], tokens)
        tokens = self.get_tokens("//aaa\n")
        self.assertEqual(['\n'], tokens)
    def test_commentedComment(self):
        tokens = self.get_tokens(" /*/*/")
        self.assertEqual([], tokens)
    def test_string(self):
        tokens = self.get_tokens(r'""')
        self.assertEqual(['""'], tokens)
        tokens = self.get_tokens(r'"x\"xx")')
        self.assertEqual(['"x\\"xx"', ')'], tokens)
    def test_define(self):
        tokens = self.get_tokens('''#define xx()\
                                      abc
                                    int''')
        self.assertEqual(['\n', 'int'], tokens)
       
    def test_line_number(self):
        tokens = self.get_tokens_and_line(r'abc')
        self.assertEqual(('abc', 1), tokens[0])
    def test_line_number2(self):
        tokens = self.get_tokens_and_line('abc\ndef')
        self.assertTrue(('def', 2) in tokens)
    def test_with_mutiple_line_string(self):
        tokens = self.get_tokens_and_line('"sss\nsss" t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_cpp_comments(self):
        tokens = self.get_tokens_and_line('//abc\n t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_line_continuer_comments(self):
        tokens = self.get_tokens_and_line('#define a \\\nb\n t')
        self.assertTrue(('t', 3) in tokens)
    def test_define2(self):
        tokens = self.get_tokens_and_line(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
''')
        self.assertTrue(('t', 3) in tokens)
    def test_with_c_comments(self):
        tokens = self.get_tokens_and_line('/*abc\n*/ t')
        self.assertTrue(('t', 2) in tokens)

class MockFileAnalyzer(FileAnalyzer):
    def __init__(self):
        self.mockRecord = []
    def analyze(self, filename):
        return filename
class Test_analyze_files(unittest.TestCase):
    def test_NoFiles(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 1)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_NoFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 2)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_OneFile(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 1)
        self.assertEqual(["filename"], [x for x in r])
    def test_OneFileMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 2)
        self.assertEqual(["filename"], [x for x in r])
    def test_MoreFiles(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 1)
        self.assertEqual(["f1", "f2"], [x for x in r])
    def test_MoreFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 2)
        self.assertEqual(["f1", "f2"], [x for x in r])

class MockFile:
    def __init__(self):
        self.mockRecord = []
    def read(self):
        return "int foo(){haha();\n}"
    def close(self):
        pass
    
def mockOpen(name):
    return MockFile()

class Test_FileAnalyzer(unittest.TestCase):
    def setUp(self):
        self.analyzer = FileAnalyzer()
        self.analyzer.open = mockOpen
    def create_cpp_hfcca(self, source_code):
        return FileAnalyzer().analyze_source_code_with_parser(source_code, "", CTokenTranslator)
    def test_analyze_c_file(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        self.assertEqual(1, len([x for x in r]))
        
    def test_analyze_c_file_with_multiple_thread(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 2)
        self.assertEqual(1, len([x for x in r]))
    
    def test_fileInfomation(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        fileInfo = list(r)[0]
        self.assertEqual(1, fileInfo.average_NLOC)
        self.assertEqual(1, fileInfo.average_CCN)
        self.assertEqual(4, fileInfo.average_token)
        
class Test_FunctionInfo(unittest.TestCase):
    def test_FunctionInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(FunctionInfo("a", 1))

from hfcca import getSourceFiles
import os

class Test_Exclude_Patterns(unittest.TestCase):
    
    @patch.object(os, "walk")
    def test_no_matching(self, mock_os_walk):
        mock_os_walk.return_value = []
        files = getSourceFiles(["dir"], [])
        self.assertEqual(0, len(list(files)))
    
    @patch.object(os.path, "isfile")
    def test_explicit_file_names(self, mock_isfile):
        mock_isfile.return_value = True
        files = getSourceFiles(["dir/file.c"], [])
        self.assertEqual(["dir/file.c"], list(files))
    
    @patch.object(os.path, "isfile")
    def test_exlclude_explicit_file_names_doesnot_support(self, mock_isfile):
        mock_isfile.return_value = True
        files = getSourceFiles(["dir/file.log"], [])
        self.assertEqual([], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_file_name(self, mock_os_walk):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['temp.log', 'useful.cpp']],)
        files = getSourceFiles(["dir"], ["*.log"])
        self.assertEqual(["./useful.cpp"], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_folder(self, mock_os_walk):
        mock_os_walk.return_value = (['ut', 
                                      None,
                                      ['useful.cpp']],)
        files = getSourceFiles(["dir"], ["ut/*"])
        self.assertEqual([], list(files))
    
    @patch.object(os, "walk")
    def test_exclude_folder_recursively(self, mock_os_walk):
        mock_os_walk.return_value = (['ut/something', 
                                      None,
                                      ['useful.cpp']],)
        files = getSourceFiles(["dir"], ["ut/*"])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_none_supported_files(self, mock_os_walk):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['useful.txt']],)
        files = getSourceFiles(["dir"],['exclude_me'])
        self.assertEqual([], list(files))
    
