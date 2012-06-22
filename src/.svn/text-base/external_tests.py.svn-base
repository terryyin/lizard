'''
Created on Mar 6, 2011

@author: terry
'''
import unittest
from hfcca import *

class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass

    def create_c_hfcca(self, source_code, preprocessor=DefaultPreprocessor):
        return analyze_source_code_with_parser(source_code, preprocessor, "", CTokenTranslator)
    def test_define_with_space(self):
        result = self.create_c_hfcca("# define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }")
        self.assertEqual(0, len(result))
    def test_double_p(self):
        result = self.create_c_hfcca("static void yyExit ARGS ((void)) { rExit (1); }")
        self.assertEqual(1, len(result))
    def test_preprocessing(self):
        code =    '''
            static void yyCheckMemory
            # ifdef HAVE_ARGS
               (char * ptr)
            # else
               (ptr) char * ptr;
            # endif
            {
              if (! (ptr)) Parser_Exit ();
            }
            '''
        result = analyze_source_code_with_parser(code, CPreprocessor, "", CTokenTranslator)
        self.assertEqual(1, len(result))
    def test_line_counting(self):
        result = analyze_source_file("../example_code/httpd.c", True)
        self.assertEqual(171, result[3].start_line)
    def test_for_loop(self):
        result = analyze_source_file("../example_code/for.c", True)
        self.assertEqual(858, result[3].start_line)
        
class Test_file_hfcca(unittest.TestCase):
    def test_full(self):
        result = analyze_source_file("../example_code/httpd.c")
        self.assertEqual("../example_code/httpd.c", result.filename)
        self.assertEqual(16, len(result))

class Test_files_hfcca(unittest.TestCase):
    def test_(self):
        result = [x for x in analyze(["../example_code/"], [])]
        self.assertEqual(3, len(result))
    def xtest_multiple_thread(self):
        result = [x for x in analyze(["../example_code/"], [], None, 4)]
        self.assertEqual(20, len(result))

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()