#!/usr/bin/env python
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


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
