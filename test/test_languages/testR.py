import os
import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions

class Test_R_Tokenizing(unittest.TestCase):
    def tokens(self, source):
        from lizard_languages import RReader
        return list(RReader.generate_tokens(source))

    def test_comment_and_division(self):
        self.assertEqual(['a', '/', 'b', ',', '123', ' ', '/', 'b'], self.tokens('a/b,123 /b'))
        self.assertEqual(['a', ' ', '# comment', '\n'], self.tokens('a # comment\n'))

class Test_R_Parsing(unittest.TestCase):
    def get_functions(self, code):
        return analyze_file.analyze_source_code('a.R', code).function_list

    def test_named_function_with_arrow(self):
        funcs = self.get_functions('f <- function(x, y) { x + y }')
        self.assertEqual(1, len(funcs))
        self.assertEqual('f', funcs[0].name)
        self.assertEqual(2, funcs[0].parameter_count)

    def test_named_function_with_equals(self):
        funcs = self.get_functions('g = function() { }')
        self.assertEqual(1, len(funcs))
        self.assertEqual('g', funcs[0].name)

    def test_anonymous_function(self):
        funcs = self.get_functions('lapply(xs, function(x) x)')
        self.assertEqual(1, len(funcs))
        self.assertEqual('(anonymous)', funcs[0].name)

    def test_nested_functions(self):
        code = 'outer <- function(a){ inner <- function(b){b}; inner }'
        funcs = self.get_functions(code)
        names = [f.name for f in funcs]
        self.assertIn('outer', names)
        self.assertIn('inner', names)

    def test_dotted_names(self):
        funcs = self.get_functions('S3.method.class <- function(x) {}')
        self.assertEqual(1, len(funcs))
        self.assertEqual('S3.method.class', funcs[0].name)

    def test_complexity_if_else_if(self):
        code = 'f<-function(x){ if (x) {} else if (x && y) {} else {} }'
        funcs = self.get_functions(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual(3, funcs[0].cyclomatic_complexity)

    def test_complexity_loops(self):
        code = 'f<-function(){ for(i in 1:10){} while(TRUE){} repeat { break } }'
        funcs = self.get_functions(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual(4, funcs[0].cyclomatic_complexity)