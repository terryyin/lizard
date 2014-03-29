import unittest
from javascript import JavaScriptReader
from lizard import  analyze_file, FileAnalyzer, get_extensions

def get_js_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.js", source_code)

def get_js_function_list(source_code):
    return get_js_fileinfo(source_code).function_list

class Test_parser_for_JavaScript(unittest.TestCase):

    def test_simple_function(self):
        functions = get_js_function_list("function foo(){}")
        self.assertEqual("foo", functions[0].name)

    def test_parameter_count(self):
        functions = get_js_function_list("function foo(a, b){}")
        self.assertEqual(2, functions[0].parameter_count)

