import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_ext import JavaScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_tokenizing_JavaScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(JavaScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_tokenizing_javascript_regular_expression(self):
        self.check_tokens(['/ab/'], '/ab/')
        self.check_tokens([r'/\//'], r'/\//')
        self.check_tokens([r'/a/igm'], r'/a/igm')

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_tokenizing_javascript_regular_expression(self):
        self.check_tokens(['a', '=', '/ab/'], 'a=/ab/')

    def test_tokenizing_javascript_comments(self):
        self.check_tokens(['/**a/*/'], '''/**a/*/''')

class Test_parser_for_JavaScript(unittest.TestCase):

    def test_simple_function(self):
        functions = get_js_function_list("function foo(){}")
        self.assertEqual("foo", functions[0].name)

    def test_simple_function_complexity(self):
        functions = get_js_function_list("function foo(){m;if(a);}")
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_parameter_count(self):
        functions = get_js_function_list("function foo(a, b){}")
        self.assertEqual(2, functions[0].parameter_count)

    def test_function_assigning_to_a_name(self):
        functions = get_js_function_list("a = function (a, b){}")
        self.assertEqual('a', functions[0].name)

    def test_not_a_function_assigning_to_a_name(self):
        functions = get_js_function_list("abc=3; function (a, b){}")
        self.assertEqual('function', functions[0].name)

    def test_function_without_name_assign_to_field(self):
        functions = get_js_function_list("a.b.c = function (a, b){}")
        self.assertEqual('a.b.c', functions[0].name)

    def test_function_in_a_object(self):
        functions = get_js_function_list("var App={a:function(){};}")
        self.assertEqual('a', functions[0].name)

    def test_function_in_a_function(self):
        functions = get_js_function_list("function a(){function b(){}}")
        self.assertEqual('b', functions[0].name)
        self.assertEqual('a', functions[1].name)

    def test_global(self):
        functions = get_js_function_list("{}")
        self.assertEqual(0, len(functions))

