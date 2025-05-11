import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import TypeScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_tokenizing_JavaScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TypeScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['$a'], '$a')

    def test_tokenizing_javascript_regular_expression(self):
        self.check_tokens(['/ab/'], '/ab/')
        self.check_tokens([r'/\//'], r'/\//')
        self.check_tokens([r'/a/igm'], r'/a/igm')

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_tokenizing_javascript_regular_expression1(self):
        self.check_tokens(['a', '=', '/ab/'], 'a=/ab/')

    def test_tokenizing_javascript_comments(self):
        self.check_tokens(['/**a/*/'], '''/**a/*/''')

    def test_tokenizing_pattern(self):
        self.check_tokens(['/\//'], r'''/\//''')

    def test_tokenizing_javascript_multiple_line_string(self):
        self.check_tokens(['"aaa\\\nbbb"'], '"aaa\\\nbbb"')

    def test_tokenizing_template_literal_with_expression(self):
        self.check_tokens(['`hello ${', 'name', '}`'], '`hello ${name}`')

    def xtest_tokenizing_template_literal_multiline(self):
        self.check_tokens(['`hello\nworld`'], '`hello\nworld`')

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
        self.assertEqual('(anonymous)', functions[0].name)

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

    # test "<>" error match in "< b) {} } function b () { return (dispatch, getState) =>"
    def test_function_in_arrow(self):
        functions = get_js_function_list(
            "function a () {f (a < b) {} } function b () { return (dispatch, getState) => {} }")
        self.assertEqual('a', functions[0].name)
        self.assertEqual('(anonymous)', functions[1].name)
        self.assertEqual('b', functions[2].name)

    # test long_name, fix "a x, y)" to "a (x, y)"
    def test_function_long_name(self):
        functions = get_js_function_list(
            "function a (x, y) {if (a < b) {} } function b () { return (dispatch, getState) => {} }")
        self.assertEqual('a ( x , y )', functions[0].long_name)
        self.assertEqual('b ( )', functions[2].long_name)

    def test_global(self):
        functions = get_js_function_list("{}")
        self.assertEqual(0, len(functions))

    def test_object_method_shorthand(self):
        functions = get_js_function_list("var obj = {method() {}}")
        self.assertEqual('method', functions[0].name)

    def test_object_method_with_computed_name(self):
        functions = get_js_function_list("var obj = {['computed' + 'Name']() {}}")
        self.assertEqual('computedName', functions[0].name)

    def xtest_object_getter_method(self):
        functions = get_js_function_list("var obj = {get prop() {}}")
        self.assertEqual('get prop', functions[0].name)

    def xtest_object_setter_method(self):
        functions = get_js_function_list("var obj = {set prop(val) {}}")
        self.assertEqual('set prop', functions[0].name)

    def xtest_async_function(self):
        functions = get_js_function_list("async function foo() {}")
        self.assertEqual('foo', functions[0].name)

    def xtest_generator_function(self):
        functions = get_js_function_list("function* gen() {}")
        self.assertEqual('gen', functions[0].name)

    def xtest_async_generator_function(self):
        functions = get_js_function_list("async function* gen() {}")
        self.assertEqual('gen', functions[0].name)

    def xtest_class_method_decorators(self):
        code = '''
            class Example {
                @decorator
                method() {}
            }
        '''
        functions = get_js_function_list(code)
        self.assertEqual('method', functions[0].name)

    def xtest_nested_object_methods(self):
        code = '''
            const obj = {
                outer: {
                    inner() {}
                }
            }
        '''
        functions = get_js_function_list(code)
        self.assertEqual('inner', functions[0].name)
