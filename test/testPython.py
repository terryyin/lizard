import unittest
import inspect
from lizard import  analyze_file, FileAnalyzer, get_extensions


def get_python_function_list(source_code):
    return analyze_file.analyze_source_code("a.py", source_code).function_list


class Test_parser_for_Python(unittest.TestCase):

    def test_empty_source_should_return_no_function(self):
        functions = get_python_function_list("")
        self.assertEqual(0, len(functions))

    def test_simple_python_function(self):
        class namespace1:
            def simple_function():
                if IamOnEarth:
                    return toMars()
        functions = get_python_function_list(inspect.getsource(namespace1))
        self.assertEqual(1, len(functions))
        self.assertEqual("simple_function", functions[0].name)
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(4, functions[0].end_line)

    def test_parameter_count(self):
        class namespace2:
            def function_with_2_parameters(a, b):
                pass
        functions = get_python_function_list(inspect.getsource(namespace2))
        self.assertEqual(2, functions[0].parameter_count)

    def test_function_end(self):
        class namespace3:
            def simple_function(self):
                pass

            blah = 42
        functions = get_python_function_list(inspect.getsource(namespace3))
        self.assertEqual(1, len(functions))
        self.assertEqual("simple_function", functions[0].name)
        self.assertEqual(3, functions[0].end_line)

    def test_top_level_functions(self):
        functions = get_python_function_list(inspect.getsource(top_level_function_for_test))
        self.assertEqual(1, len(functions))

    def test_2_top_level_functions(self):
        functions = get_python_function_list('''
def a():
    pass
def b():
    pass
''')
        self.assertEqual(2, len(functions))
        self.assertEqual("a", functions[0].name)

    def test_2_functions(self):
        class namespace4:
            def function1(a, b):
                pass
            def function2(a, b):
                pass
        functions = get_python_function_list(inspect.getsource(namespace4))
        self.assertEqual(2, len(functions))

    def test_nested_functions(self):
        class namespace5:
            def function1(a, b):
                def function2(a, b):
                    pass
                a = 1 if b == 2 else 3
        functions = get_python_function_list(inspect.getsource(namespace5))
        self.assertEqual(2, len(functions))
        self.assertEqual("function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(5, functions[1].end_line)
        self.assertEqual(2, functions[1].cyclomatic_complexity)

    def test_nested_functions_ended_at_eof(self):
        class namespace6:
            def function1(a, b):
                def function2(a, b):
                    pass
        functions = get_python_function_list(inspect.getsource(namespace6))
        self.assertEqual(2, len(functions))
        self.assertEqual("function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(4, functions[1].end_line)

    def test_nested_functions_ended_at_same_line(self):
        class namespace7:
            def function1(a, b):
                def function2(a, b):
                    pass
            def function3():
                pass
        functions = get_python_function_list(inspect.getsource(namespace7))
        self.assertEqual(3, len(functions))
        self.assertEqual("function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(4, functions[1].end_line)

    def test_one_line_functions(self):
        class namespace8:
            def a( ):pass
            def b( ):pass
        functions = get_python_function_list(inspect.getsource(namespace8))
        self.assertEqual("a", functions[0].name)
        self.assertEqual("b", functions[1].name)

    def test_comment_is_not_counted_in_nloc(self):
        def function_with_comments():

            # comment
            pass
        functions = get_python_function_list(inspect.getsource(function_with_comments))
        self.assertEqual(2, functions[0].nloc)

    def test_odd_blank_line(self):
        code =  "class c:\n" + \
                "    def f():\n" +\
                "  \n" +\
                "         pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].end_line)

    def test_odd_line_with_comment(self):
        code =  "class c:\n" + \
                "    def f():\n" +\
                "  #\n" +\
                "         pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].end_line)

    def test_tab_is_same_as_8_spaces(self):
        code =  ' ' * 7 + "def a():\n" + \
                '\t'    +  "pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(2, functions[0].end_line)

    def test_if_elif_and_or_for_while_except_finally(self):
        code =  'def a():\n' + \
                '    if elif and or for while except finally\n'
        functions = get_python_function_list(code)
        self.assertEqual(9, functions[0].cyclomatic_complexity)

    def test_block_string_is_one_token(self):
        code =  'def a():\n' + \
                "    a = '''\n" +\
                "a b c d e f g h i'''\n"+\
                "    return a\n"
        functions = get_python_function_list(code)
        self.assertEqual(9, functions[0].token_count)
        self.assertEqual(4, functions[0].end_line)

    def check_function_info(self, source, expect_token_count, expect_nloc, expect_endline):
        functions = get_python_function_list(source)
        self.assertEqual(expect_token_count, functions[0].token_count)
        self.assertEqual(expect_nloc, functions[0].nloc)
        self.assertEqual(expect_endline, functions[0].end_line)

    def test_block_string(self):
        self.check_function_info('def f(): a="""block string"""', 7, 1, 1)
        self.check_function_info("def f(): a='''block string'''", 7, 1, 1)
        self.check_function_info("def f():\n a='''block string'''", 7, 2, 2)
        self.check_function_info("def f():\n a='''block\n string'''", 7, 3, 3)
        self.check_function_info("def f():\n a='''block\n '''", 7, 3, 3)

    def test_docstring_is_not_counted_in_nloc(self):
        self.check_function_info("def f():\n '''block\n '''\n pass", 6, 2, 4)

    #global complexity


def top_level_function_for_test():
    pass

