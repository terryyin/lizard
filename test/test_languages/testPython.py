import unittest
import inspect
from ..testHelpers import get_python_function_list_with_extension
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth
from lizard_languages.python import PythonReader
import os


def get_python_function_list(source_code):
    return get_python_function_list_with_extension(source_code, NestDepth())


class Test_tokenizer_for_Python(unittest.TestCase):
    def test_comment_with_quote(self):
        tokens = PythonReader.generate_tokens("#'\n''")
        self.assertEqual(["#'", "\n", "''"], list(tokens))

    def test_multiline_string_tokens(self):
        code = '''"""First line
Second line with 'single quotes'
Third line with "double quotes"
Fourth line with mixed quotes
Fifth line with # comment markers
"""'''
        tokens = list(PythonReader.generate_tokens(code))
        self.assertEqual(1, len(tokens))  # The entire multi-line string should be one token
        self.assertEqual(code, tokens[0])  # The token should preserve the exact string

    def test_block_string_is_one_token(self):
        code = 'def a():\n    a = """\na b c d e f g h i"""\n    return a\n'
        functions = get_python_function_list(code)
        self.assertEqual(9, functions[0].token_count)
        self.assertEqual(4, functions[0].end_line)

    def check_function_info(self, source, expect_token_count, expect_nloc, expect_endline):
        functions = get_python_function_list(source)
        self.assertEqual(expect_token_count, functions[0].token_count)
        self.assertEqual(expect_nloc, functions[0].nloc)
        self.assertEqual(expect_endline, functions[0].end_line)

    def test_block_string(self):
        self.check_function_info('def f():\n a="""block string"""', 7, 2, 2)
        self.check_function_info("def f():\n a='''block string'''", 7, 2, 2)
        self.check_function_info("def f():\n a='''block string'''", 7, 2, 2)
        self.check_function_info("def f():\n a='''block\n string'''", 7, 3, 3)
        self.check_function_info("def f():\n a='''block\n '''", 7, 3, 3)

    def test_docstring_is_not_counted_in_nloc(self):
        self.check_function_info("def f():\n '''block\n '''\n pass", 6, 2, 4)

    def test_complex_multiline_string(self):
        code = '''def f():
            x = """First line
                Second line with 'single quotes'
                Third line with "double quotes"
                Fourth line with mixed quotes
                Fifth line with # comment markers
                """
            return x'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(9, functions[0].token_count)  # def, f, (), :, x, =, multiline-string, return, x
        self.assertEqual(8, functions[0].nloc)  # 8 lines total
        self.assertEqual(8, functions[0].end_line)


class Test_Python_nesting_level(unittest.TestCase):

    def test_top_level_function(self):
        functions = get_python_function_list(
            "def a():\n" +
            "    pass")
        self.assertEqual(0, functions[0].top_nesting_level)

    def test_second_top_level_functions(self):
        functions = get_python_function_list(
            "def a():\n" +
            "    pass\n" +
            "def b():\n" +
            "    pass"
        )
        self.assertEqual(0, functions[1].top_nesting_level)

    def test_top_level_function_with_leading_space(self):
        functions = get_python_function_list(
            " def a():\n" +
            "    pass\n"
        )
        self.assertEqual(1, functions[0].top_nesting_level)

    def test_2nd_level_function_with_leading_space(self):
        functions = get_python_function_list(
            "class C:\n" +
            "    def f():\n" +
            "        pass\n"
        )
        self.assertEqual(1, functions[0].top_nesting_level)

    def test_miss_indented_comment(self):
        functions = get_python_function_list(
            "class C:\n" +
            " class D:\n" +
            "  def a():\n" +
            "   pass\n" +
            " #\n" +
            "   def b():\n" +
            "    pass")
        self.assertEqual(7, functions[0].end_line)


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
        self.assertEqual(1, functions[0].max_nesting_depth)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("simple_function( )", functions[0].long_name)

    def test_two_simple_python_function(self):
        source = """
            def foo():
                #'
                return False

            def bar():
                if foo == 'bar':
                    return True
                """
        functions = get_python_function_list(source)
        self.assertEqual(2, len(functions))

    def test_multi_line_function_def_function_end(self):
        source = """
            def foo(arg1,
                arg2,
            ):
                # comment
                return True

            def foo2(arg1,
                arg2,
                arg3
            ):
                if True:
                    return False
            """
        functions = get_python_function_list(source)
        self.assertEqual(6, functions[0].end_line)
        self.assertEqual(13, functions[1].end_line)

    def test_multi_line_function_def_with_indentation_more_than_function_body(self):
        def function(arg1,
                     arg2
                     ):
            if True:
                return False

        functions = get_python_function_list(inspect.getsource(function))
        self.assertEqual(5, functions[0].nloc)
        self.assertEqual(5, functions[0].end_line)

    def test_function_surrounded_by_global_statements(self):
        source = """
        s1 = 'global statement'
        def function(arg1,
                     arg2
                     ):
            if True:
                return False
        s2 = 'global statement'
        """
        functions = get_python_function_list(source)
        self.assertEqual(5, functions[0].nloc)
        self.assertEqual(7, functions[0].end_line)

    def test_parameter_count(self):
        class namespace2:
            def function_with_2_parameters(a, b):
                pass
        functions = get_python_function_list(inspect.getsource(namespace2))
        self.assertEqual(2, functions[0].parameter_count)

    def test_parameter_count_with_default_value(self):
        class namespace_df:
            def function_with_2_parameters_and_default_value(a, b=None):
                pass
        functions = get_python_function_list(inspect.getsource(namespace_df))
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['a', 'b'], functions[0].parameters)
        self.assertEqual("function_with_2_parameters_and_default_value( a , b = None )",
                         functions[0].long_name)

    def test_parameter_count_with_type_annotations(self):
        functions = get_python_function_list('''
            def function_with_3_parameters(a: str, b: int, c: float):
                pass
        ''')
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].parameter_count)
        self.assertEqual(['a', 'b', 'c'], functions[0].parameters)
        self.assertEqual("function_with_3_parameters( a : str , b : int , c : float )",
                         functions[0].long_name)

    def test_parameter_count_with_type_annotation_and_default(self):
        functions = get_python_function_list('''
            def function_with_3_parameters(a: int = 1):
                pass
        ''')
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].parameter_count)
        self.assertEqual(['a'], functions[0].parameters)
        self.assertEqual("function_with_3_parameters( a : int = 1 )",
                         functions[0].long_name)

    def test_parameter_count_with_parameterized_type_annotations(self):
        functions = get_python_function_list('''
            def function_with_parameterized_parameter(a: dict[str, tuple[int, float]]):
                pass
            def function_with_3_parameterized_parameters(a: dict[str, int],
                                                         b: list[float],
                                                         c: tuple[int, float, str]
                                                         ):
                pass
                
        ''')
        self.assertEqual(2, len(functions))
        self.assertEqual(1, functions[0].parameter_count)
        self.assertEqual(['a'], functions[0].parameters)
        self.assertEqual("function_with_parameterized_parameter( a : dict [ str , tuple [ int , float ] ] )",
                         functions[0].long_name)
        self.assertEqual(3, functions[1].parameter_count)
        self.assertEqual(['a', 'b', 'c'], functions[1].parameters)
        self.assertEqual("function_with_3_parameterized_parameters( a : dict [ str , int ] , b : list [ float ] , c : tuple [ int , float , str ] )",
                         functions[1].long_name)

    def test_parameter_count_with_trailing_comma(self):
        functions = get_python_function_list('''
            def foo(arg1,
                    arg2,
                    ):
                # comment
                return True
        ''')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['arg1', 'arg2'], functions[0].parameters)

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
        self.assertEqual("function1.function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(5, functions[1].end_line)
        self.assertEqual(2, functions[1].cyclomatic_complexity)
        self.assertEqual(2, functions[1].max_nesting_depth)
        # will be fixed, should be equal to 1

    def test_nested_functions_ended_at_eof(self):
        class namespace6:
            def function1(a, b):
                def function2(a, b):
                    pass
        functions = get_python_function_list(inspect.getsource(namespace6))
        self.assertEqual(2, len(functions))
        self.assertEqual("function1.function2", functions[0].name)
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
        self.assertEqual("function1.function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(4, functions[1].end_line)

    def xtest_one_line_functions(self):
        class namespace8:
            def a( ):pass
            def b( ):pass
        functions = get_python_function_list(inspect.getsource(namespace8))
        self.assertEqual("a", functions[0].name)
        self.assertEqual("b", functions[1].name)

    def test_nested_depth_metric_multiple_continuous_loop_statements(self):
        class namespace9:
            def function1():
                if IamOnEarth:
                    if IamOnShip:
                        return toMars()
        functions = get_python_function_list(inspect.getsource(namespace9))
        self.assertEqual(1, len(functions))
        self.assertEqual("function1", functions[0].name)
        self.assertEqual(3, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[0].max_nesting_depth)
        self.assertEqual(5, functions[0].end_line)

    def xtest_nested_depth_metric_multiple_discrete_loop_statement(self):
        class namespace10:
            def function1():
                if IamOnEarth:
                    if not IamOnShip:
                        return toMars()
                elif IamOnMoon:
                    return backEarth()
        functions = get_python_function_list(inspect.getsource(namespace10))
        self.assertEqual(1, len(functions))
        self.assertEqual("function1", functions[0].name)
        self.assertEqual(4, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[0].max_nesting_depth)
        self.assertEqual(7, functions[0].end_line)

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

    def xtest_if_elif_and_or_for_while_except_finally(self):
        code =  'def a():\n' + \
                '    if elif and or for while except finally\n'
        functions = get_python_function_list(code)
        self.assertEqual(9, functions[0].cyclomatic_complexity)
        self.assertEqual(8, functions[0].max_nesting_depth)

    def test_python_forgive_global(self):
        code = '''
# Global code with complexity
x = 1
if x > 0:
    print("Positive")
elif x < 0:
    print("Negative")
else:
    print("Zero")

# #lizard forgive global
# More global code with complexity
y = 2
if y > 0:
    print("Y is positive")
elif y < 0:
    print("Y is negative")

# This function should still be counted
def test_function(param):
    if param > 0:
        print("Param is positive")
    elif param < 0:
        print("Param is negative")
    else:
        print("Param is zero")
'''
        functions = get_python_function_list(code)
        
        # Should have one function (test_function) since global code is forgiven
        self.assertEqual(1, len(functions))
        
        # Verify the function is the one we expect
        function = functions[0]
        self.assertEqual("test_function", function.name)
        self.assertEqual(3, function.cyclomatic_complexity)  # 1 base + 2 conditions (else doesn't count)


def top_level_function_for_test():
    pass
