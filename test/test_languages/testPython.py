import inspect
import unittest

from ..testHelpers import get_python_function_list_with_extension
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_python_function_list(source_code):
    return get_python_function_list_with_extension(source_code, NestDepth())


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
        self.assertEqual("function_with_3_parameterized_parameters( a : dict [ str , int ] , "
                         "b : list [ float ] , c : tuple [ int , float , str ] )",
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
