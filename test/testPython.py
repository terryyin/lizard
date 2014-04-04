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

    def test_2_functions(self):
        class namespace4:
            def function1(a, b):
                pass
            def function2(a, b):
                pass
        functions = get_python_function_list(inspect.getsource(namespace4))
        self.assertEqual("function1", functions[0].name)
        self.assertEqual(2, len(functions))


def top_level_function_for_test():
    pass

