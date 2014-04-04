import unittest
import inspect
from lizard import  analyze_file, FileAnalyzer, get_extensions


def get_python_function_list(source_code):
    return analyze_file.analyze_source_code("a.py", source_code).function_list


class Test_parser_for_Python(unittest.TestCase):

    def test_empty_source_should_return_no_function(self):
        functions = get_python_function_list("")

    def test_simple_python_function(self):
        def simple_function():
            if IamOnEarth:
                return toMars()
        functions = get_python_function_list(inspect.getsource(simple_function))
        self.assertEqual("simple_function", functions[0].name)
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(3, functions[0].end_line)

    def test_parameter_count(self):
        def simple_function(a, b):
            if IamOnEarth:
                return toMars()
        functions = get_python_function_list(inspect.getsource(simple_function))
        self.assertEqual(2, functions[0].parameter_count)

    def xtest_function_end(self):
        class namespace:
            def simple_function(self):
                pass
            blah = 42
        functions = get_python_function_list(inspect.getsource(namespace))
        self.assertEqual("simple_function", functions[0].name)
        self.assertEqual(3, functions[0].end_line)
