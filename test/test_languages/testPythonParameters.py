import unittest

from .testPython import get_python_function_list


class TestPythonParameters(unittest.TestCase):
    def test_parameter_count_with_comma_in_default_value(self):
        functions = get_python_function_list('''
            def f(a: dict[str, int], b: tuple[int, int] = (1, 2)):
                pass
            def g(a=[1, 2], b={'x': 1, 'y': 2}, c=max(1, 2)):
                pass
        ''')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['a', 'b'], functions[0].parameters)
        self.assertEqual(3, functions[1].parameter_count)
        self.assertEqual(['a', 'b', 'c'], functions[1].parameters)

    def test_parameter_count_with_lambda_default_value(self):
        functions = get_python_function_list('''
            def f(a, key=lambda x, y: x, b=lambda: 0, c=lambda x: (x, 1)):
                pass
        ''')
        self.assertEqual(4, functions[0].parameter_count)
        self.assertEqual(['a', 'key', 'b', 'c'], functions[0].parameters)

    def test_parameter_count_with_nested_brackets_in_a_type(self):
        functions = get_python_function_list('''
            def f(cb: Callable[[int, str], int] = None, d: dict[str, list[int]] = {}):
                pass
        ''')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['cb', 'd'], functions[0].parameters)

    def test_parameter_count_with_comma_in_an_f_string_default_value(self):
        functions = get_python_function_list('''
            def f(x=f"{1,2}", y=f"{a}, {b}"):
                pass
        ''')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['x', 'y'], functions[0].parameters)

    def test_nested_lambda_defaults_keep_outer_parameters(self):
        functions = get_python_function_list(
            'def f(callback=lambda x=lambda a, b: a, y=(1, 2): '
            '(x, y), tail=None):\n    pass\n')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['callback', 'tail'], functions[0].parameters)

    def test_lambda_returning_lambda_keeps_outer_parameters(self):
        functions = get_python_function_list(
            'def f(callback=lambda x: lambda y, z: y, tail=None):\n'
            '    pass\n')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['callback', 'tail'], functions[0].parameters)

    def test_nested_fstring_commas_keep_outer_parameters(self):
        functions = get_python_function_list(
            'def f(value=f"{f\'{(1, 2)}\'}", tail=None):\n    pass\n')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['value', 'tail'], functions[0].parameters)

    def test_defaults_preserve_signature_and_following_function(self):
        functions = get_python_function_list(
            'def f(value=(1, 2), callback=lambda x, y: x):\n'
            '    return callback(value)\n'
            'def following(item):\n'
            '    return item\n')
        self.assertEqual(['f', 'following'], [f.name for f in functions])
        self.assertEqual(['value', 'callback'], functions[0].parameters)
        self.assertEqual(
            'f( value = ( 1 , 2 ) , callback = lambda x , y : x )',
            functions[0].long_name)
        self.assertEqual(2, functions[0].end_line)
        self.assertEqual(3, functions[1].start_line)
        self.assertEqual(['item'], functions[1].parameters)

    def test_fstring_default_floor_division_keeps_commas_and_condition(self):
        functions = get_python_function_list(
            'def f(value=f"{(n // 2 or 1, n)}", tail=None):\n'
            '    return value\n'
            'def following(item):\n'
            '    return item\n')
        self.assertEqual(['f', 'following'], [f.name for f in functions])
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['value', 'tail'], functions[0].parameters)
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[0].end_line)

    def test_fstring_format_colon_does_not_close_lambda_head(self):
        functions = get_python_function_list(
            'def f(cb=lambda x=f"{a:02d}", y=1: x, tail=None):\n'
            '    pass\n')
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['cb', 'tail'], functions[0].parameters)

    def test_fstring_format_brackets_do_not_close_function_signature(self):
        functions = get_python_function_list(
            'def f(value=f"{a:)>10}", tail=None):\n'
            '    pass\n'
            'def following(item):\n'
            '    return item\n')
        self.assertEqual(['f', 'following'], [f.name for f in functions])
        self.assertEqual(2, functions[0].parameter_count)
        self.assertEqual(['value', 'tail'], functions[0].parameters)
        self.assertEqual(2, functions[0].end_line)
