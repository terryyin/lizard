import unittest

from .testPython import get_python_function_list


class Test_Python_fstring_complexity(unittest.TestCase):
    """Control flow inside f-string {...} interpolations must be counted (#317)."""

    def test_fstring_comprehension_counts_for(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"{\', \'.join([x for x in items])}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_fstring_ternary_counts_if(self):
        functions = get_python_function_list(
            'def f(cond):\n    return f"{\'a\' if cond else \'b\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_fstring_logical_operators_counted(self):
        functions = get_python_function_list(
            'def f(a, b):\n    return f"{a and b or a}"\n')
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_fstring_floor_division_keeps_following_condition(self):
        functions = get_python_function_list(
            'def f(a, b, c):\n    return f"{a // b or c}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_floor_division_in_fstring_literal_does_not_add_complexity(self):
        functions = get_python_function_list(
            'def f(value):\n    return f"a // b or c {value}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_fstring_matches_non_fstring_equivalent(self):
        with_fs = get_python_function_list(
            'def f(i):\n    return f"{[x for x in i]}"\n')
        without = get_python_function_list(
            'def f(i):\n    return [x for x in i]\n')
        self.assertEqual(without[0].cyclomatic_complexity,
                         with_fs[0].cyclomatic_complexity)

    def test_keyword_inside_nested_string_not_counted(self):
        # the 'if' lives inside a nested string literal; only 'or' is a real condition
        functions = get_python_function_list(
            'def f(x):\n    return f"{x or \'use this if empty\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_escaped_braces_are_not_interpolation(self):
        # {{ and }} are literal braces, so the 'if' is plain text, not a condition
        functions = get_python_function_list(
            'def f():\n    return f"{{ keep this if you can }}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_plain_fstring_has_base_complexity(self):
        functions = get_python_function_list(
            'def f(name):\n    return f"hello {name}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_nested_fstring_counts_inner_control_flow(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"{f\'{[x for x in items]}\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_brace_inside_nested_string_in_interpolation(self):
        # '}' inside a nested string must not terminate the interpolation early
        functions = get_python_function_list(
            'def f(x):\n    return f"{x or \'}\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_ternary_after_string_containing_brace(self):
        functions = get_python_function_list(
            'def f(cond):\n    return f"{foo(\'}\') if cond else bar}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_format_spec_does_not_add_complexity(self):
        functions = get_python_function_list(
            'def f(x):\n    return f"{x:.2f}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_triple_quoted_fstring_with_interpolation(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"""{\', \'.join([x for x in items])}"""\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_bytes_fstring_counts_control_flow(self):
        functions = get_python_function_list(
            'def f(items):\n    return bf"{b\'x\' if items else b\'\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)
