import inspect
import unittest

from lizard import analyze_file
from .testPython import get_python_function_list


class Test_Python_nloc(unittest.TestCase):

    def test_comment_is_not_counted_in_nloc(self):
        def function_with_comments():

            # comment
            pass
        functions = get_python_function_list(inspect.getsource(function_with_comments))
        self.assertEqual(2, functions[0].nloc)

    def test_triple_quoted_strings_as_comments_not_counted_in_nloc(self):
        """Test that triple-quoted strings used as comments are not counted in NLOC"""
        # Single line triple-quoted string as comment
        code1 = '''def test_func():
    x = 1
    """This is a comment, not a docstring."""
    return x
'''
        functions = get_python_function_list(code1)
        self.assertEqual(3, functions[0].nloc)  # def, x=1, return x

    def test_multiline_triple_quoted_strings_as_comments_not_counted_in_nloc(self):
        """Test that multiline triple-quoted strings used as comments are not counted in NLOC"""
        code = '''def test_func():
    x = 1
    """This is a multiline comment.
    It spans multiple lines.
    And should not be counted."""
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(3, functions[0].nloc)  # def, x=1, return x

    def test_single_quoted_triple_strings_as_comments_not_counted_in_nloc(self):
        """Test that single-quoted triple strings used as comments are not counted in NLOC"""
        code = '''def test_func():
    x = 1
    \'''This is also a comment.
    Using single quotes instead of double.
    Should also not be counted.\'''
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(3, functions[0].nloc)  # def, x=1, return x

    def test_docstring_still_not_counted_in_nloc(self):
        """Test that docstrings (first statement) are still correctly excluded from NLOC"""
        code = '''def test_func():
    """This is a proper docstring."""
    x = 1
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(3, functions[0].nloc)  # def, x=1, return x

    def test_mixed_comments_and_triple_quoted_strings_not_counted_in_nloc(self):
        """Test mixed regular comments and triple-quoted strings as comments"""
        code = '''def test_func():
    x = 1
    # Regular comment
    """Triple-quoted comment."""
    y = 2
    \'''Another triple-quoted comment.\'''
    # Another regular comment
    return x + y
'''
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].nloc)  # def, x=1, y=2, return x+y

    def test_triple_quoted_string_assigned_to_variable_counted_in_nloc(self):
        """Test that triple-quoted strings assigned to variables ARE counted in NLOC"""
        code = '''def test_func():
    x = 1
    comment = """This is assigned to a variable, so it's code."""
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].nloc)  # def, x=1, comment=..., return x

    def test_floor_division_triple_quoted_operand_counted_in_nloc(self):
        for quote in ('"""', "'''"):
            for operand in ('operand', 'first line\nsecond line'):
                with self.subTest(quote=quote, operand=operand):
                    code = ('def f(a):\n'
                            '    return a // ' + quote + operand + quote + '\n')
                    result = analyze_file.analyze_source_code('test.py', code)
                    expected_nloc = 2 + operand.count('\n')
                    self.assertEqual(expected_nloc, result.nloc)
                    self.assertEqual(expected_nloc, result.function_list[0].nloc)


class Test_Python_issue_317_comment_backslash(unittest.TestCase):
    """#317: backslash at end of comment must not swallow the next line."""

    def test_comment_backslash_hides_following_if(self):
        code = (
            'def testFunction(x):\n'
            '    # this is a comment\\\n'
            '    if x > 10:\n'
            '        return "Greater than 10"\n'
            '    return "Lower"\n'
        )
        functions = get_python_function_list(code)
        self.assertEqual(2, functions[0].cyclomatic_complexity)
