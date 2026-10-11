import unittest

from lizard_languages.python import PythonReader
from .testPython import get_python_function_list


class Test_tokenizer_for_Python(unittest.TestCase):
    def test_comment_with_quote(self):
        for addition in ('', '(?x)'):
            with self.subTest(addition=addition):
                tokens = PythonReader.generate_tokens("#'\n''", addition)
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

    def test_floor_division_is_an_operator_not_a_comment(self):
        tokens = PythonReader.generate_tokens("a // b and c\nx //= 2")
        self.assertEqual(
            ['a', ' ', '//', ' ', 'b', ' ', 'and', ' ', 'c', '\n',
             'x', ' ', '//=', ' ', '2'],
            list(tokens))

    def test_floor_division_keeps_the_rest_of_the_line(self):
        functions = get_python_function_list(
            "def f(a, b):\n"
            "    if a // b > 3 and b > 0:\n"
            "        return a // 2\n"
            "    return 0\n")
        self.assertEqual(3, functions[0].cyclomatic_complexity)
        self.assertEqual(24, functions[0].token_count)

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
        cases = (
            ('def f():\n a="""block string"""', 2),
            ("def f():\n a='''block string'''", 2),
            ("def f():\n a='''block\n string'''", 3),
            ("def f():\n a='''block\n '''", 3),
        )
        for source, nloc in cases:
            with self.subTest(source=source):
                self.check_function_info(source, 7, nloc, nloc)

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
