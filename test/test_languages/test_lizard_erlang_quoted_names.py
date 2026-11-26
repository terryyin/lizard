import unittest
from lizard import analyze_file


class TestQuotedErlangNames(unittest.TestCase):
    def test_quoted_function_heads(self):
        for name in ("'quoted-name'", "'has a space'", "'if'"):
            with self.subTest(name=name):
                code = name + '(X) -> X.\nplain(Y) -> Y.\n'
                functions = analyze_file.analyze_source_code('sample.erl', code).function_list
                self.assertEqual([name, 'plain'], [f.name for f in functions])
                self.assertEqual([(1, 1, 1), (2, 2, 1)],
                                 [(f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])

    def test_quoted_calls_and_atoms_are_not_function_heads(self):
        code = "f() -> m:'target'(), 'ok'.\n"
        functions = analyze_file.analyze_source_code('sample.erl', code).function_list
        self.assertEqual(['f'], [f.name for f in functions])
