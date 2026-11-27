import unittest
from lizard import analyze_file


class TestStructuredTextBlockComments(unittest.TestCase):
    def test_multiline_comment_does_not_change_nesting(self):
        code = 'FUNCTION f: BOOL\n(* IF a THEN\n WHILE b DO\n *)\nf := TRUE;\nEND_FUNCTION\n'
        result = analyze_file.analyze_source_code('sample.st', code)
        function, = result.function_list
        self.assertEqual(3, result.nloc)
        self.assertEqual(('f', 1, 6, 3, 1),
                         (function.name, function.start_line, function.end_line,
                          function.nloc, function.cyclomatic_complexity))

    def test_code_after_comment_remains_visible(self):
        code = 'FUNCTION f: BOOL\n(* ignored *) IF x THEN f:=TRUE; END_IF;\nEND_FUNCTION\n'
        function, = analyze_file.analyze_source_code('sample.st', code).function_list
        self.assertEqual(2, function.cyclomatic_complexity)
