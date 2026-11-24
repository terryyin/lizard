import unittest
from lizard import analyze_file


class TestSinglePercentComments(unittest.TestCase):
    def test_comment_lines_do_not_count_as_code(self):
        for marker in ('%', '%%', '%%%'):
            with self.subTest(marker=marker):
                code = marker + ' file comment\nf(X) ->\n    ' + marker + ' body comment\n    X.\n'
                result = analyze_file.analyze_source_code('sample.erl', code)
                self.assertEqual(2, result.nloc)
                function, = result.function_list
                self.assertEqual(('f', 2, 4, 2, 1),
                                 (function.name, function.start_line, function.end_line,
                                  function.nloc, function.cyclomatic_complexity))
