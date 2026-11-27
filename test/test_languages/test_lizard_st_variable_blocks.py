import unittest
from lizard import analyze_file


class TestStructuredTextVariableBlocks(unittest.TestCase):
    def test_end_var_does_not_end_the_function(self):
        for declaration in ('VAR', 'VAR_INPUT', 'VAR_OUTPUT', 'VAR_IN_OUT'):
            with self.subTest(declaration=declaration):
                code = ('FUNCTION f: BOOL\n' + declaration + '\n x: BOOL;\nEND_VAR\n'
                        'IF x THEN\n f:=TRUE;\nEND_IF\nEND_FUNCTION\n')
                function, = analyze_file.analyze_source_code('sample.st', code).function_list
                self.assertEqual(('f', 2, 1, 8),
                                 (function.name, function.cyclomatic_complexity,
                                  function.start_line, function.end_line))
