import unittest
from lizard import analyze_file


class TestNestedRParameters(unittest.TestCase):
    def test_nested_defaults_preserve_formal_parameter_count(self):
        samples = [
            ('a=list(1, 2, 3), b=3', ['a', 'b']),
            ('a=list(1), b=3, c=4', ['a', 'b', 'c']),
            ('a=matrix(1, nrow=2, ncol=3), b=4', ['a', 'b']),
            ('a=values[1, 2], b=values[[1]], c=3', ['a', 'b', 'c']),
            ('a={list(1, 2)}, b=3', ['a', 'b']),
            ('', []),
        ]
        for parameters, names in samples:
            with self.subTest(parameters=parameters):
                source = ('f <- function(' + parameters + ') { if (ok) { 1 } }'
                          '\ng <- function(z) { z }')
                functions = analyze_file.analyze_source_code(
                    'sample.r', source).function_list
                self.assertEqual(['f', 'g'], [f.name for f in functions])
                self.assertEqual(names, functions[0].parameters)
                self.assertEqual(len(names), functions[0].parameter_count)
                self.assertEqual(2, functions[0].cyclomatic_complexity)
                self.assertEqual(['z'], functions[1].parameters)
