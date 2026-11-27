import unittest
from lizard import analyze_file


class TestErlangShortCircuit(unittest.TestCase):
    def test_short_circuit_decisions(self):
        for expression, complexity in (('A andalso B', 2), ('A orelse B', 2),
                                       ('A andalso B orelse C', 3)):
            with self.subTest(expression=expression):
                code = 'f(A, B, C) -> ' + expression + '.\n'
                function, = analyze_file.analyze_source_code('sample.erl', code).function_list
                self.assertEqual(('f', complexity),
                                 (function.name, function.cyclomatic_complexity))

    def test_operator_words_in_strings_are_not_decisions(self):
        code = 'f() -> "andalso orelse".\n'
        function, = analyze_file.analyze_source_code('sample.erl', code).function_list
        self.assertEqual(1, function.cyclomatic_complexity)
