import unittest
from lizard import analyze_file


class TestPerlWordOperators(unittest.TestCase):
    def test_word_form_decisions(self):
        for expression, complexity in (('$a and $b', 2), ('$a or $b', 2),
                                       ('$a and $b or $c', 3)):
            with self.subTest(expression=expression):
                code = 'sub check { return ' + expression + '; }\n'
                function, = analyze_file.analyze_source_code('sample.pl', code).function_list
                self.assertEqual(('check', complexity),
                                 (function.name, function.cyclomatic_complexity))

    def test_quoted_words_do_not_add_decisions(self):
        code = 'sub check { return "and or"; }\n'
        function, = analyze_file.analyze_source_code('sample.pl', code).function_list
        self.assertEqual(1, function.cyclomatic_complexity)
