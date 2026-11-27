import unittest
from lizard import analyze_file


class TestStructuredTextConditionTokens(unittest.TestCase):
    def test_mixed_case_conditions(self):
        code = 'Function f: Bool\nIf a aNd b Then\n f:=TRUE;\nEnd_If\nEnd_Function\n'
        function, = analyze_file.analyze_source_code('sample.st', code).function_list
        self.assertEqual(('f', 3), (function.name, function.cyclomatic_complexity))

    def test_identifiers_starting_with_operator_words(self):
        for name in ('ORBIT', 'ANDROID', 'NOTHING', 'XORvalue'):
            with self.subTest(name=name):
                code = 'FUNCTION ' + name + ': BOOL\n' + name + ' := TRUE;\nEND_FUNCTION\n'
                function, = analyze_file.analyze_source_code('sample.st', code).function_list
                self.assertEqual((name, 1), (function.name, function.cyclomatic_complexity))
