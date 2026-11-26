import unittest
from lizard import analyze_file


class TestMixedCasePLSQLConditions(unittest.TestCase):
    def test_keyword_case_does_not_change_complexity(self):
        bodies = [
            ('If x aNd y oR z Then NULL; ElsIf y Then NULL; End If;', 5),
            ('For i In 1..5 LoOp NULL; End LoOp;', 2),
            ('WhIlE ready LoOp NULL; End LoOp;', 2),
            ('CaSe x WhEn 1 ThEn NULL; WhEn 2 ThEn NULL; End CaSe;', 3),
            ("value := 'If And Or While';", 1),
        ]
        for body, complexity in bodies:
            with self.subTest(body=body):
                source = 'Create Procedure MixedName As Begin ' + body + ' End;'
                function, = analyze_file.analyze_source_code(
                    'sample.sql', source).function_list
                self.assertEqual('MixedName', function.name)
                self.assertEqual(complexity, function.cyclomatic_complexity)
