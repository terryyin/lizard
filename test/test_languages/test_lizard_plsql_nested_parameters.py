import unittest
from lizard import analyze_file


class TestNestedPLSQLParameters(unittest.TestCase):
    def test_default_calls_do_not_split_formal_parameters(self):
        samples = [
            ('a NUMBER DEFAULT greatest(1, 2, 3), b NUMBER', 2),
            ('a NUMBER DEFAULT abs(1), b NUMBER, c NUMBER', 3),
            ('a NUMBER := greatest(abs(-1), 2), b NUMBER := 4', 2),
            ('', 0),
        ]
        for kind, suffix, body in [
                ('PROCEDURE', '', 'NULL;'),
                ('FUNCTION', ' RETURN NUMBER', 'RETURN 1;')]:
            for parameters, count in samples:
                with self.subTest(kind=kind, parameters=parameters):
                    source = ('CREATE ' + kind + ' f(' + parameters + ')' +
                              suffix + ' AS BEGIN ' + body + ' END;\n'
                              'CREATE PROCEDURE g AS BEGIN NULL; END;')
                    functions = analyze_file.analyze_source_code(
                        'sample.sql', source).function_list
                    self.assertEqual(['f', 'g'], [f.name for f in functions])
                    self.assertEqual(count, functions[0].parameter_count)
                    self.assertEqual(list("abc"[:count]), functions[0].parameters)
                    self.assertEqual(1, functions[0].cyclomatic_complexity)
