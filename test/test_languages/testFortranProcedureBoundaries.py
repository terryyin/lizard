import unittest

from lizard import analyze_file


class TestFortranProcedureBoundaries(unittest.TestCase):
    def check_procedures(self, body, first_complexity=1):
        lines = [
            'module sample',
            'contains',
            'subroutine first(a)',
            'real :: a(:)',
            *('    ' + line for line in body.splitlines()),
            'end subroutine first',
            'subroutine second()',
            'if (ready) call show()',
            'end subroutine second',
            'end module sample',
        ]
        functions = analyze_file.analyze_source_code(
            'sample.f90', '\n'.join(lines)).function_list
        self.assertEqual(['sample::first', 'sample::second'],
                         [function.name for function in functions])
        first, second = functions
        self.assertEqual((3, lines.index('end subroutine first') + 1),
                         (first.start_line, first.end_line))
        self.assertEqual((lines.index('subroutine second()') + 1,
                          lines.index('end subroutine second') + 1),
                         (second.start_line, second.end_line))
        self.assertEqual((len(body.splitlines()) + 3, 3),
                         (first.nloc, second.nloc))
        self.assertEqual((first_complexity, 2),
                         (first.cyclomatic_complexity,
                          second.cyclomatic_complexity))
        self.assertEqual((1, 0),
                         (first.parameter_count, second.parameter_count))
        self.assertEqual(12, second.token_count)
        self.assertEqual((1, 1),
                         (first.top_nesting_level, second.top_nesting_level))

    def test_string_concatenation(self):
        self.check_procedures("call show('hello ' // name)")

    def test_where_statement(self):
        for body in ('where (a < 0) a = 0',
                     'WhErE (a < 0) a = 0; a = a + 1',
                     'where &\n& (a < 0) a = 0',
                     'where (a < &\n& 0) a = 0',
                     'where (a < 0) &\n& a = 0',
                     'where (a < 0) & ! continued assignment\na = 0'):
            with self.subTest(body=body):
                self.check_procedures(body)

    def test_where_block(self):
        for body in ('where (a < 0)\na = 0\nend where',
                     'where (a < 0); a = 0; end where',
                     'where &\n& (a < 0)\na = 0\nend where',
                     'where (a < &\n& 0)\na = 0\nend where',
                     'where (a < 0) ! block mask\na = 0\nend where'):
            with self.subTest(body=body):
                self.check_procedures(body)

    def test_data_and_save_identifiers(self):
        for identifier in ('data', 'save', 'DATA', 'SAVE'):
            with self.subTest(identifier=identifier):
                self.check_procedures('pointer => ' + identifier)

    def test_data_and_save_statements(self):
        self.check_procedures('data x /0/\nsave x\nsave')

    def test_data_statement_with_keyword_identifier(self):
        self.check_procedures('integer do\ndata do /0/', first_complexity=3)

    def test_save_statement_with_keyword_identifier(self):
        self.check_procedures('integer type\nsave type')

    def test_select_type_guards(self):
        self.check_procedures(
            'select type (value)\n'
            'type is (integer)\n'
            'print *, value\n'
            'class is (point)\n'
            'print *, value\n'
            'class default\n'
            'print *, value\n'
            'end select')

    def test_derived_type_named_is(self):
        self.check_procedures('type is\ninteger :: value\nend type is')
