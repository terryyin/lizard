import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from lizard import get_reader_for
from lizard_languages import CobolReader

from .cobol_helpers import fixed, get_cobol_function_list


class TestCobol(unittest.TestCase):

    def test_extensions_registered(self):
        self.assertEqual(get_reader_for('pay.cbl'), CobolReader)
        self.assertEqual(get_reader_for('pay.cob'), CobolReader)
        self.assertEqual(get_reader_for('pay.cobol'), CobolReader)
        self.assertEqual(get_reader_for('pay.CBL'), CobolReader)

    def test_straight_line_paragraph(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. HELLO.',
            'PROCEDURE DIVISION.',
            'MAIN.',
            '    MOVE "HI" TO WS-MSG.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('MAIN', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(0, result[0].parameter_count)
        # Paragraph header + two statement lines
        self.assertEqual(3, result[0].nloc)

    def test_section_is_not_a_function(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. DEMO.',
            'PROCEDURE DIVISION.',
            'MAIN-SECTION SECTION.',
            'INIT.',
            '    MOVE 0 TO WS-COUNT.',
            'RUN.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        names = [f.name for f in result]
        self.assertEqual(['INIT', 'RUN'], names)
        self.assertNotIn('MAIN-SECTION', names)

    def test_non_procedure_divisions_yield_no_functions(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. SILENT.',
            'ENVIRONMENT DIVISION.',
            'CONFIGURATION SECTION.',
            'DATA DIVISION.',
            'WORKING-STORAGE SECTION.',
            '01 WS-COUNT PIC 9(4).',
            '01 IF-FLAG PIC X.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual([], result)

    def test_comments_and_string_literals_are_inert(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. LITERALS.',
            'PROCEDURE DIVISION.',
            'SAFE.',
            '    MOVE "IF AND OR WHEN" TO WS-TXT.',
            ('*', ' IF WHEN PERFORM UNTIL'),
            "    MOVE 'IF AND OR' TO WS-TXT2.",
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('SAFE', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        # Header + 3 code lines; column-7 comment excluded from NLOC
        self.assertEqual(4, result[0].nloc)

    def test_column7_debug_line_is_comment(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. DEBUG.',
            'PROCEDURE DIVISION.',
            'SAFE.',
            '    MOVE "HI" TO WS-MSG.',
            ('D', ' IF AND OR WHEN PERFORM UNTIL'),
            ('d', ' ELSE IF SEARCH EVALUATE'),
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('SAFE', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        # Header + MOVE + GOBACK; both debug lines excluded from NLOC
        self.assertEqual(3, result[0].nloc)

    def test_column7_continuation_keeps_string_inert(self):
        # Area-B continuation joins into one literal containing reserved words.
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. CONT.',
            'PROCEDURE DIVISION.',
            'SAFE.',
            '    MOVE "IF AND OR ',
            ('-', '    WHEN" TO WS-TXT.'),
            '    GOBACK.',
        ])
        tokens = list(CobolReader.generate_tokens(code))
        self.assertIn('"IF AND OR WHEN"', tokens)
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('SAFE', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_area_b_inline_comment_is_inert(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. INLINE.',
            'PROCEDURE DIVISION.',
            'SAFE.',
            '    MOVE "HI" TO WS-MSG. *> IF AND OR WHEN PERFORM',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('SAFE', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_preamble_before_paragraph_named_after_program_id(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. PAYROLL.',
            'PROCEDURE DIVISION.',
            '    PERFORM MAIN.',
            'MAIN.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(['PAYROLL', 'MAIN'], [f.name for f in result])

    def test_preamble_without_program_id(self):
        code = fixed([
            'PROCEDURE DIVISION.',
            '    MOVE 1 TO WS-X.',
            'MAIN.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(['(procedure)', 'MAIN'], [f.name for f in result])


class TestCobolCli(unittest.TestCase):

    def setUp(self):
        self.lizard_dir = os.path.dirname(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        self.lizard_py = os.path.join(self.lizard_dir, 'lizard.py')

    def test_discovery_language_filter_and_help(self):
        cobol_src = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. PAY.',
            'PROCEDURE DIVISION.',
            'CALC.',
            '    GOBACK.',
        ])
        py_src = 'def tool():\n    return 1\n'

        with TemporaryDirectory() as directory:
            (Path(directory) / 'pay.cbl').write_text(cobol_src)
            (Path(directory) / 'tool.py').write_text(py_src)

            cobol_only = subprocess.run(
                [sys.executable, self.lizard_py, '-l', 'cobol', directory],
                capture_output=True, text=True, cwd=self.lizard_dir)
            self.assertEqual(0, cobol_only.returncode, msg=cobol_only.stderr)
            self.assertIn('CALC', cobol_only.stdout)
            self.assertNotIn('tool', cobol_only.stdout)

            both = subprocess.run(
                [sys.executable, self.lizard_py, directory],
                capture_output=True, text=True, cwd=self.lizard_dir)
            self.assertEqual(0, both.returncode, msg=both.stderr)
            self.assertIn('CALC', both.stdout)
            self.assertIn('tool', both.stdout)

        help_result = subprocess.run(
            [sys.executable, self.lizard_py, '-h'],
            capture_output=True, text=True, cwd=self.lizard_dir)
        self.assertEqual(0, help_result.returncode, msg=help_result.stderr)
        self.assertIn('cobol', help_result.stdout)

    def test_readme_lists_cobol(self):
        readme = Path(self.lizard_dir) / 'README.rst'
        self.assertIn('COBOL', readme.read_text())
