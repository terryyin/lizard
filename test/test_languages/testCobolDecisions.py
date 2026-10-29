import unittest

from .cobol_helpers import fixed, get_cobol_function_list


class TestCobolDecisions(unittest.TestCase):

    def test_if_and_raises_ccn(self):
        # CHECK-LIMIT: IF + AND → CCN 3; ELSE / END-IF add nothing.
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. BRANCH.',
            'PROCEDURE DIVISION.',
            'CHECK-LIMIT.',
            '    IF WS-COUNT > 10 AND WS-FLAG = "Y"',
            '        THEN ADD 1 TO WS-OVER',
            '        ELSE MOVE 0 TO WS-OVER',
            '    END-IF.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('CHECK-LIMIT', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_evaluate_when_arms_and_when_other(self):
        # CLASSIFY: two counted WHEN arms (WHEN 2 WHEN 3 fall-through);
        # WHEN OTHER / END-EVALUATE add 0.
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. EVALDEMO.',
            'PROCEDURE DIVISION.',
            'CLASSIFY.',
            '    EVALUATE WS-CODE',
            '        WHEN 1 MOVE "A" TO WS-OUT',
            '        WHEN 2 WHEN 3 MOVE "B" TO WS-OUT',
            '        WHEN OTHER MOVE "Z" TO WS-OUT',
            '    END-EVALUATE.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('CLASSIFY', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_search_at_end_and_when(self):
        # LOOKUP: AT END + WHEN → CCN 3; END-SEARCH adds nothing.
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. SRCH.',
            'PROCEDURE DIVISION.',
            'LOOKUP.',
            '    SEARCH WS-TABLE',
            '        AT END MOVE "X" TO WS-STATUS',
            '        WHEN WS-KEY = WS-IN MOVE "O" TO WS-STATUS',
            '    END-SEARCH.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual('LOOKUP', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_conditional_perform_until_and_varying(self):
        # DRIVE: UNTIL + VARYING + UNTIL → CCN 4.
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. LOOPY.',
            'PROCEDURE DIVISION.',
            'DRIVE.',
            '    PERFORM PROCESS-ONE UNTIL WS-DONE = "Y".',
            '    PERFORM VARYING WS-I FROM 1 BY 1',
            '        UNTIL WS-I > 5',
            '        CONTINUE',
            '    END-PERFORM.',
            '    GOBACK.',
            'PROCESS-ONE.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        by_name = {f.name: f for f in result}
        self.assertEqual(4, by_name['DRIVE'].cyclomatic_complexity)
        self.assertEqual(1, by_name['PROCESS-ONE'].cyclomatic_complexity)

    def test_unconditional_perform_and_transfers_add_nothing(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. XFER.',
            'PROCEDURE DIVISION.',
            'CALLER.',
            '    PERFORM PROCESS-ONE.',
            '    GO TO DONE-PARA.',
            '    GOBACK.',
            '    EXIT.',
            '    STOP RUN.',
            'PROCESS-ONE.',
            '    MOVE 1 TO WS-X.',
            'DONE-PARA.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        by_name = {f.name: f for f in result}
        self.assertEqual(1, by_name['CALLER'].cyclomatic_complexity)
        self.assertEqual(1, by_name['PROCESS-ONE'].cyclomatic_complexity)
        self.assertEqual(1, by_name['DONE-PARA'].cyclomatic_complexity)

    def test_else_if_elseif_and_perform_times(self):
        code = fixed([
            'IDENTIFICATION DIVISION.',
            'PROGRAM-ID. MORE.',
            'PROCEDURE DIVISION.',
            'CHOOSE.',
            '    IF WS-A = 1',
            '        MOVE 1 TO WS-O',
            '    ELSE IF WS-A = 2',
            '        MOVE 2 TO WS-O',
            '    ELSEIF WS-A = 3',
            '        MOVE 3 TO WS-O',
            '    END-IF.',
            '    PERFORM WORK-ONE 5 TIMES.',
            '    GOBACK.',
            'WORK-ONE.',
            '    GOBACK.',
        ])
        result = get_cobol_function_list(code)
        by_name = {f.name: f for f in result}
        # IF + ELSE IF + ELSEIF + TIMES → CCN 5
        self.assertEqual(5, by_name['CHOOSE'].cyclomatic_complexity)
        self.assertEqual(1, by_name['WORK-ONE'].cyclomatic_complexity)
