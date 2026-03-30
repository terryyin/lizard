"""Cognitive Complexity tests for St"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_st_cogc(source_code):
    """Analyze St code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.st", source_code
    ).function_list


class TestStCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Structured Text"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        FUNCTION simple : INT
            VAR_INPUT
                x : INT;
            END_VAR
            simple := x + 1;
        END_FUNCTION
        '''
        functions = get_st_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        FUNCTION check : STRING
            VAR_INPUT
                x : INT;
            END_VAR
            IF x > 0 THEN  (* +1 *)
                check := 'positive';
            ELSE
                check := 'non-positive';
            END_IF;
        END_FUNCTION
        '''
        functions = get_st_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        FUNCTION nested : INT
            VAR
                i, j : INT;
            END_VAR
            FOR i := 1 TO 10 DO           (* +1 *)
                FOR j := 1 TO 10 DO       (* +2 (nesting=1) *)
                    IF i = j THEN         (* +3 (nesting=2) *)
                        nested := i;
                    END_IF;
                END_FOR;
            END_FOR;
        END_FUNCTION
        (* Total CogC = 6 *)
        '''
        functions = get_st_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_case_statement(self):
        """Case statement should count as +1"""
        code = '''
        FUNCTION classify : STRING
            VAR_INPUT
                x : INT;
            END_VAR
            CASE x OF  (* +1 *)
                1: classify := 'one';
                2: classify := 'two';
                3: classify := 'three';
            ELSE
                classify := 'other';
            END_CASE;
        END_FUNCTION
        '''
        functions = get_st_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
