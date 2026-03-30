"""Cognitive Complexity tests for R"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_r_cogc(source_code):
    """Analyze R code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.r", source_code
    ).function_list


class TestRCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for R"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        simple <- function(x) {
            return(x + 1)
        }
        '''
        functions = get_r_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        check <- function(x) {
            if (x > 0) {  # +1
                return("positive")
            }
            return("non-positive")
        }
        '''
        functions = get_r_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        nested <- function() {
            for (i in 1:10) {           # +1
                for (j in 1:10) {       # +2 (nesting=1)
                    if (i == j) {       # +3 (nesting=2)
                        print(i)
                    }
                }
            }
        }
        # Total CogC = 6
        '''
        functions = get_r_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        check <- function(a, b, c, d, e) {
            if (a && b && c) {  # +1 for if, +1 for && sequence
                return(1)
            }
            if (d || e) {       # +1 for if, +1 for || sequence
                return(2)
            }
            return(0)
        }
        # Total CogC = 4
        '''
        functions = get_r_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        countdown <- function(n) {
            while (n > 0) {  # +1
                n <- n - 1
            }
            return(n)
        }
        # Total CogC = 1
        '''
        functions = get_r_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)


if __name__ == '__main__':
    unittest.main()
