"""Cognitive Complexity tests for TTCN"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_ttcn_cogc(source_code):
    """Analyze TTCN code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.ttcn", source_code
    ).function_list


class TestTtcnCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for TTCN-3"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        module test_simple {
            function simple(integer x) return integer {
                return x + 1;
            }
        }
        '''
        functions = get_ttcn_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        module test_if {
            function check(integer x) return charstring {
                if (x > 0) {  // +1
                    return "positive";
                }
                return "non-positive";
            }
        }
        '''
        functions = get_ttcn_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        module test_nested {
            function nested() {
                for (var integer i := 0; i < 10; i := i + 1) {           // +1
                    for (var integer j := 0; j < 10; j := j + 1) {       // +2 (nesting=1)
                        if (i == j) {                                    // +3 (nesting=2)
                            log(i);
                        }
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_ttcn_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        module test_logical {
            function check(boolean a, boolean b, boolean c,
                          boolean d, boolean e) return integer {
                if (a and b and c) {  // +1 for if, +1 for 'and' sequence
                    return 1;
                }
                if (d or e) {         // +1 for if, +1 for 'or' sequence
                    return 2;
                }
                return 0;
            }
        }
        // Total CogC = 4
        '''
        functions = get_ttcn_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)
