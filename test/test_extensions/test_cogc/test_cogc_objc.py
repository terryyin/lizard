"""Cognitive Complexity tests for ObjC"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_objc_cogc(source_code):
    """Analyze ObjC code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.m", source_code
    ).function_list


class TestObjcCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Objective-C"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        - (int)simple:(int)x {
            return x + 1;
        }
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        - (NSString *)check:(int)x {
            if (x > 0) {  // +1
                return @"positive";
            }
            return @"non-positive";
        }
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        - (void)nested {
            for (int i = 0; i < 10; i++) {           // +1
                for (int j = 0; j < 10; j++) {       // +2 (nesting=1)
                    if (i == j) {                    // +3 (nesting=2)
                        NSLog(@"%d", i);
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        - (int)check:(BOOL)a b:(BOOL)b c:(BOOL)c d:(BOOL)d e:(BOOL)e {
            if (a && b && c) {  // +1 for if, +1 for && sequence
                return 1;
            }
            if (d || e) {       // +1 for if, +1 for || sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 total"""
        code = '''
        - (NSString *)classify:(int)x {
            switch (x) {  // +1
                case 1:
                    return @"one";
                case 2:
                    return @"two";
                case 3:
                    return @"three";
                default:
                    return @"other";
            }
        }
        // Total CogC = 1
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        - (int)countdown:(int)n {
            while (n > 0) {  // +1
                n = n - 1;
            }
            return n;
        }
        // Total CogC = 1
        '''
        functions = get_objc_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)