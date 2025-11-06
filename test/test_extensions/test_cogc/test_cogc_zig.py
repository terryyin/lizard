"""Cognitive Complexity tests for Zig"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_zig_cogc(source_code):
    """Analyze Zig code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.zig", source_code
    ).function_list


class TestZigCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Zig"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        pub fn simple(x: i32) i32 {
            return x + 1;
        }
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        pub fn check(x: i32) []const u8 {
            if (x > 0) {  // +1
                return "positive";
            }
            return "non-positive";
        }
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        pub fn nested() void {
            var i: usize = 0;
            while (i < 10) : (i += 1) {           // +1
                var j: usize = 0;
                while (j < 10) : (j += 1) {       // +2 (nesting=1)
                    if (i == j) {                 // +3 (nesting=2)
                        // do something
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        pub fn check(a: bool, b: bool, c: bool, d: bool, e: bool) i32 {
            if (a and b and c) {  // +1 for if, +1 for 'and' sequence
                return 1;
            }
            if (d or e) {         // +1 for if, +1 for 'or' sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 total"""
        code = '''
        pub fn classify(x: i32) []const u8 {
            return switch (x) {  // +1
                1 => "one",
                2 => "two",
                3 => "three",
                else => "other",
            };
        }
        // Total CogC = 1
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        pub fn countdown(n: i32) i32 {
            var x = n;
            while (x > 0) {  // +1
                x = x - 1;
            }
            return x;
        }
        // Total CogC = 1
        '''
        functions = get_zig_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
