"""Cognitive Complexity tests for Rust"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_rust_cogc(source_code):
    """Analyze Rust code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.rs", source_code
    ).function_list


class TestRustCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Rust"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        fn simple(x: i32) -> i32 {
            return x + 1;
        }
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        fn check(x: i32) -> &'static str {
            if x > 0 {  // +1
                return "positive";
            }
            return "non-positive";
        }
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        fn nested() {
            for i in 1..10 {           // +1
                for j in 1..10 {       // +2 (nesting=1)
                    if i == j {        // +3 (nesting=2)
                        println!("{}", i);
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        fn check(a: bool, b: bool, c: bool, d: bool, e: bool) -> i32 {
            if a && b && c {  // +1 for if, +1 for && sequence
                return 1;
            }
            if d || e {       // +1 for if, +1 for || sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_match_statement_counts_as_one(self):
        """Match (Rust's switch) should count as +1 total"""
        code = '''
        fn classify(x: i32) -> &'static str {
            match x {  // +1
                1 => "one",
                2 => "two",
                3 => "three",
                _ => "other",
            }
        }
        // Total CogC = 1
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        fn countdown(mut n: i32) -> i32 {
            while n > 0 {  // +1
                n = n - 1;
            }
            return n;
        }
        // Total CogC = 1
        '''
        functions = get_rust_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
