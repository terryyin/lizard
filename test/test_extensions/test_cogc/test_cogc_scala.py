"""Cognitive Complexity tests for Scala"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_scala_cogc(source_code):
    """Analyze Scala code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.scala", source_code
    ).function_list


class TestScalaCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Scala"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        def simple(x: Int): Int = {
            return x + 1
        }
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        def check(x: Int): String = {
            if (x > 0) {  // +1
                return "positive"
            }
            return "non-positive"
        }
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        def nested(): Unit = {
            for (i <- 1 to 10) {           // +1
                for (j <- 1 to 10) {       // +2 (nesting=1)
                    if (i == j) {          // +3 (nesting=2)
                        println(i)
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        def check(a: Boolean, b: Boolean, c: Boolean,
                  d: Boolean, e: Boolean): Int = {
            if (a && b && c) {  // +1 for if, +1 for && sequence
                return 1
            }
            if (d || e) {       // +1 for if, +1 for || sequence
                return 2
            }
            return 0
        }
        // Total CogC = 4
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_match_statement_counts_as_one(self):
        """Match (Scala's switch) should count as +1 total"""
        code = '''
        def classify(x: Int): String = x match {  // +1
            case 1 => "one"
            case 2 => "two"
            case 3 => "three"
            case _ => "other"
        }
        // Total CogC = 1
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        def countdown(n: Int): Int = {
            var x = n
            while (x > 0) {  // +1
                x = x - 1
            }
            return x
        }
        // Total CogC = 1
        '''
        functions = get_scala_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
