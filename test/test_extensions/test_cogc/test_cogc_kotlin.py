"""Cognitive Complexity tests for Kotlin"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_kotlin_cogc(source_code):
    """Analyze Kotlin code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.kt", source_code
    ).function_list


class TestKotlinCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Kotlin"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
fun simple(): Int {
    val x = 5
    return x * 2
}
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
fun checkValue(x: Int): String {
    if (x > 0) {                // +1
        return "positive"
    }
    return "non-positive"
}
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
fun nestedLoops(): Int {
    var count = 0
    for (i in 1..10) {              // +1
        for (j in 1..10) {          // +2 (nesting=1)
            if (i == j) {            // +3 (nesting=2)
                count++
            }
        }
    }
    return count
}  // Total CogC = 6
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_when_expression_counts_as_one(self):
        """When expression counts as 1 regardless of number of branches"""
        code = '''
fun getDay(day: Int): String {
    return when (day) {         // +1
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        else -> "Unknown"
    }
}  // Total CogC = 1
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # When is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
fun complexCondition(a: Boolean, b: Boolean, c: Boolean,
                     d: Boolean, e: Boolean): Boolean {
    if (a && b && c) {          // +1 for if, +1 for && sequence
        return true
    }
    if (d || e) {               // +1 for if, +1 for || sequence
        return false
    }
    return false
}  // Total CogC = 4
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_elvis_operator(self):
        """Elvis operator (?:) adds to CCN but not CogC"""
        code = '''
fun checkValue(value: String?): String {
    val result = value ?: "default"     // CCN +1, CogC +0
    return result
}  // Total CogC = 0, CCN = 2
'''
        functions = get_kotlin_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)
        self.assertEqual(2, functions[0].cyclomatic_complexity)
