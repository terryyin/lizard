"""Cognitive Complexity tests for Swift"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_swift_cogc(source_code):
    """Analyze Swift code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.swift", source_code
    ).function_list


class TestSwiftCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Swift"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
func simple() -> Int {
    let x = 5
    return x * 2
}
'''
        functions = get_swift_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
func checkValue(x: Int) -> String {
    if x > 0 {                  // +1
        return "positive"
    }
    return "non-positive"
}
'''
        functions = get_swift_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
func nestedLoops() -> Int {
    var count = 0
    for i in 1...10 {               // +1
        for j in 1...10 {           // +2 (nesting=1)
            if i == j {              // +3 (nesting=2)
                count += 1
            }
        }
    }
    return count
}  // Total CogC = 6
'''
        functions = get_swift_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch/case counts as 1 regardless of number of cases"""
        code = '''
func getDay(day: Int) -> String {
    switch day {                // +1
    case 1:
        return "Monday"
    case 2:
        return "Tuesday"
    case 3:
        return "Wednesday"
    default:
        return "Unknown"
    }
}  // Total CogC = 1
'''
        functions = get_swift_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Switch is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
func complexCondition(a: Bool, b: Bool, c: Bool,
                     d: Bool, e: Bool) -> Bool {
    if a && b && c {            // +1 for if, +1 for && sequence
        return true
    }
    if d || e {                 // +1 for if, +1 for || sequence
        return false
    }
    return false
}  // Total CogC = 4
'''
        functions = get_swift_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_guard_statement(self):
        """Guard statement counts as structural increment"""
        code = '''
func validate(input: String?) -> Bool {
    guard let value = input else {  // +1
        return false
    }
    guard value.count > 0 else {    // +1
        return false
    }
    return true
}  // Total CogC = 2
'''
        functions = get_swift_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)

