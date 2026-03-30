"""Cognitive Complexity tests for Go"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_go_cogc(source_code):
    """Analyze Go code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.go", source_code
    ).function_list


class TestGoCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Go"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
func simple() int {
    x := 5
    return x * 2
}
'''
        functions = get_go_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
func checkValue(x int) string {
    if x > 0 {              // +1
        return "positive"
    }
    return "non-positive"
}
'''
        functions = get_go_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
func nestedLoops() int {
    count := 0
    for i := 0; i < 10; i++ {       // +1
        for j := 0; j < 10; j++ {   // +2 (nesting=1)
            if i == j {              // +3 (nesting=2)
                count++
            }
        }
    }
    return count
}  // Total CogC = 6
'''
        functions = get_go_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch/case counts as 1 regardless of number of cases"""
        code = '''
func getDay(day int) string {
    switch day {            // +1
    case 1:
        return "Monday"
    case 2:
        return "Tuesday"
    case 3:
        return "Wednesday"
    default:
        return "Unknown"
    }
    return ""
}  // Total CogC = 1
'''
        functions = get_go_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Switch is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
func complexCondition(a, b, c, d, e bool) bool {
    if a && b && c {        // +1 for if, +1 for && sequence
        return true
    }
    if d || e {             // +1 for if, +1 for || sequence
        return false
    }
    return false
}  // Total CogC = 4
'''
        functions = get_go_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_select_statement(self):
        """Go select statement with case branches"""
        code = '''
func handleChannels(ch1, ch2 chan int) {
    select {
    case val := <-ch1:
        if val > 0 {        // +1
            println(val)
        }
    case val := <-ch2:
        if val > 0 {        // +1
            println(val)
        }
    }
}  // Total CogC = 2
'''
        functions = get_go_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)
