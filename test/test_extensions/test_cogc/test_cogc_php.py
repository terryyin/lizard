"""Cognitive Complexity tests for PHP"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_php_cogc(source_code):
    """Analyze PHP code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.php", source_code
    ).function_list


class TestPhpCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for PHP"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
<?php
function simple() {
    $x = 5;
    return $x * 2;
}
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
<?php
function checkValue($x) {
    if ($x > 0) {               // +1
        return "positive";
    }
    return "non-positive";
}
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
<?php
function nestedLoops() {
    $count = 0;
    for ($i = 0; $i < 10; $i++) {       // +1
        for ($j = 0; $j < 10; $j++) {   // +2 (nesting=1)
            if ($i == $j) {              // +3 (nesting=2)
                $count++;
            }
        }
    }
    return $count;
}  // Total CogC = 6
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch/case counts as 1 regardless of number of cases"""
        code = '''
<?php
function getDay($day) {
    switch ($day) {             // +1
        case 1:
            return "Monday";
        case 2:
            return "Tuesday";
        case 3:
            return "Wednesday";
        default:
            return "Unknown";
    }
}  // Total CogC = 1
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Switch is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
<?php
function complexCondition($a, $b, $c, $d, $e) {
    if ($a && $b && $c) {       // +1 for if, +1 for && sequence
        return true;
    }
    if ($d || $e) {             // +1 for if, +1 for || sequence
        return false;
    }
    return false;
}  // Total CogC = 4
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_catch_exception_handler(self):
        """Catch blocks count as structural increments"""
        code = '''
<?php
function safeDivide($a, $b) {
    try {
        return $a / $b;
    } catch (DivisionByZeroError $e) {  // +1
        return null;
    } catch (Exception $e) {            // +1
        return 0;
    }
}  // Total CogC = 2
?>
'''
        functions = get_php_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)
