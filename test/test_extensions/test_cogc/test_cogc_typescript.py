"""Cognitive Complexity tests for TypeScript"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_typescript_cogc(source_code):
    """Analyze TypeScript code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.ts", source_code
    ).function_list


class TestTypescriptCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for TypeScript"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
function simple(): number {
    const x = 5;
    return x * 2;
}
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
function checkValue(x: number): string {
    if (x > 0) {                // +1
        return "positive";
    }
    return "non-positive";
}
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
function nestedLoops(): number {
    let count = 0;
    for (let i = 0; i < 10; i++) {          // +1
        for (let j = 0; j < 10; j++) {      // +2 (nesting=1)
            if (i === j) {                   // +3 (nesting=2)
                count++;
            }
        }
    }
    return count;
}  // Total CogC = 6
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch/case counts as 1 regardless of number of cases"""
        code = '''
function getDay(day: number): string {
    switch (day) {              // +1
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
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Switch is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
function complexCondition(a: boolean, b: boolean, c: boolean,
                         d: boolean, e: boolean): boolean {
    if (a && b && c) {          // +1 for if, +1 for && sequence
        return true;
    }
    if (d || e) {               // +1 for if, +1 for || sequence
        return false;
    }
    return false;
}  // Total CogC = 4
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_async_await_structure(self):
        """TypeScript async/await doesn't add complexity"""
        code = '''
async function fetchData(url: string): Promise<string> {
    const response = await fetch(url);
    if (response.ok) {          // +1
        return await response.text();
    }
    throw new Error("Failed");
}  // Total CogC = 1
'''
        functions = get_typescript_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
