"""Cognitive Complexity tests for GDScript"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_gdscript_cogc(source_code):
    """Analyze GDScript code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.gd", source_code
    ).function_list


class TestGdscriptCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for GDScript"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
func simple(x):
    return x + 1
        '''
        functions = get_gdscript_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
func check(x):
    if x > 0:  # +1
        return "positive"
    return "non-positive"
        '''
        functions = get_gdscript_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
func nested():
    for i in range(10):           # +1
        for j in range(10):       # +2 (nesting=1)
            if i == j:            # +3 (nesting=2)
                print(i)
# Total CogC = 6
        '''
        functions = get_gdscript_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
func check(a, b, c, d, e):
    if a and b and c:  # +1 for if, +1 for 'and' sequence
        return 1
    if d or e:         # +1 for if, +1 for 'or' sequence
        return 2
    return 0
# Total CogC = 4
        '''
        functions = get_gdscript_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
func countdown(n):
    while n > 0:  # +1
        n = n - 1
    return n
# Total CogC = 1
        '''
        functions = get_gdscript_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
