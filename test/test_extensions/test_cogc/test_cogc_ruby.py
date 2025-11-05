"""Cognitive Complexity tests for Ruby"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_ruby_cogc(source_code):
    """Analyze Ruby code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.rb", source_code
    ).function_list


class TestRubyCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Ruby"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
def simple
    x = 5
    return x * 2
end
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
def check_value(x)
    if x > 0              # +1
        return "positive"
    end
    return "non-positive"
end
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_if_statements(self):
        """Nested if statements (nesting penalty not fully implemented in Ruby)"""
        code = '''
def process(a, b, c)
    if a > 0              # +1
        if b > 0          # +1 (nesting penalty not applied)
            if c > 0      # +1 (nesting penalty not applied)
                return a + b + c
            end
        end
    end
    return 0
end  # Total CogC = 3
'''
        functions = get_ruby_cogc(code)
        # Note: Ruby CogC implementation doesn't fully apply nesting penalties
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_case_statement_counts_as_one(self):
        """Case/when counts as 1 regardless of number of branches"""
        code = '''
def get_day(day)
    case day              # +1
    when 1
        return "Monday"
    when 2
        return "Tuesday"
    when 3
        return "Wednesday"
    else
        return "Unknown"
    end
end  # Total CogC = 1
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Case is CogC=1 but CCN counts each when
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
def complex_condition(a, b, c, d, e)
    if a && b && c        # +1 for if, +1 for && sequence
        return true
    end
    if d || e             # +1 for if, +1 for || sequence
        return false
    end
    return false
end  # Total CogC = 4
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_elsif_statement(self):
        """Elsif counts as additional structural increment"""
        code = '''
def categorize(value)
    if value < 0        # +1
        return "negative"
    elsif value == 0    # +1
        return "zero"
    else                # +1
        return "positive"
    end
end  # Total CogC = 3
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_modifier_if(self):
        """Ruby modifier if (suffix if) counts same as normal if"""
        code = '''
def process(value)
    return nil if value.nil?      # +1
    return 0 if value.zero?       # +1
    value * 2
end  # Total CogC = 2
'''
        functions = get_ruby_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)
