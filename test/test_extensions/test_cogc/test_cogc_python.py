"""Cognitive Complexity tests for Python"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_python_cogc(source_code):
    """Analyze Python code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.py", source_code
    ).function_list


class TestPythonCognitiveComplexity(unittest.TestCase):
    """Test Cognitive Complexity calculations for Python code"""

    def test_simple_function_has_zero_cogc(self):
        """Simple function with no control flow should have CogC = 0"""
        code = '''
def simple():
    x = 1
    y = 2
    return x + y
'''
        functions = get_python_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_nested_loops_should_have_nesting_penalty(self):
        """Nested loops with nested if - demonstrates nesting penalty"""
        code = '''
def nested():
    for i in range(10):        # +1
        for j in range(10):    # +2 (nesting=1)
            if i == j:         # +3 (nesting=2)
                print(i)
'''
        functions = get_python_cogc(code)
        # CogC = 6 (1+2+3) with nesting penalties
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_linear_if_statements(self):
        """Linear if statements - verify cognitive complexity is calculated"""
        code = '''
def linear():
    if condition1:
        action1()
    if condition2:
        action2()
    if condition3:
        action3()
'''
        functions = get_python_cogc(code)
        # CogC should be 3 (one for each if, no nesting)
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_elif_and_else_increments(self):
        """elif and else should increment complexity"""
        code = '''
def branching(x):
    if x > 10:
        return "big"
    elif x > 5:
        return "medium"
    elif x > 0:
        return "small"
    else:
        return "zero or negative"
'''
        functions = get_python_cogc(code)
        # CogC should be 4 (if + 2 elif + else)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_try_except_counts_catch(self):
        """try-except should count except clauses"""
        code = '''
def error_handler():
    try:
        risky_operation()
    except ValueError:  # +1
        handle_error()
'''
        functions = get_python_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_with_if(self):
        """While loop with nested if"""
        code = '''
def while_nested():
    while condition:    # +1
        if check:       # +2 (nesting=1)
            break
'''
        functions = get_python_cogc(code)
        # CogC = 3 (1+2) with nesting penalty
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operators in sequence"""
        code = '''
def logical():
    if a and b and c:   # +1 for if, +1 for && sequence
        return True
    if d or e or f:     # +1 for if, +1 for || sequence
        return False
'''
        functions = get_python_cogc(code)
        # CogC = 4 (1+1 for first if+&&, 1+1 for second if+||)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_decorator_pattern_no_nesting_penalty(self):
        """Test Python decorator pattern (purely declarative) - CogC=1"""
        code = '''
def create_wrapper(x, y):
	def wrapper(target):            # nesting = 0 (decorator pattern)
		if should_process:      # +1
			log(y)
		target()
	return wrapper                  # total = 1
'''
        functions = get_python_cogc(code)
        # CogC = 1 (decorator pattern: nested function doesn't get nesting penalty)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_non_decorator_pattern_with_nesting_penalty(self):
        """Test non-decorator pattern (has additional statements) - CogC=2"""
        code = '''
def complex_function(x, y):
	result = x*y                    # Additional statement breaks decorator pattern
	def nested(target):             # nesting = 1 (not pure decorator)
		if should_execute:      # +1 structure, +1 nesting = +2
			output(y)
		target()
	return nested                   # total = 2
'''
        functions = get_python_cogc(code)
        # CogC = 2 (not a decorator pattern, so nested function gets nesting penalty)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_nested_decorator_pattern(self):
        """Test nested decorator generator pattern - CogC=1"""
        code = '''
def create_decorator_factory(config):
	def factory(handler):
		def decorator(handler):  # nesting = 0 (nested decorator pattern)
			if is_valid:     # +1
				execute(config)
			return handler()
		return decorator
	return factory                   # total = 1
'''
        functions = get_python_cogc(code)
        # CogC = 1 (nested decorator pattern: inner function doesn't get nesting penalty)
        self.assertEqual(1, functions[0].cognitive_complexity)

 