import unittest

from .testPython import get_python_function_list


class Test_Python_match_case(unittest.TestCase):
    """Tests for Python 3.10+ structural pattern matching (match/case)."""

    def test_match_case_basic_complexity(self):
        """Each case arm adds +1 to cyclomatic complexity."""
        code = '''
def f(x):
    match x:
        case 1:
            return "one"
        case 2:
            return "two"
        case _:
            return "other"
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        # 1 base + 3 case arms
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_match_does_not_add_complexity_by_itself(self):
        """match without case arms only has base complexity 1."""
        code = '''
def f(x):
    match x:
        case _:
            pass
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        # 1 base + 1 case
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_match_case_with_guard(self):
        """case with an if guard is a single arm; the guard is counted as 'if'."""
        code = '''
def classify(point):
    match point:
        case (x, y) if x == y:
            return "diagonal"
        case (x, y):
            return "other"
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        # 1 base + 2 cases + 1 guard (if)
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_case_parenthesized_pattern_counted(self):
        """'case (x, y):' as a tuple pattern in a match arm must add complexity."""
        code = '''
def f(point):
    match point:
        case (x, y):
            return x + y
        case _:
            return 0
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_case_sequence_pattern_counted(self):
        """'case [0]:' as a sequence pattern in a match arm must add complexity."""
        code = '''
def f(lst):
    match lst:
        case [0]:
            return "zero"
        case _:
            return "other"
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_case_nested_brackets_in_pattern_counted(self):
        """'case (x, (y, z)):' — nested brackets in a pattern must add complexity."""
        code = '''
def f(point):
    match point:
        case (x, (y, z)):
            return x + y + z
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_case_pattern_with_guard_counted(self):
        """'case (x, y) if x > 0:' — the arm (and its guard 'if') must both add complexity."""
        code = '''
def f(point):
    match point:
        case (x, y) if x > 0:
            return x
'''
        # 1 base + 1 case arm + 1 guard 'if' = 3
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_case_pattern_guard_with_dict_literal_counted(self):
        """Guard containing a dict literal must not confuse the lookahead.

        'case (x, y) if x in {0: "a", 1: "b"}:' — the ':' tokens inside the
        dict literal are at depth > 0 and must not prematurely end the scan.
        The case arm and the guard 'if' must both be counted.
        """
        code = '''
def f(v):
    match v:
        case (x, y) if x in {0: "a", 1: "b"}:
            return x
'''
        # 1 base + 1 case arm + 1 guard 'if' = 3
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_case_sequence_pattern_with_guard_counted(self):
        """'case [x, y] if x > 0:' — sequence pattern with guard must add complexity."""
        code = '''
def f(lst):
    match lst:
        case [x, y] if x > 0:
            return x
'''
        # 1 base + 1 case arm + 1 guard 'if' = 3
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_case_multiline_pattern_counted(self):
        """A match pattern spanning two lines (implicit continuation inside brackets)
        must still be recognised and add complexity."""
        code = '''
def f(point):
    match point:
        case (x,
              y):
            return x + y
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_match_case_equivalent_complexity_to_if_elif(self):
        """match/case with N arms should match complexity of if/elif chain with N-1 elifs.

        Note: 'case _:' (wildcard/default) counts as +1 like any other case arm,
        whereas 'else:' in an if/elif chain adds 0.  So a match with a wildcard
        arm has one more complexity point than the equivalent if/elif/else chain.
        To compare fairly, omit the wildcard arm and use a plain else.
        """
        if_code = '''
def f(x):
    if x == 1:
        return "one"
    elif x == 2:
        return "two"
    elif x == 3:
        return "three"
'''
        match_code = '''
def f(x):
    match x:
        case 1:
            return "one"
        case 2:
            return "two"
        case 3:
            return "three"
'''
        if_funcs = get_python_function_list(if_code)
        match_funcs = get_python_function_list(match_code)
        # Both: 1 (base) + 3 (if/elif or case arms) = 4
        self.assertEqual(if_funcs[0].cyclomatic_complexity,
                         match_funcs[0].cyclomatic_complexity)

    def test_match_wildcard_adds_one(self):
        """'case _:' is counted as a case arm (+1), unlike 'else:' which is free."""
        code_with_wildcard = '''
def f(x):
    match x:
        case 1:
            return "one"
        case _:
            return "other"
'''
        code_with_else = '''
def f(x):
    if x == 1:
        return "one"
    else:
        return "other"
'''
        match_funcs = get_python_function_list(code_with_wildcard)
        if_funcs = get_python_function_list(code_with_else)
        # match: 1 + 2 (case 1 + case _) = 3
        self.assertEqual(3, match_funcs[0].cyclomatic_complexity)
        # if/else: 1 + 1 (if only, else is free) = 2
        self.assertEqual(2, if_funcs[0].cyclomatic_complexity)
