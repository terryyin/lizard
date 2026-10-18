import unittest

from .testPython import get_python_function_list


class Test_Python_soft_keywords(unittest.TestCase):
    """Identifiers named case or match must not add complexity."""

    def test_case_variable_not_counted_as_keyword(self):
        """'case' used as a plain variable name must not add to complexity."""
        code = '''
def f():
    case = 5
    return case
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        # 1 base only; 'case = 5' is assignment, not a keyword
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_attribute_access_not_counted(self):
        """'case.something' must not be counted as a case keyword."""
        code = '''
def f(case):
    return case.value
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_member_access_not_counted(self):
        """'foo.case' must not be counted as a case keyword."""
        code = '''
def f(foo):
    return foo.case
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_annotated_assignment_not_counted(self):
        """'case: int = 5' (type-annotated variable) must not add complexity."""
        code = '''
def f():
    case: int = 5
    return case
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_tuple_unpacking_not_counted(self):
        """'case, other = 1, 2' (tuple unpacking) must not add complexity."""
        code = '''
def f():
    case, other = 1, 2
    return case + other
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_function_call_not_counted(self):
        """'case(x)' as a function call must not add complexity."""
        code = '''
def f(x):
    case(x)
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_subscript_not_counted(self):
        """'case[0]' as a subscript must not add complexity."""
        code = '''
def f(case):
    return case[0]
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_subscript_assignment_not_counted(self):
        """'case[key] = value' (subscript assignment) must not add complexity."""
        code = '''
def f(case):
    case[0] = 99
    return case
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_case_multiline_function_call_not_counted(self):
        """A multi-line function call 'case(\\n    x\\n)' must not add complexity."""
        code = '''
def f(x):
    case(
        x
    )
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_match_function_call_not_counted(self):
        """'match(x)' as a function call must not add complexity."""
        code = '''
def f(x):
    match(x)
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_match_variable_not_counted(self):
        """'match' used as a plain variable or function name adds no complexity."""
        code = '''
def f(text):
    match = re.match(r"\\d+", text)
    return match
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)


class Test_Python_match_case_with_modified_ccn(unittest.TestCase):
    """Tests for match/case interaction with the --modified CCN extension."""

    def _get_funcs(self, code, *extensions):
        from lizard import FileAnalyzer, get_extensions
        return FileAnalyzer(get_extensions(list(extensions))).analyze_source_code(
            "a.py", code).function_list

    def test_match_case_not_cancelled_by_modified_extension(self):
        """lizardmodified treats Python match/case like switch/case:
        the whole block counts as 1 (not 1 per arm)."""
        from lizard_ext.lizardnd import LizardExtension as NestDepth
        from lizard_ext.lizardmodified import LizardExtension as Modified
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
        normal = self._get_funcs(code, NestDepth())
        modified = self._get_funcs(code, NestDepth(), Modified())
        # Regular CCN: 1 base + 3 case arms
        self.assertEqual(4, normal[0].cyclomatic_complexity)
        # Modified CCN: 1 base + 1 for the whole match block (like switch/case)
        self.assertEqual(2, modified[0].cyclomatic_complexity)

    def test_case_variable_not_penalised_by_modified_extension(self):
        """'case = 5' must not drive complexity negative under lizardmodified."""
        from lizard_ext.lizardnd import LizardExtension as NestDepth
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def f():
    case = 5
    return case
'''
        modified = self._get_funcs(code, NestDepth(), Modified())
        self.assertEqual(1, modified[0].cyclomatic_complexity)

    def test_match_function_call_not_penalised_by_modified_extension(self):
        """'match(x)' as a function call must not add a block bonus under lizardmodified."""
        from lizard_ext.lizardnd import LizardExtension as NestDepth
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def f(x):
    match(x)
    return x
'''
        modified = self._get_funcs(code, NestDepth(), Modified())
        self.assertEqual(1, modified[0].cyclomatic_complexity)
