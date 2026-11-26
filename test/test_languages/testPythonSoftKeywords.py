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

    def test_case_capture_uses_the_same_metrics_as_other_capture_names(self):
        code = """\
def find_open_case(search):
    match search:
        case ("open", item) if item.is_open:
            if item.is_ready:
                return item
        case _:
            return None
"""
        for extensions, expected_ccn in ((("ND",), 5), (("ND", "modified"), 4)):
            with self.subTest(extensions=extensions):
                control = self._get_funcs(code, *extensions)[0]
                capture = self._get_funcs(code.replace("item", "case"), *extensions)[0]
                self.assertEqual(expected_ccn, capture.cyclomatic_complexity)
                self.assertEqual(control.max_nesting_depth, capture.max_nesting_depth)

    def test_match_subject_name_does_not_add_a_modified_branch(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def summarize_match(match):
    match match:
        case {"score": score} if score > 0:
            return score
        case _:
            return 0
'''
        functions = self._get_funcs(code, Modified())
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_match_capture_name_does_not_add_a_modified_branch(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def read_result(result):
    match result:
        case ("ok", match):
            return match
        case _:
            return None
'''
        functions = self._get_funcs(code, Modified())
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_wrapped_match_subject_does_not_add_a_modified_branch(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        from lizard_ext.lizardnd import LizardExtension as NestDepth
        code = '''
def classify(match):
    match (
        match,
    ):
        case (value,):
            return value
'''
        function, = self._get_funcs(code, Modified(), NestDepth())
        self.assertEqual((2, 1), (function.cyclomatic_complexity, function.max_nesting_depth))

    def test_wrapped_match_capture_does_not_add_a_modified_branch(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        from lizard_ext.lizardnd import LizardExtension as NestDepth
        code = '''
def read_result(result):
    match result:
        case (
            "ok",
            match,
        ):
            return match
'''
        function, = self._get_funcs(code, Modified(), NestDepth())
        self.assertEqual((2, 1), (function.cyclomatic_complexity, function.max_nesting_depth))

    def test_nested_match_after_fstring_parenthesis_fills(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def classify(value):
    match (f"{value:(>2}"):
        case _:
            match (f"{value:)>2}"):
                case _:
                    return True
'''
        function, = self._get_funcs(code, Modified())
        self.assertEqual(3, function.cyclomatic_complexity)

    def test_nested_match_after_bare_carriage_return(self):
        from lizard_ext.lizardmodified import LizardExtension as Modified
        code = '''
def classify(case, value):
    match value:
        case _:
            case
            match value:
                case _:
                    return True
'''
        code = code.replace('            case\n', '            case\r')
        functions = self._get_funcs(code, Modified())
        self.assertEqual(3, functions[0].cyclomatic_complexity)

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

    def test_case_call_keeps_the_next_match_and_case_statements(self):
        code = """\
def classify(value, case):
    match value:
        case 1:
            case(value)
            match value:
                case 2:
                    return True
        case _:
            return False
"""
        for extensions, expected_ccn in (((), 4), (('modified',), 3)):
            with self.subTest(extensions=extensions):
                function, = self._get_funcs(code, *extensions)
                self.assertEqual(expected_ccn, function.cyclomatic_complexity)

    def test_wrapped_fstring_guard_keeps_case_operand_roles(self):
        code = """\
def classify(value):
    match value:
        case (1, item) if (
            f"{item}" and item
        ):
            return item
        case _:
            return None
"""
        control, = self._get_funcs(code, 'ND')
        function, = self._get_funcs(code.replace('item', 'case'), 'ND')
        self.assertEqual(5, function.cyclomatic_complexity)
        self.assertEqual(control.max_nesting_depth, function.max_nesting_depth)

    def test_bare_case_expression_before_semicolon_is_an_identifier(self):
        code = """\
def keep_value(case):
    case; return case
"""
        function, = self._get_funcs(code)
        self.assertEqual(1, function.cyclomatic_complexity)

    def test_case_guard_keeps_explicit_crlf_continuation(self):
        source = (
            'def dispatch(value):\r\n'
            '    match value:\r\n'
            '        case 1 \\\r\n'
            '            if value:\r\n'
            '            return True\r\n'
            '        case _:\r\n'
            '            return False\r\n')
        function, = self._get_funcs(source)
        self.assertEqual(4, function.cyclomatic_complexity)
