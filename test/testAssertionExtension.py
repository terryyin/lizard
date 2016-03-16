import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardignoreassert import LizardExtension

class TestAssersionExtension(unittest.TestCase):
    def test_exclusion_of_assert(self):
        result = get_cpp_function_list_with_extnesion("void fun() { assert(a && b && c); }", LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_exclusion_of_static_assert(self):
        result = get_cpp_function_list_with_extnesion(
                "void fun() { static_assert(a && b && c, \"Failed\"); }", LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_assert_with_ternary_operator(self):
        result = get_cpp_function_list_with_extnesion(
                "void fun() { assert(a ? b : c); }",
                LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_ignore_all_code_in_assert(self):
        result = get_cpp_function_list_with_extnesion(
                """void fun() { assert(any_of(v.begin(), v.end(),
                                       [](auto& a) { return a ? b : c })); }""",
                LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)
