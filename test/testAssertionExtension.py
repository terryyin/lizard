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
