import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardoutside import LizardExtension as CountOutsideComplexity


def analyze_with_outside_extension(code):
    return get_cpp_function_list_with_extnesion(code, CountOutsideComplexity())

class Test_complexity_in_c_marco(unittest.TestCase):

    def test_no_complexity_outside_function_global_cc_should_be_one(self):
        result = analyze_with_outside_extension("")
        self.assertEqual(1, len(result))
        self.assertEqual("*global*", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_complexity_outside_should_be_counted(self):
        result = analyze_with_outside_extension("#if a==b")
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_complexity_outside_should_be_counted_when_there_is_function(self):
        result = analyze_with_outside_extension("#if a==b\n void fun() {if(1);}\n #if 1")
        self.assertEqual("*global*", result[1].name)
        self.assertEqual(3, result[1].cyclomatic_complexity)

