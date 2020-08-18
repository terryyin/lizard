import unittest
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardgotocount import LizardExtension as GotoCounter

class TestFunctionGotoCount(unittest.TestCase):

    def test_empty_function_should_count_as_0(self):
        result = get_cpp_function_list_with_extension("int fun(){}", GotoCounter())
        self.assertEqual(0, result[0].goto_count)

    def test_function_name_contains_goto_count_as_0(self):
        result = get_cpp_function_list_with_extension("int funGoTo(){return 0;}", GotoCounter())
        self.assertEqual(0, result[0].goto_count)

    def test_function_with_goto_count_as_1(self):
        result = get_cpp_function_list_with_extension("int fun(){goto label;return 1;}", GotoCounter())
        self.assertEqual(1, result[0].goto_count)

