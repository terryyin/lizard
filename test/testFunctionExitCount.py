import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardexitcount import LizardExtension as ExitCounter

class TestFunctionExitCount(unittest.TestCase):

    def test_no_return_should_count_as_1(self):
        result = get_cpp_function_list_with_extnesion("int fun(){}", ExitCounter())
        self.assertEqual(1, result[0].exit_count)

    def test_one_return_should_count_as_1(self):
        result = get_cpp_function_list_with_extnesion("int fun(){return 0;}", ExitCounter())
        self.assertEqual(1, result[0].exit_count)

    def test_two_returns_should_count_as_2(self):
        result = get_cpp_function_list_with_extnesion("int fun(){return 0;return 1;}", ExitCounter())
        self.assertEqual(2, result[0].exit_count)

