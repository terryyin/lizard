import unittest
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardexitcount import LizardExtension as ExitCounter

class TestFunctionExitCount(unittest.TestCase):

    def test_no_return_should_count_as_1(self):
        result = get_cpp_function_list_with_extension("int fun(){}", ExitCounter())
        self.assertEqual(1, result[0].exit_count)

    def test_one_return_should_count_as_1(self):
        result = get_cpp_function_list_with_extension("int fun(){return 0;}", ExitCounter())
        self.assertEqual(1, result[0].exit_count)

    def test_two_returns_should_count_as_2(self):
        result = get_cpp_function_list_with_extension("int fun(){return 0;return 1;}", ExitCounter())
        self.assertEqual(2, result[0].exit_count)

    def test_three_returns_should_count_as_3(self):
        # Using the example from: https://ator1699.home.xs4all.nl/Work/GBS/Doc_and_download/doc/metrics.html#STM19
        code = """
void f( int a )
{
    return;             /* 1 */
    if ( a )
        return;         /* 2 */
    a++;
    return;             /* 3 */
}"""
        result = get_cpp_function_list_with_extension(code, ExitCounter())
        self.assertEqual(3, result[0].exit_count)
