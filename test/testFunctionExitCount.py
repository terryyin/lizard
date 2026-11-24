import unittest
from .testHelpers import get_cpp_function_list_with_extension, get_js_function_list_with_extension
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardoutside import LizardExtension as OutsideCounter
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

    def test_function_without_a_token_of_its_own_counts_as_1(self):
        code = """
        request.then(
            (response) => response.json(),
            () => fallback
        );
        """
        result = get_js_function_list_with_extension(code, ExitCounter())
        self.assertEqual([1, 1], [f.exit_count for f in result])

    def test_perl_forward_declaration_counts_as_1(self):
        code = """
        sub parse;

        sub run {
            my ($self) = @_;
            return parse($self);
        }
        """
        result = FileAnalyzer(get_extensions([ExitCounter()])).analyze_source_code("a.pl", code).function_list
        self.assertEqual([("parse", 1), ("run", 1)], [(f.name, f.exit_count) for f in result])

    def test_global_code_of_an_empty_file_counts_as_1(self):
        result = FileAnalyzer(get_extensions([ExitCounter(), OutsideCounter()])).analyze_source_code("a.js", "").function_list
        self.assertEqual([("*global*", 1)], [(f.name, f.exit_count) for f in result])
