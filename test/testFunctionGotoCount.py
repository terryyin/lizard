import unittest
from .testHelpers import get_cpp_function_list_with_extension, get_js_function_list_with_extension
from lizard import FileAnalyzer, get_extensions
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

    def test_function_without_a_token_of_its_own_count_as_0(self):
        code = """
        request.then(
            (response) => response.json(),
            () => fallback
        );
        """
        result = get_js_function_list_with_extension(code, GotoCounter())
        self.assertEqual([0, 0], [f.goto_count for f in result])

    def test_extensions_given_as_an_iterator(self):
        analyzer = FileAnalyzer(iter(get_extensions([GotoCounter()])))
        result = analyzer.analyze_source_code("a.c", "int fun(){goto label;}").function_list
        self.assertEqual([1], [f.goto_count for f in result])

    def test_extensions_given_as_an_iterator_give_the_default(self):
        code = """
        request.then(
            (response) => response.json(),
            () => fallback
        );
        """
        analyzer = FileAnalyzer(iter(get_extensions([GotoCounter()])))
        result = analyzer.analyze_source_code("a.js", code).function_list
        self.assertEqual([0, 0], [getattr(f, "goto_count", "missing") for f in result])
