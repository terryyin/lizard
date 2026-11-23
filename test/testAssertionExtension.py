import unittest
from lizard import FileAnalyzer, get_extensions
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardignoreassert import LizardExtension

class TestAssertionExtension(unittest.TestCase):
    def test_exclusion_of_assert(self):
        result = get_cpp_function_list_with_extension("void fun() { assert(a && b && c); }", LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_exclusion_of_static_assert(self):
        result = get_cpp_function_list_with_extension(
                "void fun() { static_assert(a && b && c, \"Failed\"); }", LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_assert_with_ternary_operator(self):
        result = get_cpp_function_list_with_extension(
                "void fun() { assert(a ? b : c); }",
                LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_ignore_all_code_in_assert(self):
        result = get_cpp_function_list_with_extension(
                """void fun() { assert(any_of(v.begin(), v.end(),
                                       [](auto& a) { return a ? b : c })); }""",
                LizardExtension())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_file_after_a_file_that_ends_inside_an_assertion(self):
        analyzer = FileAnalyzer(get_extensions([LizardExtension()]))
        analyzer.analyze_source_code("cut.c", "void f(int a) {\n  assert(a &&")
        result = analyzer.analyze_source_code("c.c", """
        int both(int a, int b) {
          if (a && b) {
            return 1;
          }
          return 0;
        }
        """).function_list
        self.assertEqual(3, result[0].cyclomatic_complexity)
