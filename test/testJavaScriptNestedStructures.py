import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardns import LizardExtension as NestedStructure


class TestJavaScriptNestedStructures(unittest.TestCase):

    REGEX = """
    function withRegex(text) {
      if (/#/.test(text)) {
        return 1;
      }
      return 0;
    }
    """

    NESTED = """
    function nested(list) {
      for (const x of list) {
        if (x) {
          while (x) {
            x--;
          }
        }
      }
    }
    """

    def setUp(self):
        self.analyzer = FileAnalyzer(get_extensions([NestedStructure()]))

    def nested_structures(self, filename, code):
        functions = self.analyzer.analyze_source_code(filename, code).function_list
        return [(f.name, f.max_nested_structures) for f in functions]

    def test_function_with_a_regular_expression(self):
        self.assertEqual(
            [('withRegex', 1), ('nested', 3)],
            self.nested_structures("a.js", self.REGEX + self.NESTED))

    def test_file_after_a_file_with_a_regular_expression(self):
        self.nested_structures("a.js", self.REGEX)
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.js", self.NESTED))
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.ts", self.NESTED))

    def test_return_regex_then_nested_on_same_analyzer(self):
        first = (
            'function a(s) { return /#/.test(s) || s === 1; }\n'
            'function nested(list) {\n'
            '  for (const x of list) { if (x) { while (x) { x--; } } }\n'
            '}\n'
        )
        self.assertEqual(
            [('a', 0), ('nested', 3)],
            self.nested_structures("a.js", first))
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.jsx", self.NESTED))
