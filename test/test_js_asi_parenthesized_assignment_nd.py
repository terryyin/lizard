"""ND after a parenthesized assignment that ends without a semicolon.

When `name = (…)` ends at a line break and the next line starts with a
structure keyword, that keyword's nesting depth belongs to the enclosing
function — the same as after an explicit `;`.
"""

import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth
from lizard_ext.lizardns import LizardExtension as NestStruct


class TestParenthesizedAssignmentAsiNestingDepth(unittest.TestCase):

    def test_minimal_if_after_parenthesized_assignment(self):
        self.assertEqual(1, self.nesting_depth("""
        function f(m) {
          r = (m < 2)
          if (r) w()
        }
        """))

    def test_nested_structures_keep_full_depth(self):
        self.assertEqual(2, self.nesting_depth("""
        function f(m) {
          r = (m < 2)
          if (r) { if (m) { w() } }
        }
        """))

    def test_while_after_parenthesized_assignment(self):
        self.assertEqual(1, self.nesting_depth("""
        function f(m) {
          r = (m < 2)
          while (r) w()
        }
        """))

    def test_typescript_same_as_javascript(self):
        self.assertEqual(1, self.nesting_depth("""
        function f(m) {
          r = (m < 2)
          if (r) w()
        }
        """, "a.ts"))

    def test_js_yaml_handle_yaml_directive_shape(self):
        code = """
        function handleYamlDirective(state, name, args) {
          var minor
          if (!state) return
          if (!name) return
          if (!args) return
          if (name !== "YAML") return
          minor = args[0]
          state.checkLineBreaks = (minor < 2)
          if (minor !== 1 && minor !== 2) {
            return
          }
        }
        """
        functions = self.analyze(code)
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].max_nesting_depth)
        self.assertEqual(7, functions[0].cyclomatic_complexity)

    def test_multiline_parameter_arrow_remains_a_function(self):
        functions = self.analyze("""
        function g() {
          x = (a,
           b) => { if (a) w() }
        }
        """)
        by_name = {f.name: f for f in functions}
        self.assertIn('x', by_name)
        self.assertEqual(1, by_name['x'].max_nesting_depth)
        self.assertEqual(0, by_name['g'].max_nesting_depth)

    def test_typed_arrow_remains_a_function(self):
        functions = self.analyze("""
        function g() {
          x = (a): number => { if (a) w() }
        }
        """, "a.ts")
        by_name = {f.name: f for f in functions}
        self.assertIn('x', by_name)
        self.assertEqual(1, by_name['x'].max_nesting_depth)
        self.assertEqual(0, by_name['g'].max_nesting_depth)

    def test_plain_statement_between_keeps_nd(self):
        self.assertEqual(1, self.nesting_depth("""
        function f(m) {
          r = (m < 2)
          w()
          if (r) w()
        }
        """))

    def test_minimal_ccn_and_ns_unchanged(self):
        code = """
        function f(m) {
          r = (m < 2)
          if (r) w()
        }
        """
        functions = FileAnalyzer(
            get_extensions([NestDepth(), NestStruct()])
        ).analyze_source_code("a.js", code).function_list
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(1, functions[0].max_nested_structures)

    def analyze(self, code, filename="a.js"):
        return FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            filename, code).function_list

    def nesting_depth(self, code, filename="a.js"):
        return self.analyze(code, filename)[0].max_nesting_depth


if __name__ == '__main__':
    unittest.main()
