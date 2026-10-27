import unittest
from lizard import analyze_file

class Test_ES6_arrow_function_in_a_branch_of_a_ternary(unittest.TestCase):

    def test_colon_of_the_ternary_ends_the_arrow_function(self):
        code = (
            "function f(a, b) {\n"
            "  const g = a ? (x) => x : (y) => y && b;\n"
            "  const h = a ? (x) => x ? 1 : 2 : null;\n"
            "  return g;\n"
            "}\n"
        )
        for filename in ("a.js", "a.ts", "a.tsx"):
            self.assertEqual(
                [('(anonymous)', 2, 2, 1), ('(anonymous)', 2, 2, 2),
                 ('(anonymous)', 3, 3, 2), ('f', 1, 5, 3)],
                [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                 for f in analyze_file.analyze_source_code(
                     filename, code).function_list], filename)
