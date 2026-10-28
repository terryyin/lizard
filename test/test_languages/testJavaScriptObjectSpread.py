import unittest

from lizard import analyze_file


class Test_JavaScript_spread_in_an_object(unittest.TestCase):

    def test_functions_in_a_spread_expression(self):
        code = (
            "const props = {\n"
            "  ...(cond && { f: () => { return 1; } }),\n"
            "  ...(other ? { g(x) { return x; } } : {}),\n"
            "  ...make(() => { return a || b; }),\n"
            "  h: 2,\n"
            "};\n"
            "function after(a) { return a; }\n"
        )
        for filename in ("a.js", "a.ts", "a.tsx"):
            self.assertEqual(
                [('f', 2, 2, 1), ('g', 3, 3, 1), ('(anonymous)', 4, 4, 2),
                 ('after', 7, 7, 1)],
                [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                 for f in analyze_file.analyze_source_code(
                     filename, code).function_list], filename)
