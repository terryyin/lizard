"""A "(" after `name =` is read optimistically as arrow parameters. When the
group turns out to be an expression, the functions inside it still count;
likewise for `async`/`static` used as property names and for block
statements, whose braces hold statements rather than object members."""

import unittest

from lizard import analyze_file


def summary(filename, code):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code(
                filename, code).function_list]


class Test_JavaScript_expression_groups(unittest.TestCase):

    def check(self, code, expected):
        for filename in ("a.js", "a.ts", "a.tsx"):
            with self.subTest(filename=filename):
                self.assertEqual(expected, summary(filename, code))

    def test_grouped_async_arrow(self):
        code = (
            "x = (async () => h());\n"
            "const o = {\n"
            "  b: (async () => { if (a) { return 1; } }),\n"
            "};\n"
            "function g() { return 1; }\n"
        )
        self.check(code, [('x', 1, 1, 1), ('b', 3, 3, 2), ('g', 5, 5, 1)])

    def test_async_parameter_name_still_a_parameter(self):
        code = "const w = (async) => async + 1;\n"
        self.check(code, [('w', 1, 1, 1)])

    def test_call_in_group_keeps_its_callbacks(self):
        code = (
            "function f() {\n"
            "  const d = (\n"
            "    items.map((item) => {\n"
            "      return item || 0;\n"
            "    })\n"
            "  );\n"
            "  const e = (g((item) => item && 1));\n"
            "  return d;\n"
            "}\n"
            "function g() { return 1; }\n"
        )
        self.check(code, [('(anonymous)', 3, 5, 2), ('(anonymous)', 7, 7, 2),
                          ('f', 1, 9, 1), ('g', 10, 10, 1)])

    def test_member_types_and_decorators_stay_parameters(self):
        code = (
            "const n = (a: T.U, b = c.d()) => { return 1; };\n"
            "class C {\n"
            "  constructor(@Inject(T) private x: X, y) { }\n"
            "}\n"
        )
        self.assertEqual([('n', 1, 1, 2), ('constructor', 3, 3, 1)],
                         summary("a.ts", code))

    def test_modifier_words_as_property_names(self):
        code = (
            "class X {\n"
            "  a() {\n"
            "    x({ async: v, static: 1 });\n"
            "  }\n"
            "  b() { return 1; }\n"
            "  static async c() { return 1; }\n"
            "}\n"
            "const o = { async: true, f() { return 1; } };\n"
        )
        self.check(code, [('a', 2, 4, 1), ('b', 5, 5, 1), ('c', 6, 6, 1),
                          ('f', 8, 8, 1)])

    def test_block_statement_holds_statements(self):
        code = (
            "function t() {\n"
            "  {\n"
            "    const r = find(xs, (v) => {\n"
            "      return v ? 1 : 0;\n"
            "    });\n"
            "  }\n"
            "  for ({a = 1} of values) {}\n"
            "}\n"
        )
        self.check(code, [('(anonymous)', 3, 5, 2), ('t', 1, 8, 3)])
