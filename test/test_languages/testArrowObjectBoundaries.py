"""Public API regressions for object-arrow and typed function-body boundaries."""

import unittest

from lizard import analyze_file


def summaries(filename, source):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code(
                filename, source).function_list]


class Test_object_literals_in_expression_bodied_arrows(unittest.TestCase):

    CODE = (
        "function layout(vertical) {\n"
        "  const box = (f) => vertical ? {x: f.a, y: f.b} : {x: f.b, y: f.a};\n"
        "  const pair = (a, b) => a && {a, b};\n"
        "  return box;\n"
        "}\n"
        "function after() { return 1; }\n"
    )
    EXPECTED = [
        ('box', 2, 2, 2), ('pair', 3, 3, 2),
        ('layout', 1, 5, 1), ('after', 6, 6, 1),
    ]

    def test_js_ts_jsx_tsx_keep_enclosing_and_following_separate(self):
        for filename in ("a.js", "a.ts", "a.jsx", "a.tsx"):
            self.assertEqual(
                self.EXPECTED, summaries(filename, self.CODE), filename)

    def test_vue_script_offsets_reported_object_arrow(self):
        vue = (
            "<script lang=\"ts\">\n"
            + self.CODE
            + "</script>\n"
        )
        self.assertEqual(
            [('box', 3, 3, 2), ('pair', 4, 4, 2),
             ('layout', 2, 6, 1), ('after', 7, 7, 1)],
            summaries("a.vue", vue))


class Test_object_operands_after_operators(unittest.TestCase):

    CASES = [
        ("in", "x => x in {a:1} || x"),
        ("void", "x => void {a:x} || x"),
        ("delete", "x => delete {a:x}.a || x"),
        ("typeof", "x => typeof {a:x} === \"object\" || x"),
        ("await", "async x => await {a:x} || x"),
        ("gt", "x => x > {} || x"),
        ("gte", "x => x >= {} || x"),
        ("nullish", "x => x ?? {a:x}"),
    ]

    def test_operator_object_operands_end_arrow_with_ccn(self):
        for label, arrow in self.CASES:
            code = (
                "function outer(x) {\n"
                f" const make = {arrow};\n"
                " return make;\n"
                "}\n"
                "function after() { return 1; }\n"
            )
            expected = [
                ('make', 2, 2, 2), ('outer', 1, 4, 1), ('after', 5, 5, 1),
            ]
            for filename in ("a.js", "a.ts", "a.jsx", "a.tsx"):
                self.assertEqual(
                    expected, summaries(filename, code),
                    f"{label}:{filename}")


class Test_function_type_return_keeps_real_body(unittest.TestCase):

    def test_string_template_and_boolean_function_types(self):
        cases = [
            (
                '(event: Obj) => "ok"',
                "  if (value) return () => \"ok\";\n"
                "  return () => \"ok\";\n",
                [('(anonymous)', 2, 2, 1), ('(anonymous)', 3, 3, 1),
                 ('filter', 1, 4, 2), ('after', 5, 5, 1)],
            ),
            (
                '(event: Obj) => `ok`',
                "  if (value) return () => `ok`;\n"
                "  return () => `ok`;\n",
                [('(anonymous)', 2, 2, 1), ('(anonymous)', 3, 3, 1),
                 ('filter', 1, 4, 2), ('after', 5, 5, 1)],
            ),
            (
                '(event: Obj) => boolean',
                "  return () => true;\n",
                [('(anonymous)', 2, 2, 1), ('filter', 1, 3, 1),
                 ('after', 4, 4, 1)],
            ),
        ]
        for return_type, body, expected in cases:
            code = (
                f"function filter(value: Json): {return_type} {{\n"
                f"{body}"
                "}\n"
                "function after(): void {}\n"
            )
            for filename in ("a.ts", "a.tsx"):
                self.assertEqual(
                    expected, summaries(filename, code),
                    f"{return_type}:{filename}")

    def test_vue_script_string_function_type(self):
        code = (
            "<script lang=\"ts\">\n"
            "function filter(value: Json): (event: Obj) => \"ok\" {\n"
            "  if (value) return () => \"ok\";\n"
            "  return () => \"ok\";\n"
            "}\n"
            "function after(): void {}\n"
            "</script>\n"
        )
        self.assertEqual(
            [('(anonymous)', 3, 3, 1), ('(anonymous)', 4, 4, 1),
             ('filter', 2, 5, 2), ('after', 6, 6, 1)],
            summaries("a.vue", code))


class Test_controls_remain_correct(unittest.TestCase):

    def test_block_parenthesized_and_jsx_attribute(self):
        block = (
            "function outer(x) {\n"
            " const make = x => { return {a:x} || x; };\n"
            " return make;\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        grouped = (
            "function outer(x) {\n"
            " const make = x => ({a:x}) || x;\n"
            " return make;\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        expected = [
            ('make', 2, 2, 2), ('outer', 1, 4, 1), ('after', 5, 5, 1),
        ]
        for filename in ("a.js", "a.ts"):
            self.assertEqual(expected, summaries(filename, block), filename)
            self.assertEqual(expected, summaries(filename, grouped), filename)

        jsx = (
            "function view(x) {\n"
            " return <Panel onClick={x => x ? {a:x} : {b:x}}/>;\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        jsx_expected = [
            ('(anonymous)', 2, 2, 2), ('view', 1, 3, 1), ('after', 4, 4, 1),
        ]
        for filename in ("a.jsx", "a.tsx"):
            self.assertEqual(
                jsx_expected, summaries(filename, jsx), filename)
