"""Functions inside JSX child expressions, `<p>{...}</p>`: the braces hold an
expression, never an object literal, and the text around them is not code."""

import unittest

from lizard import analyze_file


def summary(filename, code):
    return [(f.name, f.start_line, f.end_line)
            for f in analyze_file.analyze_source_code(
                filename, code).function_list]


class Test_JSX_child_expressions(unittest.TestCase):

    def check(self, code, expected):
        for filename in ("a.tsx", "a.jsx"):
            with self.subTest(filename=filename):
                self.assertEqual(expected, summary(filename, code))

    def test_render_prop_with_parenthesized_body(self):
        code = (
            "const A = () => (\n"
            "  <Field>\n"
            "    {(inputProps) => (\n"
            "      <Input {...inputProps} />\n"
            "    )}\n"
            "  </Field>\n"
            ");\n"
            "function g() { return 1; }\n"
        )
        self.check(code, [('(anonymous)', 3, 5), ('A', 1, 7), ('g', 8, 8)])

    def test_render_prop_with_bare_parameter(self):
        code = (
            "const A = () => (\n"
            "  <Bundle>\n"
            "    {Component => (\n"
            "      <Component />\n"
            "    )}\n"
            "  </Bundle>\n"
            ");\n"
        )
        self.check(code, [('(anonymous)', 3, 5), ('A', 1, 7)])

    def test_attribute_arrow_inside_conditional_element(self):
        code = (
            "const A = () => (\n"
            "  <div>\n"
            "    {open && (\n"
            "      <button onClick={() => close(m)}>x</button>\n"
            "    )}\n"
            "    {open ? <i onClick={() => close(m)}>x</i> : null}\n"
            "  </div>\n"
            ");\n"
        )
        self.check(code, [('(anonymous)', 4, 4), ('(anonymous)', 6, 6),
                          ('A', 1, 8)])

    def test_hash_in_element_text_is_text(self):
        code = (
            "class A {\n"
            "  render() {\n"
            "    const title = <>#{this.title()}</>;\n"
            "    return <p># {x}</p>;\n"
            "  }\n"
            "  other() { return 2; }\n"
            "}\n"
        )
        self.check(code, [('render', 2, 5), ('other', 6, 6)])
