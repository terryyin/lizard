"""The ")" closing "(() => {...})" or "(function () {...})" belongs
to the group, not to the block around it."""

import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_group_around_a_function_expression(unittest.TestCase):

    def summary(self, code):
        return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                for f in get_js_function_list(code)]

    def test_immediately_invoked_function_inside_a_function(self):
        for expression in ("(() => {\n    return 1;\n  })()",
                           "(function () {\n    return 1;\n  })()"):
            code = (
                "function f() {\n"
                "  const r = " + expression + ";\n"
                "  if (r) { return 2; }\n"
                "  return r;\n"
                "}\n"
                "function g() { return 1; }\n"
            )
            self.assertEqual(
                [('r', 2, 4, 1), ('f', 1, 7, 2), ('g', 8, 8, 1)],
                self.summary(code), expression)

    def test_immediately_invoked_arrow_function_in_a_class_field(self):
        code = (
            "class K {\n"
            "  y = (() => { if (a) { return 1; } return 2; })();\n"
            "  z = ((a) => a)(1);\n"
            "  m() { return 2; }\n"
            "}\n"
        )
        self.assertEqual([('y', 2, 2, 2), ('z', 3, 3, 1), ('m', 4, 4, 1)],
                         self.summary(code))

    def test_immediately_invoked_arrow_function_as_a_property_value(self):
        code = (
            "function f() {\n"
            "  const o = {\n"
            "    p: (() => { return 1; })(),\n"
            "    q: a || b,\n"
            "  };\n"
            "  return o;\n"
            "}\n"
            "function g() { return 1; }\n"
        )
        self.assertEqual([('p', 3, 3, 1), ('f', 1, 7, 2), ('g', 8, 8, 1)],
                         self.summary(code))
