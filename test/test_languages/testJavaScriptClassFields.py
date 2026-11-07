import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_member_modifier_after_a_field(unittest.TestCase):
    """A field without ';' ends at the line of the next member's modifier."""

    def test_methods_with_modifiers_after_fields_without_semicolons(self):
        code = (
            "class A {\n"
            "  count = 0\n"
            "  async load(x) {\n"
            "    return x;\n"
            "  }\n"
            "  total = count + 1\n"
            "  static create() {\n"
            "    return new A();\n"
            "  }\n"
            "  name = 'a'\n"
            "  get value() {\n"
            "    return this.count;\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(
            [('load', 3, 5, 1), ('create', 7, 9, 0), ('get value', 11, 13, 0)],
            [(f.name, f.start_line, f.end_line, f.parameter_count)
             for f in get_js_function_list(code)])

    def test_async_value_on_the_next_line_is_still_the_field_value(self):
        code = (
            "class A {\n"
            "  handler =\n"
            "    async () => { return 1; }\n"
            "}\n"
        )
        self.assertEqual(['handler'],
                         [f.name for f in get_js_function_list(code)])
