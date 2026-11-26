import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_class_field_calls(unittest.TestCase):
    def test_conditional_field_calls_function_with_the_same_name(self):
        code = '''function build(ready) {
  return new class {
    load = ready ? load() : 0
    get result() { return this.load; }
  }().result;
}
function load() { return 2; }
'''
        functions = get_js_function_list(code)
        self.assertEqual([
            ('get result', 4, 4, [], 1),
            ('build', 1, 6, ['ready'], 2),
            ('load', 7, 7, [], 1),
        ], [(f.name, f.start_line, f.end_line, f.parameters, f.cyclomatic_complexity)
            for f in functions])

    def test_same_name_field_call_keeps_its_callback_and_the_next_field_arrow(self):
        code = '''function map(transform) { return transform(''); }
class Batch {
  map = map(value => value || 'fallback');
  decorate = (value) => value || 'default';
}
'''
        functions = get_js_function_list(code)
        self.assertEqual([
            ('map', 1, ['transform'], 1),
            ('(anonymous)', 3, ['value'], 2),
            ('decorate', 4, ['value'], 2),
        ], [(f.name, f.start_line, f.parameters, f.cyclomatic_complexity)
            for f in functions])


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
