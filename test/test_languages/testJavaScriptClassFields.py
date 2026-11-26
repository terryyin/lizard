import unittest

from lizard import analyze_file

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

    def test_multiline_field_keeps_grouped_constructor_callback(self):
        code = '''
class Metric {
  constructor(normalize) { this.normalize = normalize; }
  valueOf() { return this.normalize(0); }
}
class Dashboard {
  score = 1
    + new (Metric)(value => value || 3);
  read() { return this.score; }
}
'''
        functions = get_js_function_list(code)
        self.assertEqual(
            [('constructor', 1, ['normalize']), ('valueOf', 1, []),
             ('(anonymous)', 2, ['value']), ('read', 1, [])],
            [(f.name, f.cyclomatic_complexity, f.parameters) for f in functions])

    def test_async_value_on_the_next_line_is_still_the_field_value(self):
        code = (
            "class A {\n"
            "  handler =\n"
            "    async () => { return 1; }\n"
            "}\n"
        )
        self.assertEqual(['handler'],
                         [f.name for f in get_js_function_list(code)])

    def test_direct_field_keeps_grouped_constructor_callback(self):
        source = '''class Panel {
  direct = new (Meter)(input => input || 4);
  read() { return this.direct; }
}
'''
        functions = get_js_function_list(source)
        self.assertEqual(
            [('(anonymous)', 2, ['input'], 2), ('read', 3, [], 1)],
            [(f.name, f.start_line, f.parameters, f.cyclomatic_complexity)
             for f in functions])

    def test_continued_field_keeps_nested_generic_constructor_callback(self):
        source = '''class Chart {
  series = 1
    + Number(new (Metric)<number>(value => value || 2).normalize(0));
  read() { return this.series; }
}
'''
        functions = analyze_file.analyze_source_code('sample.ts', source).function_list
        self.assertEqual(
            [('(anonymous)', 3, ['value'], 2), ('read', 4, [], 1)],
            [(f.name, f.start_line, f.parameters, f.cyclomatic_complexity)
             for f in functions])
