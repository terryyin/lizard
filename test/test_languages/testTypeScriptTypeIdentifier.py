"""`type` starts a type alias only when a name and then '<' or '=' follow.
Used as a value, e.g. `f(type as string)`, it must not swallow the code
up to the next ';'."""

import unittest

from lizard import analyze_file


def summary(filename, code):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code(
                filename, code).function_list]


class Test_TypeScript_type_identifier(unittest.TestCase):

    def test_type_is_the_only_arrow_parameter(self):
        code = """const describe = type => {
  if (type === 'text') return 'text field'
  return 'other field'
}
function visibleTypes(types) {
  return types.filter(type => type !== 'hidden')
}
"""
        for filename in ("a.js", "a.ts", "a.jsx", "a.tsx"):
            with self.subTest(filename=filename):
                self.assertEqual(
                    [('describe', 1, 4, 2), ('(anonymous)', 6, 6, 1),
                     ('visibleTypes', 5, 7, 1)],
                    summary(filename, code))

    def test_type_at_the_end_of_an_arrow_body(self):
        code = """const typeKey = type => type
const itemKey = item => item.id
"""
        for filename in ("a.js", "a.ts", "a.jsx", "a.tsx"):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('typeKey', 1, 1, ['type']), ('itemKey', 2, 2, ['item'])],
                    [(f.name, f.start_line, f.end_line, f.parameters)
                     for f in functions])

    def test_arrow_parameter_named_type_can_be_a_callee_or_tag(self):
        for expression in ("type('label')", 'type`label`'):
            with self.subTest(expression=expression):
                code = (
                    "function render(types) {\n"
                    "  const labels = types.map(type => {\n"
                    f"    return {expression};\n"
                    "  });\n"
                    "  return labels.length ? labels : ['untitled'];\n"
                    "}\n"
                    "const getLabel = item => item.label;\n"
                )
                self.assertEqual(
                    [('(anonymous)', 2, 4, 1), ('render', 1, 6, 2),
                     ('getLabel', 7, 7, 1)],
                    summary('a.js', code))

    def test_callback_inside_schema_factory_call(self):
        code = """import { type } from 'arktype';
const person = type({
  name: type('string').pipe(value => value.trim()),
  age: 'number',
});
function parse(input: unknown) {
  return person(input);
}
"""
        functions = analyze_file.analyze_source_code('a.ts', code).function_list
        self.assertIn(('(anonymous)', 3, ['value']),
                      [(f.name, f.start_line, f.parameters) for f in functions])

    def test_typed_arrow_inside_type_call_keeps_its_parameter(self):
        code = """function type(arg: unknown) { return arg; }
const schema = type({
  parse: (value: string) => value || 'empty',
});
export function after() { return schema; }
"""
        functions = analyze_file.analyze_source_code('a.ts', code).function_list
        self.assertIn((3, ['value'], 2),
                      [(f.start_line, f.parameters, f.cyclomatic_complexity) for f in functions])
        self.assertEqual('after', functions[-1].name)

    def test_type_cast_at_the_end_of_an_arrow_body(self):
        for ext in ('ts', 'tsx'):
            for alias in ('', 'type Tag = { run: (type: string) => string }\n'):
                with self.subTest(ext=ext, alias=bool(alias)):
                    code = (
                        'const first = (type: string) => type as string\n'
                        + alias
                        + 'const next = (item?: {id: string}) => item ? item.id : null\n'
                    )
                    line = 3 if alias else 2
                    self.assertEqual(
                        [('first', 1, 1, 1), ('next', line, line, 2)],
                        summary('a.' + ext, code))

    def test_qualified_cast_continues_after_the_dot(self):
        code = """const format = (value: unknown) => value as Intl.
  DateTimeFormat
"""
        for filename in ('a.ts', 'a.tsx'):
            with self.subTest(filename=filename):
                self.assertEqual([('format', 1, 2, 1)], summary(filename, code))

    def test_qualified_cast_leaves_the_next_class_method(self):
        code = """class Parser {
  format = (value: unknown) => value as Intl.
    DateTimeFormat
  is(value: unknown) {
    return value instanceof Intl.DateTimeFormat
  }
}
"""
        for filename in ('a.ts', 'a.tsx'):
            with self.subTest(filename=filename):
                self.assertEqual(
                    [('format', 2, 3, 1), ('is', 4, 6, 1)],
                    summary(filename, code))

    def test_type_used_as_value_with_cast(self):
        code = (
            "function a() {\n"
            "  if (k.includes(type as string)) {\n"
            "  }\n"
            "}\n"
            "function z(x) { if (x) {} }\n"
        )
        for filename in ("a.ts", "a.tsx"):
            with self.subTest(filename=filename):
                self.assertEqual([('a', 1, 4, 2), ('z', 5, 5, 2)],
                                 summary(filename, code))

    def test_type_alias_still_skipped(self):
        code = (
            "type Fn = (a: number) => void;\n"
            "export type G<T> = { f: () => T };\n"
            "function z(x) { if (x) {} }\n"
        )
        self.assertEqual([('z', 3, 3, 2)], summary("a.ts", code))
