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

    def test_type_alias_with_a_non_ascii_or_prefixed_name(self):
        for name in ('$$Props', '_Props', 'Fa\u0301ctory', 'F\u200dactory',
                     'Fa\u203fctory', r'\u0046actory', r'F\u{61}ctory',
                     '\u309bFactory', 'Fa\u309bctory', r'as\u0046actory'):
            with self.subTest(name=name):
                code = f"""\
type {name} = BaseProps & {{ onClose: () => void }};
function close() {{ dialog.close(); }}
"""
                self.assertEqual([('close', 2, 2, 1)], summary('a.ts', code))

    def test_type_value_before_a_prefixed_assignment(self):
        code = """\
type
$onClose = () => { dialog.close(); };
"""
        self.assertEqual([('$onClose', 2, 2, 1)], summary('a.ts', code))

    def test_multiline_function_type_alias_keeps_following_arrow(self):
        for filename, params in (('types.ts', '<T>'), ('types.tsx', '<T,>')):
            with self.subTest(filename=filename):
                code = f"""type $Parse = {params}(
  value: T,
) => T
const parse: $Parse = (value) => value
"""
                self.assertEqual([('parse', 4, 4, 1)], summary(filename, code))

    def test_alias_intersection_keeps_following_function_expression(self):
        code = """type Matcher = { description: string } & (
  (label: string) => boolean
)
(function initialize() { return true })()
"""
        for filename in ('types.ts', 'types.tsx'):
            with self.subTest(filename=filename):
                self.assertEqual([('initialize', 4, 4, 1)], summary(filename, code))

    def test_alias_keeps_following_new_and_array_expressions(self):
        cases = [
            ('{ enabled: boolean }', 'new (class Queue { run() { return true } })()', ['run']),
            ('boolean', '[() => 1].map(run => run())', ['(anonymous)', '(anonymous)']),
        ]
        for alias_type, expression, expected_names in cases:
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(filename=filename, expression=expression):
                    code = 'type Flag = ' + alias_type + '\n' + expression
                    self.assertEqual([(name, 2, 2, 1) for name in expected_names], summary(filename, code))


    def test_type_value_with_an_escaped_type_keeps_its_arrow_boundary(self):
        for assertion in (r'as \u0046oo', r'satisfies F\u{6f}o', r'as /* type */ \u0046oo'):
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(assertion=assertion, filename=filename):
                    code = """type Foo = { ready: boolean };
function build(type: Foo) {
  const value = () => type %s;
  if (type.ready) return value;
  return value
}
""" % assertion
                    self.assertEqual([('value', 3, 3, 1), ('build', 2, 6, 2)], summary(filename, code))

    def test_contextual_type_name_keeps_following_iife(self):
        for name in ('as', 'is'):
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(name=name, filename=filename):
                    code = f"""type {name} = string;
type Result = {name}
(() => true)();
function close() {{ return 'closed'; }}
"""
                    self.assertEqual([('(anonymous)', 3, 3, 1), ('close', 4, 4, 1)], summary(filename, code))

    def test_identifier_after_alias_keeps_runtime_arrow_name(self):
        for name, reference, expected in [('fn', r'\u0066n', 'u0066n'), ('\u309bfn', '\u309bfn', 'fn')]:
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(name=reference, filename=filename):
                    code = f"""function run({name}: () => boolean) {{
  type Options = {{ id: string }}
  {reference} = () => true
  return {name}()
}}
"""
                    self.assertEqual([(expected, 3, 3, 1), ('run', 1, 5, 1)], summary(filename, code))

    def test_contextual_call_after_type_alias_keeps_its_callback(self):
        for name in ('as', 'is'):
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(name=name, filename=filename):
                    code = f"""function {name}(callback: () => boolean) {{ return callback() }}
type Result = string
{name}(() => true)
function close() {{}}
"""
                    self.assertEqual([(name, 1, 1, 1), ('(anonymous)', 3, 3, 1), ('close', 4, 4, 1)],
                                     summary(filename, code))

    def test_contextual_assignment_after_type_alias_keeps_its_name(self):
        for name in ('as', 'is'):
            for filename in ('types.ts', 'types.tsx'):
                with self.subTest(name=name, filename=filename):
                    code = f"""let {name} = () => false
type Result = string
{name} = () => true
function close() {{}}
"""
                    self.assertEqual([(name, 1, 1, 1), (name, 3, 3, 1), ('close', 4, 4, 1)], summary(filename, code))

    def test_wrapped_assertion_type_in_alias_ends_before_runtime_call(self):
        code = """function is(callback: () => boolean) { return callback() }
type Assert = (value: unknown) => asserts value
  is string
is(() => true)
"""
        self.assertEqual([('is', 1, 1, 1), ('(anonymous)', 4, 4, 1)], summary('types.ts', code))
