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
