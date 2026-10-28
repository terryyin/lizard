"""Arrow functions without braces over many lines, as authored in PR533."""

import unittest
from lizard import analyze_file


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_ES6_arrow_function_without_braces_on_many_lines(unittest.TestCase):
    """An arrow function without braces ends with the last line of its
    expression; the first token of the next line is not its own."""

    def summary(self, code, filename="a.js"):
        return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
                 f.nloc)
                for f in analyze_file.analyze_source_code(
                    filename, code).function_list]

    def test_lines_and_tokens_are_the_same_with_and_without_a_semicolon(self):
        with_semicolon = get_js_function_list(
            "const inc = x => x + 1;\nlet a\n")[0]
        without = get_js_function_list("const inc = x => x + 1\nlet a\n")[0]
        self.assertEqual(
            (1, 1, with_semicolon.token_count - 1),
            (without.end_line, without.nloc, without.token_count))

    def test_ended_by_a_closing_bracket_on_the_next_line(self):
        code = (
            "run(\n"
            "  x => x + 1\n"
            ")\n"
            "const rules = {\n"
            "  find: text => text.match(/a/g)\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        self.assertEqual([('(anonymous)', 2, 2, 1, 1), ('find', 5, 5, 1, 1),
                          ('after', 7, 7, 1, 1)],
                         self.summary(code))

    def test_statements_and_members_on_the_next_lines(self):
        code = (
            "const inc = x => x + 1\n"
            "const dec = x => x - 1\n"
            "if (a) { b() }\n"
            "class A {\n"
            "  handler = () => 1\n"
            "  async load() {\n"
            "    return 1;\n"
            "  }\n"
            "}\n"
        )
        for filename in ("a.js", "a.ts", "a.tsx"):
            self.assertEqual(
                [('inc', 1, 1, 1, 1), ('dec', 2, 2, 1, 1),
                 ('handler', 5, 5, 1, 1), ('load', 6, 8, 1, 3)],
                self.summary(code, filename), filename)

    def test_case_and_closing_brace_go_to_the_function_around(self):
        code = (
            "function m(key, cursor) {\n"
            "  switch (true) {\n"
            "    case key.left && key.ctrl:\n"
            "      return () => cursor.prevWord()\n"
            "    case key.right:\n"
            "      return () => cursor.nextWord()\n"
            "  }\n"
            "}\n"
            "export function register(fn) {\n"
            "  add(fn)\n"
            "  return () => remove(fn)\n"
            "}\n"
        )
        self.assertEqual(
            [('(anonymous)', 4, 4, 1, 1), ('(anonymous)', 6, 6, 1, 1),
             ('m', 1, 8, 4, 8), ('(anonymous)', 11, 11, 1, 1),
             ('register', 9, 12, 1, 4)],
            self.summary(code))

    def test_expression_that_goes_on_at_the_next_lines(self):
        code = (
            "const pick = (a, b) => a && b\n"
            "    ? a\n"
            "    : b || 0;\n"
            "const row = item => '<td>' + item.name\n"
            "  + (item.on ? 'on' : 'off') +\n"
            "  '</td>';\n"
            "const names = list => list\n"
            "  .map(x => x.name)\n"
            "  .join(', ')\n"
            "function after() { return 1; }\n"
        )
        self.assertEqual(
            [('pick', 1, 3, 4, 3), ('row', 4, 6, 2, 3),
             ('(anonymous)', 8, 8, 1, 1), ('names', 7, 9, 1, 3),
             ('after', 10, 10, 1, 1)],
            self.summary(code))
