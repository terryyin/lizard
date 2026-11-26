"""Where expression-bodied arrows end across lines, and who owns the rest."""

import unittest
from lizard import analyze_file

EXTENSIONS = ('js', 'ts', 'jsx', 'tsx')


def functions_in(filename, code):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
             f.nloc, f.token_count) for f in analyze_file.analyze_source_code(
                 filename, code).function_list]


def as_vue_script(code):
    return '<script lang="ts">\n' + code + '</script>\n<template><div/></template>\n'


REPORTED = '''function m(key, cursor) {
  switch (true) {
    case key.left:
      return () => cursor.prevWord()
    case key.right:
      return () => cursor.nextWord()
  }
}
const pick = (a, b) => a && b
  ? a
  : b || 0
'''

ASYNC_FIELD = '''class A {
  handler = () => 1
  async load() {
    return 1
  }
}
function after() { return 1 }
'''


class TestStatementAfterExpressionArrow(unittest.TestCase):

    def assert_in_all_readers(self, code, expected):
        for ext in EXTENSIONS:
            with self.subTest(ext=ext):
                self.assertEqual(expected, functions_in('a.' + ext, code))

    def test_case_returned_arrows_and_multiline_ternary_body(self):
        self.assert_in_all_readers(REPORTED, [
            ('(anonymous)', 4, 4, 1, 1, 5),
            ('(anonymous)', 6, 6, 1, 1, 5),
            ('m', 1, 8, 3, 8, 33),
            ('pick', 9, 11, 4, 3, 15)])

    def test_default_label_arrow_and_label_named_object_keys(self):
        code = ('function m(k) {\n  switch (k) {\n    default:\n'
                '      return () => go()\n  }\n}\n'
                'const o = { case: 1, default: () => 2 }\n')
        self.assert_in_all_readers(code, [
            ('(anonymous)', 4, 4, 1, 1, 3),
            ('m', 1, 6, 1, 6, 18),
            ('default', 7, 7, 1, 1, 5)])

    def test_class_field_arrow_beside_following_method(self):
        self.assert_in_all_readers(ASYNC_FIELD, [
            ('handler', 2, 2, 1, 1, 4),
            ('load', 3, 5, 1, 3, 6),
            ('after', 7, 7, 1, 1, 6)])

    def test_asi_matches_semicolon_except_the_semicolon_token(self):
        with_semicolon = functions_in('a.js', 'const inc = x => x + 1;\nlet a\n')
        without = functions_in('a.js', 'const inc = x => x + 1\nlet a\n')
        self.assertEqual([('inc', 1, 1, 1, 1, 4)], with_semicolon)
        self.assertEqual([('inc', 1, 1, 1, 1, 3)], without)

    def test_postfix_increment_on_next_line_is_a_new_statement(self):
        code = 'const f = x => x\n++counter;\nfunction after() { return 1; }\n'
        self.assert_in_all_readers(code, [
            ('f', 1, 1, 1, 1, 1), ('after', 3, 3, 1, 1, 7)])

    def test_next_statement_condition_belongs_to_enclosing_function(self):
        code = ('function outer(x) {\n const f = x => x\n if (x) return f;\n}\n'
                'function after() { return 1; }\n')
        self.assert_in_all_readers(code, [
            ('f', 2, 2, 1, 1, 1), ('outer', 1, 4, 2, 4, 18),
            ('after', 5, 5, 1, 1, 7)])

    def test_blank_and_comment_lines_do_not_extend_the_arrow(self):
        code = ('const f = x => x\n\n// comment\n/* more\n */\n'
                'const g = x => x;\nfunction after() { return 1; }\n')
        self.assert_in_all_readers(code, [
            ('f', 1, 1, 1, 1, 1), ('g', 6, 6, 1, 1, 2),
            ('after', 7, 7, 1, 1, 7)])

    def test_closing_brace_on_next_line_belongs_to_enclosing_function(self):
        code = 'function register(fn) {\n  return () => remove(fn)\n}\n'
        self.assert_in_all_readers(code, [
            ('(anonymous)', 2, 2, 1, 1, 4), ('register', 1, 3, 1, 3, 10)])

    def test_expression_in_parentheses_at_the_end_of_an_arrow_body(self):
        code = ('function bind(model) {\n'
                '  const set = value => model.value = (value)\n'
                '  return set\n}\n')
        self.assert_in_all_readers(code, [
            ('set', 2, 2, 1, 1, 7), ('bind', 1, 4, 1, 4, 13)])

    def test_block_after_a_parenthesized_initializer(self):
        code = """function select(value, fallback) {
  const selected = (value || fallback)
  {
    const value = decorate(selected)
    if (value) log(value)
  }
  return selected
}"""
        self.assert_in_all_readers(code, [('select', 1, 8, 3, 8, 34)])

    def test_block_after_a_call_to_async(self):
        code = """function schedule(value) {
  const task = async(value)
  {
    if (task) track(task)
  }
  return task
}"""
        self.assert_in_all_readers(code, [('schedule', 1, 7, 2, 7, 24)])

    def test_block_after_an_arrow_body_with_a_grouped_assignment(self):
        code = """function bind(model, fallback) {
  const set = value => model.value = (value)
  {
    if (fallback) model.reset()
  }
  return set
}"""
        self.assert_in_all_readers(code, [
            ('set', 2, 2, 1, 1, 7), ('bind', 1, 7, 2, 7, 26)])

    def test_class_heritage_call_inside_an_arrow_assignment(self):
        code = """function update(Base, mixin) {
  const create = () => mixin = class extends mixin(Base) {
    pick(x) { return x ? 1 : 0 }
  }
  return create
}"""
        for ext in EXTENSIONS:
            with self.subTest(ext=ext):
                functions = analyze_file.analyze_source_code(
                    'a.' + ext, code).function_list
                self.assertEqual(
                    [('pick', 3, 3, 2, 1, 11, 1),
                     ('create', 2, 4, 1, 3, 15, 0),
                     ('update', 1, 6, 1, 4, 13, 2)],
                    [(f.name, f.start_line, f.end_line,
                      f.cyclomatic_complexity, f.nloc, f.token_count,
                      f.parameter_count) for f in functions])

    def test_computed_method_in_a_multiline_default_export(self):
        code = """export default
{
  ['='](left, right) {
    return left === right || left.equals(right)
  }
}
"""
        self.assert_in_all_readers(code, [('(anonymous)', 3, 5, 2, 3, 18)])

    def test_assignment_in_parentheses_in_an_arrow_body(self):
        code = ('function bind(model, ready) {\n'
                '  const set = value => model.value = (last = value) && ready\n'
                '  return set\n}\n')
        self.assert_in_all_readers(code, [
            ('set', 2, 2, 2, 1, 11), ('bind', 1, 4, 1, 4, 15)])

    def test_parameters_of_an_arrow_with_an_expression_in_parentheses(self):
        code = 'const set = value => model.value = (last, value) && ready\n'
        for ext in EXTENSIONS:
            with self.subTest(ext=ext):
                result = analyze_file.analyze_source_code(
                    'a.' + ext, code).function_list
                self.assertEqual([('set', 'set', ['value'])], [
                    (f.name, f.long_name, f.parameters) for f in result])

    def test_comma_separated_object_members_stay_separate(self):
        code = 'const o = {\n  a: x => x,\n  b: y => y\n}\n'
        self.assert_in_all_readers(code, [
            ('a', 2, 2, 1, 1, 2), ('b', 3, 3, 1, 1, 1)])

    def test_same_line_object_members_stay_separate(self):
        # A member's arrow owns its comma, as a statement's owns its `;`, and
        # a closer on its own line (`}` here), as before this change.
        code = ('const o = {a: x => x, b: y => y}\n'
                'function after() { return 1 }\n')
        self.assert_in_all_readers(code, [
            ('a', 1, 1, 1, 1, 2), ('b', 1, 1, 1, 1, 2),
            ('after', 2, 2, 1, 1, 6)])


class TestExpressionArrowContinuation(unittest.TestCase):

    def assert_continues(self, second_line):
        code = ('const f = x => x\n' + second_line +
                '\nfunction after() { return 1; }\n')
        for ext in EXTENSIONS:
            with self.subTest(ext=ext, second_line=second_line):
                functions = functions_in('a.' + ext, code)
                self.assertEqual([('f', 1, 2), ('after', 3, 3)],
                                 [f[:3] for f in functions])
                self.assertEqual(1, functions[1][3])

    def test_binary_operator_on_next_line(self):
        for second_line in ('  * 2;', '  < 2;', '  > 2;', '  ** 2;',
                            '  - 2;', '  instanceof Y;', '  in y;'):
            self.assert_continues(second_line)

    def test_call_subscript_member_and_tagged_template_on_next_line(self):
        for second_line in ('  (2);', '  [2];', '  `value`;', '  .y;',
                            '  ?.y;', '  ? 1 : 2;'):
            self.assert_continues(second_line)

    def test_operator_ending_previous_line(self):
        code = ('const f = x => x *\n  2\nconst g = (x) =>\n  x\n'
                'function after() { return 1; }\n')
        self.assertEqual([('f', 1, 2), ('g', 3, 4), ('after', 5, 5)],
                         [f[:3] for f in functions_in('a.ts', code)])

    def test_continued_body_owns_its_conditions_and_next_statement_does_not(self):
        code = ('function outer(a) {\n  const f = x => x &&\n    a ||\n    x\n'
                '  if (a) return f\n}\n')
        self.assertEqual([('f', 2, 4, 3, 3, 5), ('outer', 1, 6, 2, 4, 17)],
                         functions_in('a.js', code))

    def test_parenthesized_body_spans_its_lines(self):
        for end in (');', ')'):
            code = ('const f = x => (\n  x && x\n' + end +
                    '\nfunction after() { return 1; }\n')
            with self.subTest(end=end):
                self.assertEqual([('f', 1, 3, 2), ('after', 4, 4, 1)],
                                 [f[:4] for f in functions_in('a.js', code)])

    def test_block_body_spans_its_lines(self):
        for end in ('};', '}'):
            code = ('const f = x => {\n if (x) return 1;\n return 0\n' + end +
                    '\nfunction after() { return 1; }\n')
            with self.subTest(end=end):
                self.assertEqual([('f', 1, 4, 2), ('after', 5, 5, 1)],
                                 [f[:4] for f in functions_in('a.tsx', code)])

    def test_arrow_argument_closed_on_next_line(self):
        code = 'run(\n  x => x + 1\n)\nfunction after() { return 1 }\n'
        self.assertEqual([('(anonymous)', 2, 2, 1, 1, 3), ('after', 4, 4)],
                         [f if f[0] != 'after' else f[:3]
                          for f in functions_in('a.js', code)])

    def test_arrow_body_ends_on_the_same_line_as_its_callback(self):
        code = '''const columns = (length) =>
  Array.from({ length }).map((_, index) => ({
    key: String(index),
  }))
'''
        for ext in EXTENSIONS:
            with self.subTest(ext=ext):
                self.assertEqual([
                    ('(anonymous)', 2, 4, 1, 3, 12),
                    ('columns', 1, 4, 1, 2, 22)],
                    functions_in('a.' + ext, code))

    def test_generic_arguments_split_after_comma_keep_their_line(self):
        code = ('const f = (x) => foo<A,\n B>(x)\n'
                'function after() { return 1 }\n')
        for ext in ('ts', 'tsx'):
            with self.subTest(ext=ext):
                self.assertEqual([('f', 1, 2), ('after', 3, 3)],
                                 [f[:3] for f in functions_in('a.' + ext, code)])

    def test_curried_arrow_owns_its_whole_continued_body(self):
        code = 'const f = (a) => (b) => a\n  + b\nlet z = 1\n'
        self.assertEqual([('f', 1, 2, 1, 2, 11)], functions_in('a.js', code))

    def test_multiline_jsx_body_ends_with_its_closing_tag(self):
        code = ('const f = () => <div>\n  hi\n</div>\n'
                'function after() { return 1 }\n')
        for ext in ('jsx', 'tsx'):
            with self.subTest(ext=ext):
                self.assertEqual([('f', 1, 3), ('after', 4, 4)],
                                 [f[:3] for f in functions_in('a.' + ext, code)])


class TestVueScriptArrowBoundaries(unittest.TestCase):

    def test_reported_example_keeps_script_offsets(self):
        self.assertEqual([
            ('(anonymous)', 5, 5, 1, 1, 5),
            ('(anonymous)', 7, 7, 1, 1, 5),
            ('m', 2, 9, 3, 8, 33),
            ('pick', 10, 12, 4, 3, 15)],
            functions_in('a.vue', as_vue_script(REPORTED)))

    def test_class_field_arrow_beside_following_method(self):
        self.assertEqual([
            ('handler', 3, 3, 1, 1, 4),
            ('load', 4, 6, 1, 3, 6),
            ('after', 8, 8, 1, 1, 6)],
            functions_in('a.vue', as_vue_script(ASYNC_FIELD)))

    def test_multiline_continuation(self):
        code = as_vue_script(
            'const f = x => x\n  * 2;\nfunction after() { return 1; }\n')
        self.assertEqual([('f', 2, 3), ('after', 4, 4)],
                         [f[:3] for f in functions_in('a.vue', code)])
