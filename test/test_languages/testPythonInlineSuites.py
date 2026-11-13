"""Inline function suites must retain native metrics and function boundaries."""
import unittest

from lizard import analyze_file


class TestPythonInlineSuites(unittest.TestCase):
    def functions(self, source):
        return analyze_file.analyze_source_code('inline.py', source).function_list

    def test_inline_function_at_eof_with_or_without_newline(self):
        for ending in ('', '\n'):
            with self.subTest(ending=ending):
                functions = self.functions('def value(): return 1' + ending)
                self.assertEqual(['value'], [f.name for f in functions])
                self.assertEqual((1, 1, 1, 1), (
                    functions[0].start_line, functions[0].end_line,
                    functions[0].nloc, functions[0].cyclomatic_complexity))

    def test_protocol_methods_and_decorators_are_not_dropped(self):
        functions = self.functions(
            'class Stream(Protocol):\n'
            '    def read(self, size: int = -1, /) -> str: ...\n'
            '    @property\n'
            '    def path(self) -> Path | None: ...\n'
            '    async def close(self) -> None: ...\n')
        self.assertEqual(['read', 'path', 'close'], [f.name for f in functions])
        self.assertEqual([2, 4, 5], [f.start_line for f in functions])
        self.assertEqual([1, 1, 1], [f.cyclomatic_complexity for f in functions])

    def test_inline_and_block_siblings_have_independent_metrics(self):
        functions = self.functions(
            'def first(value): return 1 if value else 2\n'
            'def second(value):\n'
            '    return value and True\n'
            'def third(): pass\n'
            'module_value = 1 if condition else 2\n')
        self.assertEqual(['first', 'second', 'third'], [f.name for f in functions])
        self.assertEqual([2, 2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([1, 3, 4], [f.end_line for f in functions])

    def test_nested_inline_function_preserves_outer_function(self):
        functions = self.functions(
            'def outer(value):\n'
            '    def inner(flag): return 1 if flag else 2\n'
            '    return inner(value) if value else 0\n')
        self.assertEqual(['outer.inner', 'outer'], [f.name for f in functions])
        self.assertEqual([2, 2], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([2, 3], [f.end_line for f in functions])

    def test_nested_inline_function_at_eof_is_finalized_before_outer(self):
        functions = self.functions('def outer():\n    def inner(): ...')
        self.assertEqual(['outer.inner', 'outer'], [f.name for f in functions])
        self.assertEqual([1, 1], [f.cyclomatic_complexity for f in functions])

    def test_multiline_signature_with_inline_suite(self):
        functions = self.functions(
            'def value(\n'
            '    flag: bool,\n'
            ') -> int: return 1 if flag else 2\n')
        self.assertEqual(['value'], [f.name for f in functions])
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(3, functions[0].end_line)

    def test_inline_semicolons_and_comments(self):
        functions = self.functions(
            'def value(flag): x = flag and 1; return x  # trailing comment\n'
            '# next line\n'
            'def empty(): ...\n')
        self.assertEqual(['value', 'empty'], [f.name for f in functions])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in functions])

    def test_inline_parenthesized_body_continues_at_column_zero(self):
        functions = self.functions(
            'def value(flag): return (\n'
            '1 if flag else 2)\n'
            'def next_value(): return 3\n')
        self.assertEqual(['value', 'next_value'], [f.name for f in functions])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([2, 3], [f.end_line for f in functions])

    def test_explicit_continuation_keeps_the_inline_function_open(self):
        functions = self.functions(
            'def value(flag): return ' + '\\' + '\n'
            '1 if flag else 2\n'
            'def next_value(): return 3\n')
        self.assertEqual(['value', 'next_value'], [f.name for f in functions])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([2, 3], [f.end_line for f in functions])

    def test_fstring_interpolation_does_not_change_suite_boundaries(self):
        functions = self.functions(
            'def value(flag): return f"{[1] if flag else [2]}"\n'
            'def next_value(): return 3\n')
        self.assertEqual(['value', 'next_value'], [f.name for f in functions])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([1, 2], [f.end_line for f in functions])

    def test_nested_inline_suite_inside_a_branch_closes_before_dedent(self):
        functions = self.functions(
            'def outer(flag):\n'
            '    if flag:\n'
            '        def inner(): return 1\n'
            '    return 1 if flag else 2\n'
            'def next_value(): return 3\n')
        self.assertEqual(['outer.inner', 'outer', 'next_value'],
                         [f.name for f in functions])
        self.assertEqual([1, 3, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([3, 4, 5], [f.end_line for f in functions])

    def test_multiline_fstring_interpolation_keeps_nested_function_open(self):
        functions = self.functions(
            'def outer(flag):\n'
            '    def inner(): return f"""{\n'
            '1 if flag else 2\n'
            '}"""\n'
            '    return inner() if flag else 0\n'
            'def next_value(): pass\n')
        self.assertEqual(['outer.inner', 'outer', 'next_value'],
                         [f.name for f in functions])
        self.assertEqual([2, 2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual(5, functions[1].end_line)
