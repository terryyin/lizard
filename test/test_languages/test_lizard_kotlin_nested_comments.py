import unittest
from lizard import analyze_file


class TestNestedKotlinComments(unittest.TestCase):
    def test_adjacent_nested_comment_after_delimiter_inside_comment(self):
        for delimiter, declaration in (
                ('"', 'val s = "x"'),
                ('"""', 'val s = """x"""'),
                ("'", "val s = 'x'"),
                ('`', 'val `x` = 1')):
            with self.subTest(delimiter=delimiter):
                source = (
                    f'fun f() {{\n/* {delimiter} */\n'
                    '/* outer /* inner */ if (hidden) {} */\n'
                    f'{declaration}\nif (real) {{}}\n}}\nfun g() {{}}'
                )
                result = analyze_file.analyze_source_code('sample.kt', source)
                self.assertEqual(
                    [('f', 2, 4, 1), ('g', 1, 1, 7)],
                    [(function.name, function.cyclomatic_complexity,
                      function.nloc, function.start_line)
                     for function in result.function_list],
                )
                self.assertEqual(5, result.nloc)

    def test_nested_comments_do_not_add_decisions(self):
        for comment in ('/* outer /* inner */ if (fake) {} */',
                        '/* outer /* one /* two */ end */ if (fake) {} */',
                        '/* outer\n/* inner */\nif (fake) {}\n*/'):
            with self.subTest(comment=comment):
                source = ('fun f() {\n' + comment +
                          '\n if (real) {}\n}\nfun g() {}')
                result = analyze_file.analyze_source_code('sample.kt', source)
                self.assertEqual([('f', 2), ('g', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in result.function_list])
                self.assertEqual(4, result.nloc)
                self.assertEqual(source.count('\n') + 1,
                                 result.function_list[1].start_line)

    def test_comment_markers_in_literals_do_not_hide_code(self):
        samples = [
            'val text = "/*"',
            'val text = "\\\"/*"',
            'val text = """/*"""',
            'val `/*` = 1',
            "val character = '/'",
            '// /* line comment\n val text = 1',
        ]
        for declaration in samples:
            with self.subTest(declaration=declaration):
                source = ('fun f() { ' + declaration +
                          '; if (real) {} }\nfun g() {}')
                functions = analyze_file.analyze_source_code(
                    'sample.kt', source).function_list
                self.assertEqual([('f', 2), ('g', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_quote_in_one_comment_does_not_hide_the_next_comment(self):
        samples = [
            ('/* " */', 'val s = "x"'),
            ('/* """ */', 'val s = """x"""'),
            ("/* ' */", "val c = 'x'"),
            ('/* ` */', 'val `x` = 1'),
        ]
        for first, literal in samples:
            with self.subTest(first=first):
                source = ('fun f() {\n' + first + '\n'
                          '/* outer /* inner */ if (hidden) {} */\n' +
                          literal + '\nif (real) {}\n}\nfun g() {}')
                result = analyze_file.analyze_source_code('sample.kt', source)
                f, g = result.function_list
                self.assertEqual(('f', 2, 4), (f.name, f.cyclomatic_complexity, f.nloc))
                self.assertEqual(('g', 1, 7), (g.name, g.cyclomatic_complexity, g.start_line))
                self.assertEqual(5, result.nloc)

    def test_raw_string_with_trailing_quotes_does_not_hide_a_comment(self):
        source = ('fun f() {\nval s = """x""""\n'
                  '/* outer /* inner */ if (hidden) {} */\n'
                  'val t = "y"\nif (real) {}\n}\nfun g() {}')
        functions = analyze_file.analyze_source_code(
            'sample.kt', source).function_list
        self.assertEqual([('f', 2), ('g', 1)],
                         [(f.name, f.cyclomatic_complexity) for f in functions])
