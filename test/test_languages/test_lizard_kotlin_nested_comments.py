import unittest
from lizard import analyze_file


class TestNestedKotlinComments(unittest.TestCase):
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
