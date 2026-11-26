import unittest

from lizard import analyze_file


class Test_conditions_in_template_interpolations(unittest.TestCase):
    """An interpolation is one token for the states; its conditions count."""

    def test_story_label_reports_ccn_3_on_each_extension(self):
        code = (
            "function label(x, y) {\n"
            "  return `v: ${x ? 1 : 2} ${x && y}`;\n"
            "}\n"
        )
        for filename in ('label.js', 'label.ts', 'label.jsx', 'label.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('label', 3)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_conditions_in_interpolations(self):
        code = (
            "function a(x, y) {\n"
            "  return `v: ${x ? 1 : 2} ${x && y || 0}`;\n"
            "}\n"
            "function b(x, y) {\n"
            "  return `v: ${x ? `w: ${y ? 1 : 2}` : ''}`;\n"
            "}\n"
            "function c(x) {\n"
            "  return `if (x) ${'&& ?'} ${x /* || */}`;\n"
            "}\n"
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 4), ('b', 3), ('c', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_nested_follow_and_literal_controls(self):
        cases = (
            ("function a(x) {\n  return `${x ? `n:${x}` : ''}`;\n}\n"
             "function b() {\n  return 2;\n}\n",
             [('a', 2), ('b', 1)]),
            ("function a(x) {\n"
             "  return `if (x) ${'&& ?'} ${x /* || */} ${/\\?/.test(x) ? 1 : 0}`;\n"
             "}\nfunction b() {\n  return 2;\n}\n",
             [('a', 2), ('b', 1)]),
            ("function a() {\n  return `no interpolation`;\n}\n",
             [('a', 1)]),
            ("function a(x) {\n  return `${x.replace(/\\?/g, '') && x}`;\n}\n",
             [('a', 2)]),
            ("function a(x) {\n  return `${x // ?\n} ${x && 1}`;\n}\n",
             [('a', 2)]),
            ("function outside(x, y) {\n"
             "  return (x ? 1 : 2) + (x && y ? 1 : 0);\n}\n",
             [('outside', 4)]),
        )
        for code, expected in cases:
            for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
                with self.subTest(filename=filename, expected=expected):
                    functions = analyze_file.analyze_source_code(
                        filename, code).function_list
                    self.assertEqual(
                        expected,
                        [(f.name, f.cyclomatic_complexity) for f in functions])

    def test_keyword_member_inside_interpolation_is_not_a_condition(self):
        code = "function a(p) { return `${p.catch(x)}`; }\n"
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])
