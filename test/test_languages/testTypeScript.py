import unittest

from lizard import analyze_file
from lizard_languages import TypeScriptReader


def get_ts_function_list(source_code):
    return analyze_file.analyze_source_code("a.ts", source_code).function_list


class Test_TypeScript_default_initializers(unittest.TestCase):
    def test_typed_parameter_defaults(self):
        functions = get_ts_function_list(
            'function example(a: number = 1, b: string = "value") {}')
        self.assertEqual([('example', 3, 2)],
                         [(f.name, f.cyclomatic_complexity, f.parameter_count)
                          for f in functions])

    def test_typed_arrow_and_method_defaults(self):
        code = '''
        const example = (a: number = 1): number => a;
        class Example { method({a = 1}: Options = {}) {} }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([('example', 2), ('method', 3)],
                         [(f.name, f.cyclomatic_complexity) for f in functions])



class Test_function_boundaries(unittest.TestCase):
    def assert_spans(self, filename, code, expected):
        functions = analyze_file.analyze_source_code(filename, code).function_list
        self.assertEqual(expected, [(f.name, f.start_line, f.end_line, f.nloc)
                                    for f in functions])

    def test_return_annotation_ends_at_body_closing_brace(self):
        self.assert_spans(
            'a.ts', 'function a(): number {\n  return 1;\n}\n'
            'const k = 2;\nfunction b() {\n  return 2;\n}\n',
            [('a', 1, 3, 3), ('b', 5, 7, 3)])

    def test_adjacent_annotated_functions_have_distinct_ranges(self):
        self.assert_spans(
            'a.ts', 'function a(): number {\n  return 1;\n}\n'
            'function b(): number {\n  return 2;\n}\n',
            [('a', 1, 3, 3), ('b', 4, 6, 3)])

    def test_nested_template_preserves_following_function(self):
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                self.assert_spans(
                    filename, 'function a(x) {\n'
                    '  return `p ${x ? `q` : ""} r`;\n}\n'
                    'const k = 2;\nfunction b() {\n  return 2;\n}\n',
                    [('a', 1, 3, 3), ('b', 5, 7, 3)])

    def test_deeply_nested_template_preserves_following_function(self):
        expression = '`${`${x ? `${`q`}` : ""}`}`'
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                self.assert_spans(
                    filename, 'function a(x) {\n  return ' + expression
                    + ';\n}\nconst k = 2;\n'
                    'function b() {\n  return 2;\n}\n',
                    [('a', 1, 3, 3), ('b', 5, 7, 3)])

    def test_template_nesting_has_no_fixed_or_recursive_depth_limit(self):
        for depth in (1, 2, 5, 32, 1500):
            expression = '`${' * depth + 'x' + '}`' * depth
            for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
                with self.subTest(depth=depth, filename=filename):
                    self.assert_spans(
                        filename, 'function a(x) {\n  return ' + expression
                        + ';\n}\nfunction b() {\n  return 2;\n}\n',
                        [('a', 1, 3, 3), ('b', 4, 6, 3)])

    def test_template_strings_comments_regex_and_escapes_preserve_ranges(self):
        expressions = (
            r'''`${"}" + '{' + "`" + `q`}`''',
            r'''`${"a\\\"}" + 'b\\\'}' + `q`}`''',
            r'''`escaped \` and \${fake} ${`q`}`''',
            r'''`backslash \\${`q`}`''',
            r'''`${({a: {b: "}"}}).a.b + `q`}`''',
            r'''`${x /* } ` */ ? `yes` : `no`}`''',
            r'''`${/[}`"']/.test(x) ? `yes` : `no`}`''',
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            for expression in expressions:
                with self.subTest(filename=filename, expression=expression):
                    self.assert_spans(
                        filename, 'function a(x) {\n  return ' + expression
                        + ';\n}\nfunction b() {\n  return 2;\n}\n',
                        [('a', 1, 3, 3), ('b', 4, 6, 3)])

    def test_template_line_comment_and_multiline_text_preserve_ranges(self):
        code = ('function a(x) {\n  return `text\n${x // } `\n'
                ' ? `yes` : `no`}\nend`;\n}\n'
                'function b() {\n  return 2;\n}\n')
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                self.assert_spans(filename, code,
                                  [('a', 1, 6, 6), ('b', 7, 9, 3)])

    def test_existing_literals_preserve_function_ranges(self):
        expressions = ('x', '`${x}`', 'x.replace(/x/g, "y")',
                       r'x.split(/\s+/)', '/[<>"]/.test(x)', 'x.match(/`/)',
                       '"`" + `${x}`', "'`' + `${x}`",
                       'x /* ` */ + `${x}`')
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            for expression in expressions:
                with self.subTest(filename=filename, expression=expression):
                    self.assert_spans(
                        filename, 'function a(x) {\n  return ' + expression
                        + ';\n}\nconst k = 2;\n'
                        'function b() {\n  return 2;\n}\n',
                        [('a', 1, 3, 3), ('b', 5, 7, 3)])


class Test_TypeScript_conditional_style_arrays(unittest.TestCase):
    def test_conditional_style_array_with_object_and_following_child(self):
        code = ('const Alert = () => { return (<View style={[{ elevation: 0.4 }, '
                'mobile ? a : b ]}><Text>{t("hello")}</Text></View>); };')
        functions = analyze_file.analyze_source_code('a.tsx', code).function_list
        self.assertEqual([('Alert', 2, 0)],
                         [(f.name, f.cyclomatic_complexity, f.parameter_count)
                          for f in functions])

    def test_grouped_and_ungrouped_conditionals_preserve_function_boundaries(self):
        for filename in ('a.jsx', 'a.tsx'):
            for conditional in ('mobile ? a : b', '(mobile ? a : b)'):
                with self.subTest(filename=filename, conditional=conditional):
                    code = ('const Alert = () => {\n'
                            '  return (<View style={[{ elevation: 0.4 }, '
                            + conditional + ']}><Text>{t("hello")}</Text></View>);\n'
                            '};\n'
                            'function after(value) { return value || 0; }')
                    functions = analyze_file.analyze_source_code(
                        filename, code).function_list
                    self.assertEqual(
                        [('Alert', 2, 0, 1, 3), ('after', 2, 1, 4, 4)],
                        [(f.name, f.cyclomatic_complexity, f.parameter_count,
                          f.start_line, f.end_line) for f in functions])


class Test_tokenizing_TypeScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TypeScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_simple(self):
        self.check_tokens(['abc?'], 'abc?')

    def test_nested_template_literal_exact_tokens(self):
        source_code = 'output.push(`${`${n}: `.padStart(w)}${s}`);'
        expected_tokens = [
            'output', '.', 'push', '(', '`', '${',
            '`${n}: `.padStart(w)', '}', '${', 's', '}', '`', ')', ';'
        ]
        self.check_tokens(expected_tokens, source_code)

    def test_quoted_braces_stay_inside_the_interpolation_token(self):
        expression = r'''"}" + '{' + "a\\\"}" + 'b\\\'}' + `q`'''
        self.check_tokens(['`', '${', expression, '}', '`'],
                          '`${' + expression + '}`')

    def test_escaped_interpolation_is_text_and_backslash_parity_matters(self):
        self.check_tokens(['`', r'`\${x}`', '`'], r'`\${x}`')
        self.check_tokens(['`', r'`\\`', '${', 'x', '}', '`'], r'`\\${x}`')
        self.check_tokens(['`', r'`a\`b`', '${', 'x', '}', '`'], r'`a\`b${x}`')

    def test_unterminated_nested_template_finishes_without_recursion(self):
        source = '`${' * 1500 + 'x'
        self.check_tokens([source], source)

    def test_token_class_receives_original_source_offsets(self):
        source = 'return `${x}`;'
        positions = []

        def token_class(match):
            self.assertIs(source, match.string)
            positions.append((match.start(), match.end()))
            return match.group()

        self.assertEqual(list(TypeScriptReader.generate_tokens(source)),
                         list(TypeScriptReader.generate_tokens(
                             source, token_class=token_class)))
        self.assertIn((7, 13), positions)
