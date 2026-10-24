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

    def test_existing_literals_preserve_function_ranges(self):
        expressions = ('x', '`${x}`', 'x.replace(/x/g, "y")',
                       r'x.split(/\s+/)', '/[<>"]/.test(x)', 'x.match(/`/)')
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
