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
            '`${`', '$', '{', 'n', '}', ':', ' ', '`',
            '`.padStart(w)}`', '${', 's', '}', '`', ')', ';'
        ]
        self.check_tokens(expected_tokens, source_code)
