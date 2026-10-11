import unittest

from lizard import analyze_file
from lizard_languages import TypeScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_JavaScript_default_initializers(unittest.TestCase):
    def check_complexities(self, code, expected):
        functions = get_js_function_list(code)
        self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                    for f in functions])
        return functions

    def test_functions_arrows_and_methods(self):
        cases = [
            'function example(a = 1, b = 2) {}',
            'const example = (a = 1, b = 2) => a;',
            'const example = (a = 1, b = 2) => {};',
            'const obj = { example(a = 1, b = 2) {} };',
            'class Example { example(a = 1, b = 2) {} }',
        ]
        for code in cases:
            with self.subTest(code=code):
                functions = self.check_complexities(code, [('example', 3)])
                self.assertEqual(2, functions[0].parameter_count)

    def test_destructuring_parameter_defaults(self):
        cases = [
            ('function example({a = 1, b: c = 2}) {}', 3),
            ('function example([a = 1, [b = 2]]) {}', 3),
            ('function example({a: {b = 1}, c: [d = 2]} = {}) {}', 4),
            ('const example = ({a = 1} = {}) => a;', 3),
        ]
        for code, complexity in cases:
            with self.subTest(code=code):
                self.check_complexities(code, [('example', complexity)])

    def test_destructuring_inside_function(self):
        cases = [
            'const {a = 1, b: c = 2} = values;',
            'let [a = 1, [b = 2]] = values;',
            'const {a: {b = 1}, c: [d = 2]} = values;',
            'let a, b; [a = 1, b = 2] = values;',
            'let a, b; ({a = 1, b = 2} = values);',
        ]
        for statement in cases:
            with self.subTest(statement=statement):
                self.check_complexities(
                    'function example(values) { ' + statement + ' }',
                    [('example', 3)])

    def test_initializer_expression_decisions_are_counted_separately(self):
        self.check_complexities(
            'function example(a = flag ? 1 : 2, b = x || y) {}',
            [('example', 5)])

    def test_expression_assignments_are_not_defaults(self):
        cases = [
            ('function example() { const a = 1; a = 2; call(a = 3); }', 1),
            ('function example(a = (b = 1)) {}', 2),
            ('function example(a = {b: (c = 1)}) {}', 2),
            ('function example({[key = x]: a = 1}) {}', 2),
            ('function example() { const obj = {a: (b = 1)}; arr[i = 0] = 1; }', 1),
            ('function example() { _arr[i = 0] = 1; $arr[i = 0] = 1; }', 1),
        ]
        for code, complexity in cases:
            with self.subTest(code=code):
                self.check_complexities(code, [('example', complexity)])

    def test_nested_initializer_expressions_preserve_function_boundaries(self):
        self.check_complexities(
            'function example(a = makeValue(1, 2), b = 3) { if (a) {} } '
            'function after() {}',
            [('example', 4), ('after', 1)])

    def test_template_default_preserves_nested_destructuring(self):
        self.check_complexities(
            'function example({outer: {a = `${x}`} = {}, b = 2} = {}) {}',
            [('example', 5)])

    def test_nested_function_defaults_belong_to_nested_function(self):
        self.check_complexities(
            'function example(a = function inner(b = 1) {}) {}',
            [('inner', 2), ('example', 2)])

    def test_grouped_arrow_iife_preserves_function_and_defaults(self):
        self.check_complexities('const example = ((a) => a)();',
                                [('example', 1)])
        self.check_complexities('const example = ((a = 1) => a)();',
                                [('example', 2)])

    def test_catch_and_loop_destructuring_defaults(self):
        cases = [
            'try {} catch ({message = 1}) {}',
            'for (const {a = 1} of values) {}',
            'for (let [a = 1] of values) {}',
            'for ({a = 1} of values) {}',
        ]
        for statement in cases:
            with self.subTest(statement=statement):
                self.check_complexities(
                    'function example(values) { ' + statement + ' }',
                    [('example', 3)])

    def test_anonymous_arrow_defaults_belong_to_arrow(self):
        for statement in ('return (a = 1, b = 2) => a;',
                          'items.map((a = 1, b = 2) => a);',
                          'return ({a = 1} = {}) => a;'):
            with self.subTest(statement=statement):
                self.check_complexities(
                    'function example() { ' + statement + ' }',
                    [('(anonymous)', 3), ('example', 1)])

    def test_anonymous_arrow_initializer_expression_decisions(self):
        cases = [
            ('return (a = ({b = 1} = obj)) => a;', 3),
            ('return ({a = flag ? 1 : 2} = {}) => a;', 4),
            ('return (a = flag ? 1 : 2) => a;', 3),
        ]
        for statement, complexity in cases:
            with self.subTest(statement=statement):
                self.check_complexities(
                    'function example() { ' + statement + ' }',
                    [('(anonymous)', complexity), ('example', 1)])

    def test_array_assignment_defaults_in_expressions(self):
        self.check_complexities(
            'function example() { flag ? [a = 1] = values : 0; }',
            [('example', 3)])
        self.check_complexities('const example = () => [a = 1] = values;',
                                [('example', 2)])
        self.check_complexities(
            'function example() { if (flag) {} [a = 1] = values; }',
            [('example', 3)])

    def test_jsx_and_tsx_share_default_initializer_behavior(self):
        for filename in ('a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, 'const Example = ({a = 1} = {}) => <div>{a}</div>;'
                ).function_list
                self.assertEqual([('Example', 3)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])


class Test_JavaScript_nullish_coalescing(unittest.TestCase):
    def test_nullish_operators_count_once(self):
        code = (
            'function h(a, b, c) { return (a && b) ?? c ?? 0; }\n'
            'function g(o) { o.c ??= 3; return o; }\n'
            'function t(a, b, c) { return a ?? b ? c : 0; }\n'
        )
        expected = [('h', 4), ('g', 2), ('t', 3)]
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in functions])


class Test_tokenizing_JavaScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TypeScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['$a'], '$a')

    def test_tokenizing_javascript_regular_expression(self):
        self.check_tokens(['/ab/'], '/ab/')
        self.check_tokens([r'/\//'], r'/\//')
        self.check_tokens([r'/a/igm'], r'/a/igm')

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_tokenizing_javascript_regular_expression1(self):
        self.check_tokens(['a', '=', '/ab/'], 'a=/ab/')

    def test_tokenizing_javascript_comments(self):
        self.check_tokens(['/**a/*/'], '''/**a/*/''')

    def test_tokenizing_pattern(self):
        self.check_tokens([r'/\//'], r'/\//')

    def test_tokenizing_javascript_multiple_line_string(self):
        self.check_tokens(['"aaa\\\nbbb"'], '"aaa\\\nbbb"')

    def test_tokenizing_template_literal_with_expression(self):
        self.check_tokens(['`', '`hello `', '${', 'name', '}', '`'], '`hello ${name}`')

    def test_tokenizing_template_literal_multiline(self):
        self.check_tokens(['`','`hello\nworld`', '`'], '`hello\nworld`')
