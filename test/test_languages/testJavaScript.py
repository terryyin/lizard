import unittest

from lizard import FileAnalyzer, analyze_file, get_extensions
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags
from lizard_ext.lizardmodified import LizardExtension as Modified
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


class Test_JavaScript_keyword_as_the_name_of_a_member(unittest.TestCase):
    def test_keyword_after_a_dot_is_not_a_condition(self):
        code = (
            'function a(p, q) {\n'
            '  p.catch(() => null);\n'
            '  q?.catch(() => null).finally(() => 1);\n'
            '  return p.if + p.for + p.while + p.case;\n'
            '}\n'
            'function b(p) { return p.then(f).catch(g) || 0; }\n'
        )
        expected = [('(anonymous)', 1), ('(anonymous)', 1), ('(anonymous)', 1),
                    ('a', 1), ('b', 2)]
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in functions])

    def test_comment_between_the_dot_and_the_keyword(self):
        code = (
            'function a(p) { if (p) p./* note */catch(g); }\n'
            'function b(p) {\n'
            '  if (p) p?. // note\n'
            '    catch(g);\n'
            '}\n'
            'function c(o) { o.elseif(g); }\n'
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 2), ('b', 2), ('c', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_keyword_after_a_number_and_a_dot(self):
        code = (
            'function a() { return 1..catch(g); }\n'
            'function b(o) { return `${o. /* note */ catch(g)}`; }\n'
            'function c(o) { return `${o?.\n  catch(g)}`; }\n'
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 1), ('b', 1), ('c', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_ternary_after_a_decimal_point_is_a_condition(self):
        code = (
            'function a(c) { return 1. ? c : 0; }\n'
            'function b(c) { return `${1. ? c : 0}`; }\n'
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 2), ('b', 2)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_keyword_after_a_decimal_point_is_a_structure(self):
        code = (
            'function a(x) { 1.\n'
            'if (x) g(); }\n'
            'function b(p) { if (p) 1 .catch(g); }\n'
            'function c(p) { if (p) 1.5.catch(g); }\n'
        )
        analyzer = FileAnalyzer(get_extensions(
            ['cognitive', 'nd', ComplexTags()]))
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('a', 2, 1, 1, [['if', 2]]), ('b', 2, 1, 1, [['if', 3]]),
                     ('c', 2, 1, 1, [['if', 4]])],
                    [(f.name, f.cyclomatic_complexity, f.cognitive_complexity,
                      f.max_nesting_depth, f.complex_tags)
                     for f in functions])

    def test_keyword_after_a_decimal_point_in_a_template_is_a_condition(self):
        code = (
            'function a(x) { return `${(() => { 1.\n'
            'if (x) g(); })()}`; }\n'
            'function b(p) { return `${p && 1.5.catch(g)}`; }\n'
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('a', 2), ('b', 2)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_keyword_after_a_dot_is_no_case_or_switch(self):
        code = (
            'function pick(list, key) {\n'
            '  return list.switch(key);\n'
            '}\n'
            'function label() {\n'
            '  return this.case;\n'
            '}\n'
            'function load(p) {\n'
            '  if (p) { p.catch(f); }\n'
            '}\n'
        )
        analyzer = FileAnalyzer(get_extensions([Modified(), ComplexTags()]))
        functions = analyzer.analyze_source_code('a.js', code).function_list
        self.assertEqual([('pick', 1, []), ('label', 1, []),
                          ('load', 2, [['if', 8]])],
                         [(f.name, f.cyclomatic_complexity, f.complex_tags)
                          for f in functions])

    def test_method_named_like_a_keyword(self):
        code = '''
class Deferred {
  catch(onRejected) {
    if (onRejected) { return this.then(null, onRejected); }
    return this;
  }
  static finally(promise) { return promise; }
  async if(x) { return x; }
  get switch() { return this.state; }
  static { registry.add(this); }
  other() { return 1; }
  finally(done) { if (done) { done(); } return this; }
}
const thenable = {
  catch(e) { if (e) { return 1; } return 2; },
  *while(x) { yield x; },
  ok() { return 1; }
};
'''
        expected = [('catch', 2, 1), ('finally', 1, 0), ('if', 1, 0),
                    ('get switch', 1, 0), ('other', 1, 0), ('finally', 2, 1),
                    ('catch', 2, 1), ('while', 1, 0), ('ok', 1, 0)]
        analyzer = FileAnalyzer(get_extensions(['ns']))
        for filename, source in (('a.js', code), ('a.ts', code),
                                 ('a.jsx', code), ('a.tsx', code),
                                 ('a.vue', '<script>' + code + '</script>'),
                                 ('a.svelte', '<script>' + code + '</script>')):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, source).function_list
                # A .svelte file also reports its component.
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity,
                                             f.max_nested_structures)
                                            for f in functions
                                            if f.name != '(component)'])

    def test_generic_method_named_like_a_keyword(self):
        code = '''
function wrap(promise) {
  return {
    catch<T>(onRejected: (e: unknown) => T) {
      if (onRejected) { return promise.catch(onRejected); }
      return promise;
    }
  };
}
'''
        for filename in ('a.ts', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('catch', 2), ('wrap', 1)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_key_named_like_a_keyword_is_no_structure(self):
        code = '''
function options(a) {
  return { if: a, for: a, while: a, case: a, do: a, else: a };
}
function read({ catch: onError, finally: done }) {
  const { try: attempt, switch: choose } = onError;
  return attempt && done(choose);
}
class Flags {
  catch = (e) => e;
  switch: boolean;
}
'''
        analyzer = FileAnalyzer(get_extensions(
            ['cognitive', 'nd', 'ns', ComplexTags()]))
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            source = code if filename.startswith('a.t') else code.replace(
                '  switch: boolean;\n', '')
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, source).function_list
                self.assertEqual(
                    [('options', 1, 0, 0, 0, []),
                     ('read', 2, 1, 1, 0, [['&&', 7]]),
                     ('catch', 1, 0, 0, 0, [])],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity, f.max_nesting_depth,
                      f.max_nested_structures, f.complex_tags)
                     for f in functions])

    def test_key_named_function(self):
        code = '''
function load(content) {
  const { function: wrapper, sourceMapURL } = compile(content);
  if (sourceMapURL) {
    cache(wrapper);
  }
}
class Coverage {
  summary(options) {
    const totals = { function: options.functions, line: options.lines };
    if (options.empty) { return totals; }
    return totals;
  }
  cleanup() { return 1; }
}
function call({ function: fn, args }) {
  if (fn) { return fn(args); }
}
'''
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('load', 2, 2, 7), ('summary', 2, 9, 13),
                     ('cleanup', 1, 14, 14), ('call', 2, 16, 18)],
                    [(f.name, f.cyclomatic_complexity, f.start_line,
                      f.end_line) for f in functions])

    def test_function_expression_as_a_default_value_is_no_method(self):
        code = '''
const { run = async function (x) { return x; }, id } = options;
const { stop = flag ? async function (x) { return x; } : noop } = options;
const { wait = (async function (x) { return x; })(1), rest } = options;
const hooks = { ready: 1, async function(x) { return x; } };
'''
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('run', 2), ('stop', 3), ('wait', 4), ('function', 5)],
                    [(f.name, f.start_line) for f in functions])

    def test_structure_at_the_start_of_a_block_is_no_method(self):
        code = '''
function close(stream) {
  try { stream.end(); } catch { if (stream.destroy) { stream.destroy(); } }
}
function pick(kind) {
  switch (kind) {
    case 1: { if (kind) { return 1; } break; }
    default: { while (kind) { kind--; } }
  }
}
function drain(queue) {
  try { queue.run(); } finally { for (;;) { queue.pop(); } }
}
function flush(queue) {
  try { queue.run(); } finally { { if (queue.left) { queue.pop(); } } }
}
function release(x) {
  try { x.run(); } finally { cleanup: { if (!x) break cleanup; x.close(); } }
}
function resume(stop, x) {
  if (stop) return
  { if (x) { run(); } }
}
function* produce(x) {
  yield
  { if (x) { run(); } }
}
function build(flag) {
  return class { static { load(); if (flag) { setup(); } } };
}
function connect(host, retries) {
  const address = { interface: host, port: 80 };
  if (host) { open(address); }
  while (retries--) { open(address); }
}
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('close', 3, 3, 2, 2), ('pick', 4, 5, 2, 2),
                     ('drain', 2, 1, 1, 2), ('flush', 2, 1, 1, 2),
                     ('release', 2, 2, 1, 2), ('resume', 3, 2, 1, 1),
                     ('produce', 2, 1, 1, 1), ('build', 2, 1, 1, 1),
                     ('connect', 3, 2, 1, 1)],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity, f.max_nesting_depth,
                      f.max_nested_structures) for f in functions])

    def test_structure_in_a_block_after_a_line_break(self):
        code = '''
function check(x: unknown) {
  x!
  { if (x) { work(); } }
}
function freeze(x: boolean) {
  const values = [1] as const
  { if (x) { work(); } }
}
function assert(of: unknown) {
  of!
  { if (of) { work(); } }
}
function pick(factory: Factory) {
  const make = factory.of
  { if (make) { make(); } }
}
function take(of: number) {
  let n = of
  { if (n) { work(); } }
}
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        for filename in ('a.ts', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('check', 2, 1, 1, 1), ('freeze', 2, 1, 1, 1),
                     ('assert', 2, 1, 1, 1), ('pick', 2, 1, 1, 1),
                     ('take', 2, 1, 1, 1)],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity, f.max_nesting_depth,
                      f.max_nested_structures) for f in functions])

    def test_structure_in_a_block_after_a_regular_expression(self):
        code = '''
function match(x) {
  const re = /a/
  { if (x) { re.test(x); } }
}
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('match', 2, 1, 1, 1)],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity, f.max_nesting_depth,
                      f.max_nested_structures) for f in functions])

    def test_structure_in_a_block_after_a_regular_expression_operand(self):
        code = '''
function match(x) {
  const re = "" + /a/
  { if (x) { re.test(x); } }
}
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyzer.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('match', 2, 1, 1, 1)],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity, f.max_nesting_depth,
                      f.max_nested_structures) for f in functions])

    def test_object_method_after_a_variable_named_static(self):
        code = '''
function pick(static) {
  const { a = static ?? { run() {} } } = {};
}
'''
        for filename in ('a.js', 'a.jsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('run', 1), ('pick', 2)],
                    [(f.name, f.cyclomatic_complexity) for f in functions])

    def test_function_in_a_static_block(self):
        code = '''
class Event {
  static {
    check = make((value) => value !== null && value.ok);
    trusted = describe({
      get isTrusted() { return this !== null && this.trusted; },
    }).get;
  }
  catch(e) { return e; }
}
'''
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(
                    [('(anonymous)', 2), ('get isTrusted', 2), ('catch', 1)],
                    [(f.name, f.cyclomatic_complexity) for f in functions])


class Test_JavaScript_line_comment_ending_with_a_backslash(unittest.TestCase):
    def test_line_comment_ends_at_its_line(self):
        code = (
            "function b(code) {\n"
            "  return (\n"
            "    code === 47 || // /\n"
            "    code === 92 || // \\\n"
            "    code === 45 || // -\n"
            "    code === 32\n"
            "  )\n"
            "}\n"
        )
        for filename in ('a.js', 'a.ts', 'a.jsx', 'a.tsx'):
            with self.subTest(filename=filename):
                functions = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual([('b', 1, 8, 4, 8)],
                                 [(f.name, f.start_line, f.end_line,
                                   f.cyclomatic_complexity, f.nloc)
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
