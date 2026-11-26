import unittest

from lizard import FileAnalyzer, analyze_file, get_extensions


class TestJavaScriptRegex(unittest.TestCase):
    extensions = ('js', 'ts', 'jsx', 'tsx')
    script_extensions = ('js', 'ts')

    def assert_functions(self, source, expected, extensions=None):
        for extension in self.extensions if extensions is None else extensions:
            with self.subTest(extension=extension):
                functions = analyze_file.analyze_source_code(
                    'a.' + extension, source).function_list
                self.assertEqual(expected, [
                    (f.name, f.start_line, f.end_line,
                     f.cyclomatic_complexity) for f in functions])

    def test_quote_in_regex_preserves_following_function(self):
        source = (
            'function a(x: string): boolean {\n'
            '  const p = /[<>"]/;\n'
            '  return p.test(x);\n'
            '}\n'
            'function b(x: string): boolean {\n'
            '  const s = "str";\n'
            '  return x === s;\n'
            '}\n'
        )
        for extension in self.extensions:
            with self.subTest(extension=extension):
                functions = analyze_file.analyze_source_code(
                    'a.' + extension, source).function_list
                self.assertEqual([('a', 1, 1), ('b', 5, 1)], [
                    (f.name, f.start_line, f.cyclomatic_complexity)
                    for f in functions])
                self.assertEqual(8, functions[1].end_line)

    def test_regex_contents_do_not_count_as_conditions(self):
        patterns = (
            r'''/[<>"]/''',
            r"/[<>']/",
            r'''/["']/''',
            r'''/["'/?]|\//gimu''',
            r'''/a "b" && c\/?/''',
        )
        contexts = ('const p = %s;', 'const p = [%s][0];',
                    'const p = x.match(%s);')
        for pattern in patterns:
            for context in contexts:
                with self.subTest(pattern=pattern, context=context):
                    source = (
                        'function a(x) {\n'
                        '  ' + context % pattern + '\n'
                        '  return p.test(x);\n'
                        '}\n'
                        'function b(x) {\n'
                        '  const s = "str";\n'
                        '  return x === s || x === null;\n'
                        '}\n'
                    )
                    self.assert_functions(
                        source, [('a', 1, 4, 1), ('b', 5, 8, 2)])

    def test_division_comments_and_strings_keep_conditions(self):
        source = (
            'function a(x, y) {\n'
            '  const q = x / "2" / y + x / \'2\' / y'
            ' + x / `2` / y + x[0] / y;\n'
            '  const s = "/[<>\\\"]/";\n'
            '  // /[<>\"]/? && ||\n'
            '  /* /[<>\"]/ ? && || */\n'
            '  return q > 1 && s.length > 0;\n'
            '}\n'
            'function b() { return "str"; }\n'
        )
        self.assert_functions(source, [('a', 1, 7, 2), ('b', 8, 8, 1)])

    def test_whitespace_before_regex_preserves_lines(self):
        source = (
            'function a(x) {\n'
            '  const p =\n'
            '    /[<>"]/;\n'
            '  return p.test(x);\n'
            '}\n'
            'function b() { return "str"; }\n'
        )
        self.assert_functions(source, [('a', 1, 5, 1), ('b', 6, 6, 1)])

    def test_regex_after_comments_preserves_function_complexity(self):
        cases = (
            ('block', """function matches(s) {
              const ok = /* spelling */ /colou?r/.test(s);
            }""", 1, 0),
            ('line', """function matches(s) {
              return (
                // spelling
                /colou?r/.test(s) && s.length > 2
              );
            }""", 2, 1),
            ('mixed', """function matches(s, found, total) {
              return found / total > 0 && // require a match
                /* spelling */ /colou?r/.test(s);
            }""", 2, 1),
            ('prefix', """function matches(s) {
              return ! /* exclude other spellings */ /colou?r/.test(s);
            }""", 1, 0),
            ('nonbreaking space', """async function matches(s) {
              const ok = /* spelling */\u00a0/colou?r/.test(s);
            }""", 1, 0),
            ('byte order mark', """function matches(s) {
              const ok = (
                // spelling
                \ufeff/colou?r/.test(s)
              );
            }""", 1, 0),
        )
        analyzer = FileAnalyzer(get_extensions(['cognitive']))
        for extension in self.script_extensions:
            for name, source, ccn, cognitive in cases:
                with self.subTest(extension=extension, comment=name):
                    functions = analyzer.analyze_source_code(
                        'a.' + extension, source).function_list
                    self.assertEqual([('matches', ccn, cognitive)], [
                        (f.name, f.cyclomatic_complexity, f.cognitive_complexity)
                        for f in functions])

    def test_regex_after_comment_in_webpack_header(self):
        source = r'''function removeRaster(options, name, trimSvgValue, xmlAttributeValue, attributes, href) {
          if (
            options.removeSvgRasterImages === true &&
            name === "image" &&
            // A data URL's media type, or else the path's extension, says what it is.
            /^(?:data:image\/(?:jpe?g|png|gif)[;,]|(?!data:)[^?#]*\.(?:jpe?g|png|gif)(?:[?#]|$))/i.test(
              trimSvgValue(xmlAttributeValue(
                /** @type {number} */ (attributes.get("href") || (href === undefined ? 0 : href))
              ))
            )
          ) return true;
        }'''
        self.assert_functions(source, [('removeRaster', 1, 12, 6)], self.script_extensions)

    def test_division_continues_after_comments(self):
        self.assert_ccn_and_tokens("""
            function divide(a, b, c) { return a /* note */ / b / c; }
        """, 1, 16)
        self.assert_ccn_and_tokens("""
            function divide(x, y, z) {
              x = y // note
                / 2 / (z || 1);
              return x;
            }
        """, 2, 24)
        self.assert_ccn_and_tokens("""function convert(total, packs, size) {
          return total / packs /* https://example.org/units */
            / (size || 1) / 100;
        }""", 2, 22)

    def test_arrow_regex_after_division_and_comment(self):
        source = """function build(total, count, text) {
          return total / count > 0 && (() => /* spelling */ /colou?r/.test(text))();
        }"""
        self.assert_functions(source, [('(anonymous)', 2, 2, 1), ('build', 1, 3, 2)], self.script_extensions)

    def test_jsx_url_does_not_hide_a_child_expression(self):
        source = '''function EndpointExample(usePreview: boolean) {
          return <pre>
            https://api.example.com
            /{usePreview ? "preview" : "v1"}/users
          </pre>;
        }'''
        self.assert_functions(source, [('EndpointExample', 1, 6, 2)], ('tsx',))

    def test_url_in_division_operand_stays_in_its_string(self):
        for quote in ('"', "'", '`'):
            with self.subTest(quote=quote):
                source = f"""function convert(total, scale) {{
                  return total / {quote}https://example.org/units{quote}.length
                    / (scale || 1) / 100;
                }}"""
                self.assert_functions(source, [('convert', 1, 4, 2)])

    def test_member_name_before_line_comment_still_allows_division(self):
        source = """function divide(obj, a, b, c) {
          return obj. // member
            return // divide this value
            / (a || b) / c;
        }"""
        self.assert_ccn_and_tokens(source, 2, 24)

    def test_private_and_unicode_names_still_allow_division(self):
        source = r"""class Share {
          #return = 120;
          scale(a, b, c) {
            const share = this.#return // validated
              / (a || b) / c;
            return share;
          }
        }
        function normalize(a, b, c) {
          const πin = a;
          const \u{03c0}instanceof = b;
          const mass = πin // validated
            / (a || b) / c;
          return \u{03c0}instanceof // validated
            / (mass || 1) / c;
        }"""
        self.assert_functions(source, [('scale', 3, 7, 2), ('normalize', 9, 16, 3)])

    def test_non_null_assertion_before_comment_still_allows_division(self):
        source = """function divide(value: number | undefined, divisor: number) {
          return value! /* use the validated value */ / (divisor || 1) / 3;
        }"""
        for extension in ('ts', 'tsx'):
            with self.subTest(extension=extension):
                functions = analyze_file.analyze_source_code(
                    'a.' + extension, source).function_list
                self.assertEqual([2], [f.cyclomatic_complexity for f in functions])

    def test_braceless_if_regex_keeps_following_conditions(self):
        self.assert_functions('''function collect(source, found, count) {
          if (source) /return/.test(source) && found.push(source); return (found.length || 1) / count;
        }''', [('collect', 1, 3, 4)])
        self.assert_functions('''function accepts(s) {
          if (s) /["]/.test(s); return /colou?r/.test(s);
        }''', [('accepts', 1, 3, 2)])

    def test_module_regex_keeps_following_function(self):
        for opener in ('export default', 'await'):
            with self.subTest(opener=opener):
                source = opener + ' /["]/; export function matches(s) { return /colou?r/.test(s); }'
                functions = analyze_file.analyze_source_code('a.mjs', source).function_list
                self.assertEqual([('matches', 1)], [
                    (f.name, f.cyclomatic_complexity) for f in functions])

    def test_regex_in_class_heritage_keeps_method_body(self):
        source = (
            'class QuotePattern extends /return/.constructor {'
            ' static match(s) { return matches(s) && s.length / 2; } }\n'
            'function matches(s) { return /colou?r/.test(s); }'
        )
        self.assert_functions(source, [('match', 1, 1, 2), ('matches', 2, 2, 1)])

    def test_yield_parameter_before_comment_still_allows_division(self):
        source = """function convert(total, yield, scale, units) {
          return total / yield /* normalize */ / (scale || 1) / units;
        }
        function convertAgain(total, yield, scale, units) {
          return total / yield / (scale || 1) / units;
        }
        function convertLine(yield, scale) {
          return yield // normalize the value
            / (scale || 1) / 100;
        }
        function* patterns() {
          yield /* spelling */ /colou?r/;
        }
        function normalize(total, yield, scale) {
          return total / +yield / (scale || 1) / 100;
        }
        function normalizeComment(total, yield, scale) {
          return total / +yield /* convert the input */ / (scale || 1) / 100;
        }"""
        self.assert_functions(source, [
            ('convert', 1, 3, 2), ('convertAgain', 4, 6, 2),
            ('convertLine', 7, 10, 2), ('patterns', 11, 13, 1),
            ('normalize', 14, 16, 2), ('normalizeComment', 17, 19, 2)])

    def test_binary_operator_before_yield_keeps_division(self):
        analyzer = FileAnalyzer(get_extensions(['cognitive']))
        for expression, ccn in (
                ('total / count > yield / (scale || 1) / 100', 2),
                ('total / count === yield / (scale || 1) / 100', 2),
                ('total / count > yield /* target */ / (scale || 1) / 100', 2),
                ('a + yield / (b || 1) / 2', 2),
                ('total / count in yield / (scale || 1) / 100', 2),
                ('total / count instanceof yield / (scale || 1) / 100', 2),
                ('total / count ?? yield / (scale || 1) / 100', 3),
                ('total /\n yield / (scale || 1) / 100', 2),
                ('total / count >\n yield / (scale || 1) / 100', 2)):
            with self.subTest(expression=expression):
                source = (
                    'function meetsTarget(total, count, yield, scale, a, b) {'
                    f' return {expression}; }}')
                functions = analyzer.analyze_source_code('a.js', source).function_list
                self.assertEqual([('meetsTarget', ccn, 1)], [
                    (f.name, f.cyclomatic_complexity, f.cognitive_complexity)
                    for f in functions])

    def test_yield_in_assignment_and_group_keeps_regex(self):
        source = """function* patterns(a, s) {
          const reply = yield /* spelling */ /colou?r/.test(s);
          a <<= yield /colou?r/.test(s);
          /prefix/
          yield /colou?r/.test(s);
          return a > (yield /colou?r/.test(s));
        }"""
        self.assert_functions(source, [('patterns', 1, 7, 1)], self.script_extensions)

    def test_shared_ruby_reader_keeps_quotes_inside_regex(self):
        source = (
            'def a(x)\n'
            '  p = /["\'/?]|\\//im\n'
            '  p.match?(x)\n'
            'end\n'
            'def b(x)\n'
            '  s = "str"\n'
            '  x == s || x.nil?\n'
            'end\n'
        )
        functions = analyze_file.analyze_source_code(
            'a.rb', source).function_list
        self.assertEqual([('a', 1, 4, 1), ('b', 5, 8, 2)], [
            (f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions])

    def test_shared_vue_reader_keeps_quotes_inside_regex(self):
        source = (
            '<script>\n'
            'function a(x) {\n'
            '  const p = /[<>"]/;\n'
            '  return p.test(x);\n'
            '}\n'
            'function b(x) { return x === "str" || x === null; }\n'
            '</script>\n'
        )
        functions = analyze_file.analyze_source_code(
            'a.vue', source).function_list
        self.assertEqual([('a', 2, 5, 1), ('b', 6, 6, 2)], [
            (f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions])

    def test_call_division_does_not_hide_a_later_commented_regex(self):
        for operand in ('total()', 'total(value) /* recorded above */'):
            with self.subTest(operand=operand):
                source = f'''
function normalize(value, text) {{
  return {operand} / 2 + (/* alternate spelling */ /colou?r/.test(text) ? 1 : 0);
}}
'''
                self.assert_functions(source, [('normalize', 2, 4, 2)],
                                      extensions=('js', 'ts'))

    def test_regex_statement_after_a_control_header(self):
        source = '''
function report(text) {
  if (ready(text)) /colou?r/.test(text) && print(text);
  return text;
}
'''
        self.assert_functions(source, [('report', 2, 5, 3)], extensions=('js', 'ts'))

    def test_return_regex_exposes_following_condition(self):
        source = (
            'function a(s) { return /#/.test(s) || s === 1; }\n'
            'function b(s) { return s; }\n'
        )
        self.assert_functions(source, [('a', 1, 1, 2), ('b', 2, 2, 1)])

    def test_arrow_regex_exposes_nested_condition(self):
        source = (
            'function b(xs) { return xs.filter(s => /"/.test(s) || s === "a"); }\n'
        )
        for extension in self.extensions:
            with self.subTest(extension=extension):
                functions = analyze_file.analyze_source_code(
                    'a.' + extension, source).function_list
                self.assertEqual([2, 1], [
                    f.cyclomatic_complexity for f in functions])
                self.assertEqual('(anonymous)', functions[0].name)
                self.assertEqual('b', functions[1].name)

    def assert_ccn_and_tokens(self, source, expected_ccn, expected_tokens):
        for extension in self.extensions:
            with self.subTest(extension=extension):
                functions = analyze_file.analyze_source_code(
                    'a.' + extension, source).function_list
                self.assertEqual(1, len(functions))
                self.assertEqual(expected_ccn, functions[0].cyclomatic_complexity)
                self.assertEqual(expected_tokens, functions[0].token_count)

    def test_member_division_keeps_condition_and_tokens(self):
        cases = (
            ('member',
             'function divide(obj,a,b,c) { return obj.return / (a || b) / c; }'),
            ('spaced',
             'function divide(obj,a,b,c) { return obj . return / (a || b) / c; }'),
            ('commented',
             'function divide(obj,a,b,c) {'
             ' return obj. /* member */ return / (a || b) / c; }'),
            ('optional',
             'function divide(obj,a,b,c) { return obj?.return / (a || b) / c; }'),
        )
        for label, source in cases:
            with self.subTest(label=label):
                self.assert_ccn_and_tokens(source, 2, 24)

    def test_identifier_division_keeps_baseline_tokens(self):
        cases = (
            ('dollar',
             'function divide($return,a,b,c) { return $return / (a || b) / c; }'),
            ('contextual_of',
             'function divide(of,a,b,c) { return of / (a || b) / c; }'),
            ('non_module_await',
             'function divide(await,a,b,c) { return await / (a || b) / c; }'),
            ('ordinary',
             'function divide(x,a,b,c) { return x / (a || b) / c; }'),
        )
        for label, source in cases:
            with self.subTest(label=label):
                self.assert_ccn_and_tokens(source, 2, 22)

    def test_for_of_regex_exposes_following_condition(self):
        source = (
            'function a(s) {\n'
            '  for (const x of /#/.test(s) ? [] : [s]) {\n'
            '    if (x || s === 1) { return x; }\n'
            '  }\n'
            '}\n'
        )
        # for + ternary + if + ||
        self.assert_functions(source, [('a', 1, 5, 5)])

    def test_await_keyword_regex_exposes_following_condition(self):
        source = (
            'async function a(s) { return await /#/.test(s) || s === 1; }\n'
        )
        self.assert_functions(source, [('a', 1, 1, 2)])

    def test_regex_with_quote_brace_flag_preserves_following_function(self):
        patterns = ('/"/', '/{/', '/}/', '/`/', r'/\\//', '/[/]/', '/x/g', '/[?#]/')
        for pattern in patterns:
            with self.subTest(pattern=pattern):
                source = (
                    'function a(s) {\n'
                    '  if (' + pattern + '.test(s) || s) { return 1; }\n'
                    '  return 0;\n'
                    '}\n'
                    'function b(s) { return s; }\n'
                )
                self.assert_functions(
                    source, [('a', 1, 4, 3), ('b', 5, 5, 1)])

    def test_vue_return_regex_and_member_division(self):
        source = (
            '<script>\n'
            'function a(s) { return /#/.test(s) || s === 1; }\n'
            'function divide(obj,a,b,c) { return obj.return / (a || b) / c; }\n'
            '</script>\n'
        )
        functions = analyze_file.analyze_source_code(
            'a.vue', source).function_list
        self.assertEqual([('a', 2), ('divide', 2)], [
            (f.name, f.cyclomatic_complexity) for f in functions])
        self.assertEqual(24, functions[1].token_count)
