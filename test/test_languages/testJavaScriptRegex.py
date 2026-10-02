import unittest

from lizard import analyze_file

from .testJavaScript import get_js_function_list


class TestJavaScriptRegex(unittest.TestCase):
    extensions = ('js', 'ts', 'jsx', 'tsx')

    def assert_functions(self, source, expected):
        for extension in self.extensions:
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


class Test_js_function_end_after_regular_expressions(unittest.TestCase):

    def spans(self, regex, filename="a.js"):
        code = (
            "function a(s) {\n"
            "  if (" + regex + ".test(s)) {\n"
            "    return 1;\n"
            "  }\n"
            "  return 0;\n"
            "}\n"
            "function b(s) {\n"
            "  return s;\n"
            "}\n"
        )
        return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                for f in analyze_file.analyze_source_code(filename, code).function_list]

    def check_function_ends(self, regex):
        for filename in ("a.js", "a.ts", "a.jsx", "a.tsx"):
            self.assertEqual([('a', 1, 6, 2), ('b', 7, 9, 1)],
                             self.spans(regex, filename),
                             regex + " in " + filename)

    def test_regex_with_a_hash(self):
        self.check_function_ends("/#/")

    def test_regex_with_a_hash_in_a_character_class(self):
        self.check_function_ends("/[?#]/")

    def test_regex_ending_with_an_escaped_slash(self):
        self.check_function_ends(r"/\//")

    def test_regex_with_a_double_quote(self):
        self.check_function_ends('/"/')

    def test_regex_with_a_single_quote(self):
        self.check_function_ends("/'/")

    def test_regex_with_a_backtick(self):
        self.check_function_ends("/`/")

    def test_regex_with_braces(self):
        self.check_function_ends("/{/")
        self.check_function_ends("/}/")

    def test_regex_with_a_slash_in_a_character_class(self):
        self.check_function_ends("/[/]/")

    def test_regex_with_escaped_slashes(self):
        self.check_function_ends(r"/a\/\/b/")
        self.check_function_ends(r"/(\d+)\s*\/\/.*/")

    def test_regex_with_a_flag(self):
        self.check_function_ends("/x/g")

    def test_regex_with_a_hash_after_return(self):
        code = (
            "function a(s) {\n"
            "  return /#/.test(s) || s === 1;\n"
            "}\n"
            "function b(s) {\n"
            "  return s;\n"
            "}\n"
        )
        functions = get_js_function_list(code)
        self.assertEqual([('a', 1, 3, 2), ('b', 4, 6, 1)],
                         [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                          for f in functions])

    def test_regex_with_a_quote_in_an_arrow_function(self):
        code = (
            "function a(list) {\n"
            "  return list.filter(s => /\"/.test(s) || s === \"a\");\n"
            "}\n"
            "function b(s) {\n"
            "  return s;\n"
            "}\n"
        )
        functions = get_js_function_list(code)
        self.assertEqual([('(anonymous)', 2, 2, 2), ('a', 1, 3, 1), ('b', 4, 6, 1)],
                         [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                          for f in functions])

    def test_operators_in_a_regex_are_not_conditions(self):
        code = (
            "function a(s) {\n"
            "  return s.replace(/a ?b||c&&d/, '');\n"
            "}\n"
        )
        self.assertEqual(1, get_js_function_list(code)[0].cyclomatic_complexity)
