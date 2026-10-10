import unittest
from lizard_languages import SwiftReader
from .swift_helpers import get_swift_function_list, swift_function_spans


class Test_tokenizing_Swift(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(SwiftReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['`a`'], '`a`')

class Test_parser_for_Swift(unittest.TestCase):

    def test_empty(self):
        functions = get_swift_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_swift_function_list('''
            for name in names {
                print("Hello, \\(name)!")
            }
                ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_swift_function_list('''
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_with_parameter(self):
        result = get_swift_function_list('''
            func sayGoodbye(personName: String, alreadyGreeted: Bool) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_return_value(self):
        result = get_swift_function_list('''
            func sayGoodbye() -> String { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_one_function_with_complexity(self):
        result = get_swift_function_list('''
            func sayGoodbye() { if ++diceRoll == 7 { diceRoll = 1 }}
                ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_interface(self):
        result = get_swift_function_list('''
            protocol p {
                func f1() -> Double
                func f2() -> NSDate
            }
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_followed_by_a_class(self):
        result = get_swift_function_list('''
            protocol p {
                func f1() -> Double
                func f2() -> NSDate
            }
            class c { }
                ''')
        self.assertEqual(0, len(result))

    def test_interface_with_var(self):
        result = get_swift_function_list('''
            protocol p {
                func f1() -> Double
                var area: Double { get }
            }
            class c { }
                ''')
        self.assertEqual(0, len(result))

    def test_interface_with_var(self):
        result = get_swift_function_list('''
            protocol p {
                func f1() -> Double
                var area: Double { get }
            }
            class c { }
                ''')
        self.assertEqual(0, len(result))

#https://docs.swift.org/swift-book/LanguageGuide/Initialization.html
    def test_init(self):
        result = get_swift_function_list('''
            init() {}
                ''')
        self.assertEqual("init", result[0].name)

#https://docs.swift.org/swift-book/LanguageGuide/Deinitialization.html
    def test_deinit(self):
        result = get_swift_function_list('''
            deinit {}
                ''')
        self.assertEqual("deinit", result[0].name)

#https://docs.swift.org/swift-book/LanguageGuide/Subscripts.html
    def test_subscript(self):
        result = get_swift_function_list('''
            override subscript(index: Int) -> Int {}
                ''')
        self.assertEqual("subscript", result[0].name)

#https://stackoverflow.com/a/30593673
    def test_labeled_subscript(self):
        result = get_swift_function_list('''
            extension Collection {
                /// Returns the element at the specified index iff it is within bounds, otherwise nil.
                subscript (safe index: Index) -> Iterator.Element? {
                    return indices.contains(index) ? self[index] : nil
                }
            }
                ''')
        self.assertEqual("subscript", result[0].name)

    def test_generic_function(self):
        result = get_swift_function_list('''
            func f<T>() {}
                ''')
        self.assertEqual("f", result[0].name)

    def test_generic_function(self):
        result = get_swift_function_list('''
        func f<C1, C2: Container where (C1.t == C2.t)> (c1: C1, c: C2) -> Bool {}
                ''')
        self.assertEqual("f", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_optional(self):
        result = get_swift_function_list(''' func f() {optional1?} ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_coalescing_operator(self):
        result = get_swift_function_list(''' func f() {
                let keep = filteredList?.contains(ingredient) ?? true
            }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)


    def test_for_label(self):
        result = get_swift_function_list('''
            func f0() { something(for: .something) }
            func f1() { something(for :.something) }
            func f2() { something(for : .something) }
            func f3() { something(for: isValid ? true : false) }
            func f4() { something(label1: .something, label2: .something, for: .something) }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[1].cyclomatic_complexity)
        self.assertEqual(1, result[2].cyclomatic_complexity)
        self.assertEqual(2, result[3].cyclomatic_complexity)
        self.assertEqual(1, result[4].cyclomatic_complexity)

    def test_guard(self):
        # `guard isValid else { return }` equal to `if isValid { return }`
        # ccn = 2
        result = get_swift_function_list('''
            func f() { guard isValid else { return } }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nested(self):
        result = get_swift_function_list('''
            func f() {
                func stepForward(input: Int) -> Int { return input + 1 }
            }
        ''')
        self.assertEqual(2, len(result))

    def test_macro_keeps_its_braces(self):
        result = get_swift_function_list("""\
#Preview("A folder") {
    Text("x")
}
func t() {
    #expect(xs.allSatisfy {
        $0 > 1
    })
    #expect(a && b)
}
func f() {
    if #available(macOS 14, *) {
        g()
    }
}
""")
        self.assertEqual(
            [("t", 4, 9, 2), ("f", 10, 14, 2)],
            [(function.name, function.start_line, function.end_line,
              function.cyclomatic_complexity) for function in result])

    def test_compiler_directive_conditions_are_not_counted(self):
        result = get_swift_function_list("""\
func f() {
#if os(macOS) || os(iOS)
    g()
#endif
}
""")
        self.assertEqual(
            [("f", 1, 5, 1)],
            [(function.name, function.start_line, function.end_line,
              function.cyclomatic_complexity) for function in result])

    def assert_functions(self, source, expected):
        self.assertEqual(expected, swift_function_spans(source))

    def test_raw_strings_are_single_literals(self):
        self.assert_functions("""\
func a() {
    if s == #"x"# {
        g()
    }
}
func b() {
    let r = #\"\"\"
    { "unbalanced
    \"\"\"#
    let s = #"v \\#(f("}")) w"#
}
func c() {
    return
}
""", [("a", 1, 5, 2), ("b", 6, 11, 1), ("c", 12, 14, 1)])

    def test_quotes_inside_interpolation_stay_in_the_literal(self):
        self.assert_functions("""\
func a() {
    do { try db.execute("ATTACH '\\(p.replacing(of: "'", with: "''"))' AS previous") } catch { return }
}
func b() {
    return
}
""", [("a", 1, 3, 2), ("b", 4, 6, 1)])

    def test_multiline_string_with_interpolation_is_one_literal(self):
        self.assert_functions("""\
func a() -> String {
    return \"\"\"
    \\(names.map { "\\"\\($0)\\"" }.joined(separator: ", ")) {
    \"\"\"
}
func b() {
    return
}
""", [("a", 1, 5, 1), ("b", 6, 8, 1)])

    def test_comments_hide_quotes_and_nest(self):
        self.assert_functions("""\
func a() {
    // it's a "quote
    /* outer /* inner */ { still comment */
    if b { }
}
func c() { }
""", [("a", 1, 5, 2), ("c", 6, 6, 1)])

    def test_type_is_not_a_declaration_keyword(self):
        self.assert_functions("""\
func a() {
    #expect(x == .type)
}
func b() {
    let t = type(of: self)
}
""", [("a", 1, 3, 1), ("b", 4, 6, 1)])

    def test_failable_initializer(self):
        body = """\
struct S {{
    init{mark}(text: String) {{
        if text.isEmpty {{ return nil }}
        if text == "a" {{ return nil }}
    }}
}}
"""
        for mark in ('?', '!'):
            with self.subTest(mark=mark):
                result = get_swift_function_list(body.format(mark=mark))
                self.assertEqual(
                    [("init", 3)],
                    [(function.name, function.cyclomatic_complexity)
                     for function in result])
