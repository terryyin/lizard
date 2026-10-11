import unittest

from .swift_helpers import get_swift_function_list, swift_function_spans


class TestSwiftTokens(unittest.TestCase):
    """Macros, directives, strings, comments, and `type` stay out of function structure."""

    def assert_functions(self, source, expected):
        self.assertEqual(expected, swift_function_spans(source))

    def test_macro_keeps_its_braces(self):
        self.assert_functions("""\
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
""", [("t", 4, 9, 2), ("f", 10, 14, 2)])

    def test_compiler_directive_conditions_are_not_counted(self):
        self.assert_functions("""\
func f() {
#if os(macOS) || os(iOS)
    g()
#endif
}
""", [("f", 1, 5, 1)])

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

    def test_attached_question_mark_is_not_a_ternary(self):
        self.assert_functions("""\
func a(d: [String: [Int]]) -> [Int]? {
    let x = f()?.b
    let y = d["x"]?.count
    let z = g(x)! ?? 0
    return y == 1 ? nil : [z]
}
func b() -> Int {
    return c
        ? 1
        : 2
}
""", [("a", 1, 6, 2), ("b", 7, 11, 2)])

    def test_optional_types_keep_their_question_mark_in_the_signature(self):
        function, = get_swift_function_list(
            "func f(d: [Int]?, cb: (() -> Void)?) { }")
        self.assertEqual(1, function.cyclomatic_complexity)
        self.assertIn("[ Int ] ?", function.long_name)
        self.assertNotIn("postfix", function.long_name)
