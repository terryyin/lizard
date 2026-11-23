import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags
from lizard_ext.lizardmodified import LizardExtension as Modified
from .swift_helpers import get_swift_function_list


def swift_metrics(source_code):
    analyzer = FileAnalyzer(get_extensions(
        ['cognitive', 'nd', 'ns', ComplexTags()]))
    return [(f.name, f.cyclomatic_complexity, f.cognitive_complexity,
             f.max_nesting_depth, f.max_nested_structures, f.complex_tags)
            for f in analyzer.analyze_source_code(
                "a.swift", source_code).function_list]


class Test_Swift_keyword_as_the_name_of_a_member(unittest.TestCase):
    """A keyword after a '.' names a member: `promise.catch { }` calls a
    method, and `.if` is an enum case."""

    def test_keyword_after_a_dot_adds_no_structure(self):
        self.assertEqual(
            [('fetch', 1, 0, 0, 0, []),
             ('isStatement', 2, 1, 1, 1, [['if', 6]])],
            swift_metrics("""\
func fetch(user: Promise<User>?) {
    firstly { user! }.done { u in print(u) }.catch { e in print(e) }
    user?.ensure { spinner.stop() }.catch { _ in }
}
func isStatement(token: Token) -> Bool {
    if token.tokenKind == .keyword(.if) {
        return true
    }
    return [.keyword(.for), .keyword(.while), .keyword(.guard),
            .keyword(.repeat), .keyword(.else), .keyword(.break),
            .keyword(.continue), .keyword(.try), .keyword(.do)]
        .contains(token.tokenKind)
}
"""))

    def test_keyword_after_a_dot_is_no_switch_or_case(self):
        result = FileAnalyzer(get_extensions([Modified()])) \
            .analyze_source_code("a.swift", """\
func isBranch(kind: TokenKind) -> Bool {
    return [.keyword(.switch), .keyword(.case)].contains(kind)
}
""")
        self.assertEqual([('isBranch', 1)],
                         [(f.name, f.cyclomatic_complexity)
                          for f in result.function_list])

    def test_keyword_after_a_dot_declares_nothing(self):
        self.assertEqual(
            [('isDeclaration', 3, 5, 1), ('isBinding', 7, 9, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in get_swift_function_list("""\
struct Keywords {
    static let declarations: [Keyword] = [.protocol, .func, .let, .var]
    func isDeclaration(_ keyword: Keyword) -> Bool {
        return Keywords.declarations.contains(keyword)
    }
}
func isBinding(_ keyword: Keyword) -> Bool {
    return keyword == .let || keyword == .var
}
""")])

    def test_keyword_after_a_dot_ends_no_condition_list(self):
        self.assertEqual(
            [('check', 3), ('poll', 3)],
            [(f.name, f.cyclomatic_complexity)
             for f in get_swift_function_list("""\
func check(_ key: Keyword, _ ready: Bool) {
    guard key == .else, ready else { return }
}
func poll(tool: Tool, first: Bool, second: Bool) {
    tool.repeat {}
    while first, second { break }
}
""")])

    def test_brace_after_a_dot_operator_ends_the_function(self):
        self.assertEqual(
            [('first', 3, 3), ('following', 4, 4)],
            [(f.name, f.start_line, f.end_line)
             for f in get_swift_function_list("""\
postfix operator ..
postfix func .. (value: Int) -> Int { value }
func first() -> Int { 1.. }
func following() {}
""")])

    def test_keyword_after_a_dot_operator(self):
        self.assertEqual(
            [('poll', 2, 1, 1, [['if', 5]]),
             ('pick', 4, 2, 3, [['case', 8], ['case', 8], ['case', 8]]),
             ('following', 1, 0, 0, [])],
            [(name, ccn, cognitive, nd, tags)
             for name, ccn, cognitive, nd, _, tags in swift_metrics("""\
postfix operator .+.
postfix operator .±.
func poll(_ ready: Bool) {
    _ = 1.+.
    if ready {}
}
func pick(_ x: Int) {
    switch x { case 0: _ = x.+. case 1: _ = x.±. case 2: break default: break }
}
_ = 1.+.
func following() {}
""")])

    def test_init_after_a_comma_declares_nothing(self):
        self.assertEqual(
            [('sizes', 1, 1), ('next', 2, 2)],
            [(f.name, f.start_line, f.end_line)
             for f in get_swift_function_list("""\
func sizes() -> [Size] { [Size(width: 1), .init(width: 2)] }
func next() {}
""")])

    def test_keyword_after_a_range_operator(self):
        self.assertEqual(
            [('closed', 1), ('halfOpen', 1)],
            [(f.name, f.cyclomatic_complexity)
             for f in get_swift_function_list("""\
func closed() -> ClosedRange<Keyword> { Keyword.first ... .if }
func halfOpen() -> Range<Keyword> { Keyword.first ..< .if }
""")])

    def test_keyword_after_a_dot_in_an_enum_body(self):
        result = FileAnalyzer(get_extensions(["outside"])) \
            .analyze_source_code("a.swift", """\
enum Keyword {
    case `if`, `case`, other
    static let conditions: [Keyword] = [.if, .case]
}
""")
        self.assertEqual([('*global*', 1)],
                         [(f.name, f.cyclomatic_complexity)
                          for f in result.function_list])
