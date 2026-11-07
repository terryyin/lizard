import unittest
from lizard import FileAnalyzer, analyze_file, get_extensions
from .swift_helpers import get_swift_function_list, swift_function_spans


class TestSwiftAccessors(unittest.TestCase):

    def test_getter_setter(self):
        result = get_swift_function_list('''
            class Time
            {
                var minutes: Double
                {
                    get
                    {
                        return (seconds / 60)
                    }
                    set
                    {
                        self.seconds = (newValue * 60)
                    }
                }
            }
                ''')
        self.assertEqual("get", result[0].name)
        self.assertEqual("set", result[1].name)

    # https://docs.swift.org/swift-book/LanguageGuide/Properties.html#ID259
    def test_explicit_getter_setter(self):
        result = get_swift_function_list('''
            var center: Point {
                get {
                    let centerX = origin.x + (size.width / 2)
                    let centerY = origin.y + (size.height / 2)
                    return Point(x: centerX, y: centerY)
                }
                set(newCenter) {
                    origin.x = newCenter.x - (size.width / 2)
                    origin.y = newCenter.y - (size.height / 2)
                }
            }
                ''')
        self.assertEqual("get", result[0].name)
        self.assertEqual("set", result[1].name)

    def test_willset_didset(self):
        result = get_swift_function_list('''
            var cue = -1 {
                willSet {
                    if newValue != cue {
                        tableView.reloadData()
                    }
                }
                didSet {
                    tableView.scrollToRow(at: IndexPath(row: cue, section: 0), at: .bottom, animated: true)
                }
            }
                ''')
        self.assertEqual("willSet", result[0].name)
        self.assertEqual("didSet", result[1].name)

    # https://docs.swift.org/swift-book/LanguageGuide/Properties.html#ID262
    def test_explicit_willset_didset(self):
        result = get_swift_function_list('''
            class StepCounter {
                var totalSteps: Int = 0 {
                    willSet(newTotalSteps) {
                        print("About to set totalSteps to \\(newTotalSteps)")
                    }
                    didSet {
                        if totalSteps > oldValue  {
                            print("Added \\(totalSteps - oldValue) steps")
                        }
                    }
                }
            }
                ''')
        self.assertEqual("willSet", result[0].name)
        self.assertEqual("didSet", result[1].name)

    def test_keyword_declarations(self):
        result = get_swift_function_list('''
            enum Func {
                static var `init`: Bool?, willSet: Bool?
                static let `deinit` = 0, didSet = 0
                case `func`; case get, set
                func `default`() {}
            }
                ''')
        self.assertEqual("`default`", result[0].name)

    def test_setter_access_modifier_is_not_a_function(self):
        result = get_swift_function_list("""\
struct T {
    private(set) var x = 0
    fileprivate(set) var y = 0
    internal(set) var z = 0
    public(set) var w = 0
    init() {}
    func f(y: Int) -> Int {
        if y > 0 { return 1 }
        return 0
    }
}
""")
        self.assertEqual(
            [("init", 6, 6, 1), ("f", 7, 10, 2)],
            [(function.name, function.start_line, function.end_line,
              function.cyclomatic_complexity) for function in result])

    def assert_functions(self, source, expected):
        self.assertEqual(expected, swift_function_spans(source))

    def test_member_get_call_is_not_an_accessor(self):
        self.assert_functions("""\
struct A {
    func one(_ r: Result<Int, Error>) -> Int? {
        return try? r.get()
    }
    func two() -> Int {
        return 2
    }
}
""", [("one", 2, 4, 1), ("two", 5, 7, 1)])

    def test_init_expressions_are_not_initializers(self):
        self.assert_functions("""\
struct A {
    init(x: Int) {
        self.init(y: x)
    }
    func make() -> A {
        return .init(x: 1)
    }
    func other() -> A {
        return A.init(x: 2)
    }
}
""", [("init", 2, 4, 1), ("make", 5, 7, 1), ("other", 8, 10, 1)])

    def test_init_expressions_after_comma_are_not_initializers(self):
        self.assert_functions("""\
struct A {
    func make() -> [A] {
        return [.init(x: 1), .init(x: 2)]
    }
    func two() -> Int {
        return 2
    }
}
""", [("make", 2, 4, 1), ("two", 5, 7, 1)])

    def test_declaration_words_as_argument_labels_are_not_functions(self):
        self.assert_functions("""\
func boot() {
    run(init: "/sbin/agent", deinit: 1)
    run(a: 1, init: "/sbin/agent")
}
func after() {
    return
}
""", [("boot", 1, 4, 1), ("after", 5, 7, 1)])

    def test_wrapped_argument_labels_are_not_functions(self):
        self.assert_functions("""\
func boot() {
    run(
        init: "/sbin/agent"
    )
    something(
        for: .something
    )
    run(/* note */ deinit: 1)
}
func after() { return }
""", [("boot", 1, 9, 1), ("after", 10, 10, 1)])

    def test_set_as_method_or_variable_is_not_an_accessor(self):
        self.assert_functions("""\
func store() {
    UserDefaults.standard.set(1, forKey: "k")
}
func fill() -> Set<Int> {
    var set = Set<Int>()
    set.insert(1)
    return set
}
func after() {
    if a { }
}
""", [("store", 1, 3, 1), ("fill", 4, 8, 1), ("after", 9, 11, 2)])

    def test_accessor_words_as_labels_and_cases_are_not_accessors(self):
        self.assert_functions("""\
func view() -> some View {
    Toggle(isOn: Binding(get: { flag }, set: { flag = $0 }))
}
func kind(_ m: Mode) -> Int {
    switch m {
    case .get: return 1
    case .set: return 2
    }
}
""", [("view", 1, 3, 1), ("kind", 4, 9, 3)])

    def test_getter_effects_and_setter_parameter(self):
        self.assert_functions("""\
var value: Int {
    get async throws {
        return 1
    }
}
var typed: Int {
    get throws(MyError) {
        return 1
    }
}
var named: Int {
    get { 1 }
    nonmutating set(newValue) {
        if newValue > 0 { }
    }
}
""", [
            ("get", 2, 4, 1),
            ("get", 7, 9, 1),
            ("get", 12, 12, 1),
            ("set", 13, 15, 2),
        ])

    def test_setter_access_modifier_may_span_lines(self):
        result = get_swift_function_list("""\
struct T {
    private(
        set) var x = 0
    init() {}
}
""")
        self.assertEqual(
            [("init", 4, 4)],
            [(function.name, function.start_line, function.end_line)
             for function in result])


class TestSwiftPropertyFunctions(unittest.TestCase):

    def assert_functions(self, source, expected):
        self.assertEqual(expected, swift_function_spans(source))

    def test_implicit_getter_is_a_function(self):
        self.assert_functions("""\
struct V {
    var body: some View {
        Text("hi")
    }
    var score: Int {
        if flag { return 1 }
        return 0
    }
    var table: [String: Int]
    {
        return [:]
    }
    var pair: Result<Int, Error> { .success(1) }
}
""", [("body", 2, 4, 1), ("score", 5, 8, 2), ("table", 9, 12, 1),
      ("pair", 13, 13, 1)])

    def test_closure_initializer_is_a_function(self):
        self.assert_functions("""\
struct V {
    let handler: (Int) -> Int = { x in
        if x > 0 { return 1 }
        return 0
    }
    lazy var label: String = {
        return flag ? "a" : "b"
    }()
}
""", [("handler", 2, 5, 2), ("label", 6, 8, 2)])

    def test_accessor_blocks_keep_their_accessor_functions(self):
        self.assert_functions("""\
struct V {
    var a: Int {
        get { if x { return 1 }; return 0 }
        set { }
    }
    var b: Int {
        mutating get { return 1 }
    }
    var c: Int = 0 {
        didSet { if c > 1 { print(c) } }
    }
}
protocol P {
    var area: Double { get set }
}
""", [("get", 3, 3, 2), ("set", 4, 4, 1), ("get", 7, 7, 1),
      ("didSet", 10, 10, 2)])

    def test_stored_properties_and_locals_are_not_functions(self):
        self.assert_functions("""\
struct V {
    @State private var x: Int = 0
    var y: Int
    var fn: (Int) -> Void
    func f() -> Int {
        var local: Int { 3 }
        let c = { if x { } }
        return 1
    }
}
let typed: Int = 5
if let z = w { print(z) }
""", [("f", 5, 9, 2)])

    def test_property_function_lines_and_tokens_stay_with_file(self):
        source = """\
var score: Int {
    if flag { return 1 }
    return 0
}
"""
        result = analyze_file.analyze_source_code("a.swift", source)
        self.assertEqual(4, result.nloc)
        self.assertEqual(4, result.function_list[0].nloc)

    def test_type_annotation_may_continue_on_the_next_line(self):
        self.assert_functions("""\
let flag = true
struct V { var score:
 Int { if flag { return 1 }; return 0 } }
""", [("score", 2, 3, 2)])

    def test_multiline_header_lines_belong_to_the_function(self):
        functions = get_swift_function_list("""\
struct V {
    var score: Int
    {
        if flag { return 1 }
        return 0
    }
    var handler: (Int)
        -> Int = { x in
        x
    }
}
""")
        self.assertEqual([("score", 2, 6, 5), ("handler", 7, 10, 4)],
                         [(f.name, f.start_line, f.end_line, f.nloc)
                          for f in functions])

    def test_accessor_blocks_leave_global_counts_unchanged(self):
        source = """\
struct V {
    var a: Int {
        get { 1 }
        set { }
    }
    var c: Int = 0 {
        didSet { }
    }
}
"""
        functions = FileAnalyzer(get_extensions(["outside"])) \
            .analyze_source_code("a.swift", source).function_list
        outside = [f for f in functions if f.name == "*global*"][0]
        self.assertEqual((10, 25), (outside.nloc, outside.token_count))
