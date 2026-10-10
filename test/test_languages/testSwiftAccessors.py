import unittest
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
""", [("get", 2, 4, 1), ("get", 7, 9, 1), ("get", 12, 12, 1),
      ("set", 13, 15, 2)])

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
