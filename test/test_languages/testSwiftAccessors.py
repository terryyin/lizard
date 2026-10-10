import unittest
from .swift_helpers import get_swift_function_list


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
