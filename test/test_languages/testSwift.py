import unittest
import inspect
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import SwiftReader


def get_swift_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.swift", source_code).function_list


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

#https://docs.swift.org/swift-book/LanguageGuide/Properties.html#ID259
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

#https://docs.swift.org/swift-book/LanguageGuide/Properties.html#ID262
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


class Test_Swift_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Swift"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
func simple() -> Int {
    let x = 5
    return x * 2
}
'''
        functions = get_swift_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
func checkValue(x: Int) -> String {
    if x > 0 {                  // +1
        return "positive"
    }
    return "non-positive"
}
'''
        functions = get_swift_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
func nestedLoops() -> Int {
    var count = 0
    for i in 1...10 {               // +1
        for j in 1...10 {           // +2 (nesting=1)
            if i == j {              // +3 (nesting=2)
                count += 1
            }
        }
    }
    return count
}  // Total CogC = 6
'''
        functions = get_swift_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch/case counts as 1 regardless of number of cases"""
        code = '''
func getDay(day: Int) -> String {
    switch day {                // +1
    case 1:
        return "Monday"
    case 2:
        return "Tuesday"
    case 3:
        return "Wednesday"
    default:
        return "Unknown"
    }
}  // Total CogC = 1
'''
        functions = get_swift_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
        # Switch is CogC=1 but CCN counts each case
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
func complexCondition(a: Bool, b: Bool, c: Bool,
                     d: Bool, e: Bool) -> Bool {
    if a && b && c {            // +1 for if, +1 for && sequence
        return true
    }
    if d || e {                 // +1 for if, +1 for || sequence
        return false
    }
    return false
}  // Total CogC = 4
'''
        functions = get_swift_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_guard_statement(self):
        """Guard statement counts as structural increment"""
        code = '''
func validate(input: String?) -> Bool {
    guard let value = input else {  // +1
        return false
    }
    guard value.count > 0 else {    // +1
        return false
    }
    return true
}  // Total CogC = 2
'''
        functions = get_swift_function_list(code)
        self.assertEqual(2, functions[0].cognitive_complexity)

