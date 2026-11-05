import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_languages import SwiftReader
from .swift_helpers import get_swift_function_list


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


class Test_Swift_keyword_argument_labels(unittest.TestCase):
    """A keyword used as an argument label is not a control-flow keyword,
    even when a declaration gives it a separate local name."""

    BODY = """ -> Int {
    switch value {
    case 1: return enabled ? 10 : 20
    default:
        if enabled { return value > 0 ? 30 : 40 }
        return 0
    }
}
"""

    def measure(self, code):
        result = FileAnalyzer(get_extensions([Cognitive()])) \
            .analyze_source_code("a.swift", code)
        return [(f.cyclomatic_complexity, f.cognitive_complexity)
                for f in result.function_list]

    def test_unlabelled_baseline(self):
        self.assertEqual(
            [(5, 8)],
            self.measure("func f(_ value: Int, enabled: Bool)" + self.BODY))

    def test_keyword_label_with_local_name(self):
        for label in ('for', 'if', 'while', 'guard', 'catch', 'case'):
            with self.subTest(label=label):
                self.assertEqual([(5, 8)], self.measure(
                    "func f(%s value: Int, enabled: Bool)" % label
                    + self.BODY))

    def test_keyword_label_after_another_parameter(self):
        self.assertEqual([(5, 8)], self.measure(
            "func f(enabled: Bool, for value: Int)" + self.BODY))

    def test_keyword_label_with_wildcard_or_backtick_name(self):
        for name in ('_', '`default`'):
            with self.subTest(name=name):
                self.assertEqual([(5, 8)], self.measure(
                    "func f(for %s: Int, enabled: Bool)" % name
                    + self.BODY))

    def test_comments_and_newlines_around_local_name(self):
        self.assertEqual([(5, 8)], self.measure(
            "func f(\n    for /* label */\n    value // name\n    : Int,"
            "\n    enabled: Bool)" + self.BODY))

    def test_call_labels_still_masked(self):
        self.assertEqual([(1, 0), (2, 1)], self.measure("""
func a() { run(for: x, if: y, case: z) }
func b() { run(for: ok ? 1 : 2) }
"""))

    def test_real_loops_still_count(self):
        self.assertEqual([(4, 4)], self.measure("""
func loop(xs: [Int]) -> Int {
    var t = 0
    for i: Int in xs { if i > 0 { t += i } }
    run(a, b)
    for x in xs { t += x }
    return t
}
"""))


class TestSwiftConditionLists(unittest.TestCase):

    def ccn(self, body):
        source = "func f() -> Bool {\n" + body + "\n}\n"
        return get_swift_function_list(source)[0].cyclomatic_complexity

    def test_condition_list_commas_count_like_and(self):
        self.assertEqual(
            self.ccn("guard a && b && c else { return false }\nreturn true"),
            self.ccn("guard a, b, c else { return false }\nreturn true"))
        self.assertEqual(4, self.ccn("if a, b, c { return true }"))
        self.assertEqual(3, self.ccn("while a, b { step() }"))

    def test_optional_bindings_and_patterns_in_condition_lists(self):
        self.assertEqual(4, self.ccn("""\
guard let x = first,
      var y = second,
      x > y else { return false }"""))
        self.assertEqual(3, self.ccn("if let a, let b { use(a, b) }"))
        self.assertEqual(4, self.ccn(
            "if case let (a, b) = pair, a > 0 { return true }"))
        self.assertEqual(3, self.ccn(
            "if #available(iOS 15, *), ok { return true }"))

    def test_nested_commas_are_not_conditions(self):
        self.assertEqual(2, self.ccn("if f(a, b) { return true }"))
        self.assertEqual(2, self.ccn("if [a, b].contains(c) { }"))
        self.assertEqual(2, self.ccn("if (a, b) == (c, d) { }"))
        self.assertEqual(2, self.ccn("if xs.contains(where: { a, b in a }) { }"))
        self.assertEqual(2, self.ccn("if let d = x as? [String: Int] { }"))
        self.assertEqual(3, self.ccn(
            "if x is Result<Int, Error>, y { return true }"))
        self.assertEqual(2, self.ccn(
            "guard let r = x as? Dictionary<String, Int> else { return false }"))

    def test_comparisons_are_not_generic_arguments(self):
        self.assertEqual(3, self.ccn("if a < b, c > d { return true }"))
        self.assertEqual(3, self.ccn("if a < 1, b { return true }"))

    def test_commas_outside_condition_lists_are_not_conditions(self):
        self.assertEqual(3, self.ccn("""\
repeat { step() } while a
let x = 1, y = 2
if ok { use(x, y) }
var z = [1, 2]"""))
        self.assertEqual(2, self.ccn("if ok { let p = 1, q = 2 }"))
