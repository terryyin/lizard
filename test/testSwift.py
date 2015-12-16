import unittest
import inspect
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_swift_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.swift", source_code).function_list


class Test_parser_for_Swift(unittest.TestCase):

    def test_empty(self):
        functions = get_swift_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_swift_function_list('''
            for name in names {
                print("Hello, \(name)!")
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

    def test_generic_function(self):
        result = get_swift_function_list('''
            func f<T>() {}
                ''')
        self.assertEqual("f", result[0].name)

    def xtest_generic_function(self):
        result = get_swift_function_list('''
        func f<C1, C2: Container where (C1.t == C2.t)> (c1: C1, c: C2) -> Bool {}
                ''')
        self.assertEqual("f", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

