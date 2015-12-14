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

