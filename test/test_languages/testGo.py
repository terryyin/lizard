import unittest
import inspect
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_go_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.go", source_code).function_list


class Test_parser_for_Go(unittest.TestCase):

    def test_empty(self):
        functions = get_go_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_go_function_list('''
        for name, ok := range names; ok {
                print("Hello, \(name)!")
            }
                ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_go_function_list('''
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_with_parameter(self):
        result = get_go_function_list('''
            func sayGoodbye(personName string, alreadyGreeted chan bool) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_return_value(self):
        result = get_go_function_list('''
            func sayGoodbye() string { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_one_function_with_two_return_values(self):
        result = get_go_function_list('''
            func sayGoodbye(p int) (string, error) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_one_function_defined_on_a_struct(self):
        result = get_go_function_list('''
            func (s Stru) sayGoodbye(){ }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual("(s Stru)sayGoodbye", result[0].long_name)

    def test_one_function_with_complexity(self):
        result = get_go_function_list('''
            func sayGoodbye() { if ++diceRoll == 7 { diceRoll = 1 }}
                ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_interface(self):
        result = get_go_function_list('''
			type geometry interface{
					 area()  float64
					 perim()  float64
			 }
            func sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_followed_by_a_class(self):
        result = get_go_function_list('''
			type geometry interface{
					 area()  float64
					 perim()  float64
			 }
            class c { }
                ''')
        self.assertEqual(0, len(result))

