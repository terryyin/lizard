import unittest

from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_expression_body(unittest.TestCase):

    def test_braced_and_expression_body_functions_are_both_reported(self):
        result = get_kotlin_function_list('''
            fun braced(x: Int): Int {
                if (x > 0) {
                    return x
                }
                return -x
            }
            fun exprBody(x: Int) = if (x > 0) x else -x
            fun exprBodyTyped(x: Int): Int = x + 1
        ''')
        self.assertEqual(["braced", "exprBody", "exprBodyTyped"],
                         [f.name for f in result])
        self.assertEqual([1, 1, 1], [f.parameter_count for f in result])
        self.assertEqual([2, 2, 1],
                         [f.cyclomatic_complexity for f in result])

    def test_expression_body_then_braced_function(self):
        result = get_kotlin_function_list('''
            fun exprBody(x: Int) = x + 1
            fun braced() { }
        ''')
        self.assertEqual(["exprBody", "braced"], [f.name for f in result])

    def test_expression_body_when_complexity(self):
        result = get_kotlin_function_list('''
            fun exprBody(x: Int) = when (x) {
                0, 1 -> 1
                else -> 2
            }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual("exprBody", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_expression_body_class_reference_is_not_a_new_function(self):
        result = get_kotlin_function_list('''
            fun exprBody() = Foo::class
            fun after() { }
        ''')
        self.assertEqual(["exprBody", "after"], [f.name for f in result])
