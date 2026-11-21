import unittest

from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_token_after_comma(unittest.TestCase):

    def test_lambda_argument_after_a_comma(self):
        result = get_kotlin_function_list('''
fun f() {
    listOf(1, { println(1) })
    if (true) println(1)
}
fun g() {}
        ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual([2, 6], [f.start_line for f in result])
        self.assertEqual([5, 6], [f.end_line for f in result])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in result])

    def test_when_argument_after_a_comma(self):
        result = get_kotlin_function_list('''
fun label(x: Int) {
    listOf("label", when (x) {
        1 -> "one"
        2 -> "two"
        else -> "many"
    })
}
        ''')
        self.assertEqual(["label"], [f.name for f in result])
        self.assertEqual([3], [f.cyclomatic_complexity for f in result])

    def test_trailing_comma_in_a_when_branch(self):
        result = get_kotlin_function_list('''
fun color(x: Int): Int {
    return when (x) {
        1, 2, -> 1
        else -> 2
    }
}
        ''')
        self.assertEqual(["color"], [f.name for f in result])
        self.assertEqual([2], [f.cyclomatic_complexity for f in result])

    def test_function_after_an_enum_with_a_trailing_comma(self):
        result = get_kotlin_function_list('''
class Counter {
    var first = 0
        private set
    enum class Mode {
        ON,
    }
    var second = 0
        private set
    fun reset() { second = 0 }
}
        ''')
        self.assertIn("reset", [f.name for f in result])

    def test_object_argument_after_a_comma(self):
        result = get_kotlin_function_list('''
fun outer() {
    listOf(0, object {
        val n = listOf(1).get(0)
        fun read() {}
    })
}
        ''')
        self.assertEqual(["read", "outer"], [f.name for f in result])

    def test_anonymous_function_argument_after_a_comma(self):
        result = get_kotlin_function_list('''
fun outer() {
    listOf(0, fun(x: Int) = x)
}
fun after() {}
        ''')
        self.assertEqual([("outer", 2, 4), ("after", 5, 5)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_trailing_comma_in_lambda_parameters(self):
        result = get_kotlin_function_list('''
fun outer() {
    var Sink = {}
    listOf(1).forEach { x, -> Sink = { println(x) } }
    Sink()
}
        ''')
        self.assertEqual([("outer", 33)],
                         [(f.name, f.token_count) for f in result])

    def test_accessor_name_in_a_parameter_list(self):
        result = get_kotlin_function_list('''
class Toggle(name: String, set: Boolean) : Flag(name, set) {
    fun flip(): Boolean {
        return !isSet
    }
}

class Entry(val key: String, val set: Boolean) {
    fun label(): String {
        return key
    }
}
        ''')
        self.assertEqual(["flip", "label"], [f.name for f in result])
