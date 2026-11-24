import unittest

from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_lambda(unittest.TestCase):

    def test_lambda_with_named_parameter_is_part_of_its_function(self):
        result = get_kotlin_function_list('''
fun f(xs: List<Int>): Int {
    return xs.map { x -> x + 1 }.size
}

fun g(): Int {
    return 1
}
        ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual([2, 6], [f.start_line for f in result])
        self.assertEqual([4, 8], [f.end_line for f in result])

    def test_multiline_lambda_with_named_parameter(self):
        result = get_kotlin_function_list('''
fun f(xs: List<Int>): Int {
    val ys = xs.map { x -> x + 1 }
    return ys.size
}

fun g(xs: List<Int>): Int {
    xs.forEach { x ->
        if (x > 0) println(x)
    }
    return 1
}
        ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual([1, 2], [f.cyclomatic_complexity for f in result])

    def test_lambda_parameter_forms(self):
        result = get_kotlin_function_list('''
fun pairs(m: Map<String, Int>) = m.forEach { (k, v) -> println(k + v) }
fun sum(xs: List<Int>): Int {
    return xs.fold(0) { acc, x -> acc + x }
}
fun typed(xs: List<Int>) {
    xs.forEach { x: Int -> println(x) }
}
fun ignored(xs: List<Int>) {
    xs.forEachIndexed { _, x -> println(x) }
}
fun nested(xss: List<List<Int>>) {
    xss.forEach { xs -> xs.forEach { x -> if (x > 0) println(x) } }
}
        ''')
        self.assertEqual(["pairs", "sum", "typed", "ignored", "nested"],
                         [f.name for f in result])
        self.assertEqual([1, 1, 1, 1, 2],
                         [f.cyclomatic_complexity for f in result])

    def test_when_inside_lambda_counts_in_its_function(self):
        result = get_kotlin_function_list('''
fun describe(xs: List<Any>): List<String> {
    return xs.map { x ->
        when (x) {
            1 -> "one"
            is String -> "text"
            else -> "other"
        }
    }
}
fun after() { }
        ''')
        self.assertEqual(["describe", "after"], [f.name for f in result])
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_property_lambda_with_parameter_is_not_a_function(self):
        result = get_kotlin_function_list('''
val inc = { x: Int -> x + 1 }

fun g(): Int {
    return 1
}
        ''')
        self.assertEqual(["g"], [f.name for f in result])

    def test_typed_lambda_property_in_a_function_is_anonymous(self):
        result = get_kotlin_function_list('''
fun f(): Int {
    val handler: (Int) -> Unit = { x -> println(x) }
    return 1
}
        ''')
        self.assertEqual(["(anonymous)", "f"], [f.name for f in result])

    def test_function_type_without_lambda_does_not_start_a_function(self):
        result = get_kotlin_function_list('''
typealias Listener = (Int) -> Unit
data class Item(val onClick: () -> Unit)
fun register(): Int {
    val listeners: Map<String, (Int) -> Unit> = mutableMapOf()
    return listeners.size
}
fun g() { }
        ''')
        self.assertEqual(["register", "g"], [f.name for f in result])

    def test_anonymous_functions_at_file_scope_and_parenthesized_receiver(self):
        result = get_kotlin_function_list('''\
val shift: Int.(Int) -> Int = fun Int.(offset: Int): Int = this + offset

val title: String
    get() { return "scores" }

val clamp: (Int) -> Int = fun(value: Int): Int {
    return if (value < 0) 0 else value
}

fun (() -> Int).runTwice(): Int {
    return this() + this()
}
        ''')
        self.assertEqual(
            [("", 1, 1, 1, 1), ("get", 4, 4, 0, 1), ("", 6, 8, 1, 2), ("runTwice", 10, 12, 0, 1)],
            [(f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
             for f in result],
        )

    def test_receiver_annotations_leave_the_named_extension_parameters(self):
        result = get_kotlin_function_list('''\
fun @JvmSuppressWildcards(false) (List<Number>).countNumbers(): Int = size
fun @kotlin.jvm.JvmSuppressWildcards(false) List<Number>.numberAt(index: Int): Number = this[index]
fun @JvmSuppressWildcards List<Number>.countImplicitly(): Int = size
fun @[JvmSuppressWildcards(false)] (List<Number>).countGrouped(): Int = size
        ''')
        self.assertEqual(
            [("countNumbers", 0), ("numberAt", 1), ("countImplicitly", 0), ("countGrouped", 0)],
            [(f.name, f.parameter_count) for f in result],
        )
