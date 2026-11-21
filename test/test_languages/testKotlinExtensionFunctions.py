import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_ext.lizardnd import LizardExtension as NestDepth
from lizard_ext.lizardns import LizardExtension as NestStruct
from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_extension_functions(unittest.TestCase):

    def test_extension_function(self):
        result = get_kotlin_function_list('''
fun String.shout(): String {
    return uppercase()
}

fun g(): Int {
    return 2
}
        ''')
        self.assertEqual(["shout", "g"], [f.name for f in result])
        self.assertEqual([2, 6], [f.start_line for f in result])
        self.assertEqual([4, 8], [f.end_line for f in result])

    def test_receiver_types(self):
        result = get_kotlin_function_list('''
fun <T> List<T>.second(): T { return this[1] }
fun Map<String, List<Int>>.total(): Int { return size }
fun String?.orBlank(): String { return this ?: "" }
fun <T> T?.orThrow(): T { return this!! }
fun <T> List<T>?.count(): Int { return this?.size ?: 0 }
fun Outer.Inner.describe() { }
fun Companion.create() { }
fun Foo.Companion.create() { }
fun <T : Comparable<T>> Array<out T>.sortedCopy(): List<T> { return sorted() }
fun after() { }
        ''')
        self.assertEqual(["second", "total", "orBlank", "orThrow", "count",
                          "describe", "create", "create", "sortedCopy", "after"],
                         [f.name for f in result])

    def test_receiver_with_nullable_type_arguments(self):
        result = get_kotlin_function_list('''
fun <T : Any> Iterable<T?>.filterNotNull(): List<T> {
    return filterNotNullTo(ArrayList<T>())
}
private fun Array<Any?>.getBufferAt(index: Long) = get(index.toInt())
private fun MutableMap<Int, String?>.fill() { put(1, null) }
fun after() { }
        ''')
        self.assertEqual(["filterNotNull", "getBufferAt", "fill", "after"],
                         [f.name for f in result])

    def test_function_type_receivers(self):
        result = get_kotlin_function_list('''
fun (() -> Unit).runTwice() {
    this()
    this()
}
fun <T> (suspend () -> T).asTask(): Task<T> { return Task(this) }
fun ((Int) -> String).describe(x: Int): String { return this(x) }
fun after() { }
        ''')
        self.assertEqual(["runTwice", "asTask", "describe", "after"],
                         [f.name for f in result])
        self.assertEqual([0, 0, 1, 0], [f.parameter_count for f in result])

    def test_annotated_receiver(self):
        result = get_kotlin_function_list('''
fun @Ann String.mark() { }
fun after() { }
        ''')
        self.assertEqual(["mark", "after"], [f.name for f in result])

    def test_modifiers_and_expression_bodies(self):
        result = get_kotlin_function_list('''
suspend fun Flow<Int>.sum(): Int = fold(0) { a, b -> a + b }
inline fun <T> T.also2(block: (T) -> Unit): T { block(this); return this }
infix fun Int.plus2(other: Int) = this + other
operator fun Point.plus(other: Point) = Point(x + other.x, y + other.y)
private fun String.isLong() = length > 10 && isNotBlank()
fun after() { }
        ''')
        self.assertEqual(["sum", "also2", "plus2", "plus", "isLong", "after"],
                         [f.name for f in result])
        self.assertEqual([1, 1, 1, 1, 2, 1],
                         [f.cyclomatic_complexity for f in result])

    def test_extension_function_in_a_class(self):
        result = get_kotlin_function_list('''
class Formatter {
    override fun String.format(): String {
        if (isEmpty()) return this
        return trim()
    }

    fun plain() { }
}
        ''')
        self.assertEqual(["format", "plain"], [f.name for f in result])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in result])

    def test_extension_function_inside_a_function(self):
        result = get_kotlin_function_list('''
fun outer(xs: List<String>): Int {
    fun String.score(): Int {
        return length * 2
    }
    if (xs.isEmpty()) return 0
    return xs.sumOf { it.score() }
}
fun after() { }
        ''')
        self.assertEqual(["score", "outer", "after"], [f.name for f in result])
        self.assertEqual([3, 2, 9], [f.start_line for f in result])
        self.assertEqual([5, 8, 9], [f.end_line for f in result])
        self.assertEqual([1, 2, 1], [f.cyclomatic_complexity for f in result])

    def test_extension_property_accessor_is_reported_like_a_member_accessor(self):
        result = get_kotlin_function_list('''
val String.lastIndex2: Int
    get() {
        return length - 1
    }
fun after() { }
        ''')
        self.assertEqual(["get", "after"], [f.name for f in result])

    def test_type_parameters_do_not_change_an_ordinary_function(self):
        result = get_kotlin_function_list('''
fun <T> first(xs: List<T>, fallback: T): T { return xs.firstOrNull() ?: fallback }
fun after() { }
        ''')
        self.assertEqual(["first", "after"], [f.name for f in result])
        self.assertEqual([2, 0], [f.parameter_count for f in result])

    def test_metrics_match_the_function_without_receiver(self):
        body = '''(items: List<Int>, limit: Int): Int {
    var total = 0
    for (x in items) {
        if (x > limit && x % 2 == 0) {
            total += x
        }
    }
    return total
}
'''
        plain = self.metrics("fun sumOver" + body)
        extension = self.metrics("fun <T> Map<T, List<T>>.sumOver" + body)
        self.assertEqual(plain, extension)

    @staticmethod
    def metrics(code):
        extensions = get_extensions([NestDepth(), NestStruct(), Cognitive()])
        result = FileAnalyzer(extensions).analyze_source_code("a.kt", code)
        return [(f.name, f.parameters, f.start_line, f.end_line, f.nloc,
                 f.cyclomatic_complexity, f.max_nesting_depth,
                 f.max_nested_structures, f.cognitive_complexity)
                for f in result.function_list]
