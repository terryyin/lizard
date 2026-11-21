import unittest

from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_declarations_without_body(unittest.TestCase):

    def test_interface_without_body(self):
        result = get_kotlin_function_list('''
sealed interface Clause

class Impl(val x: Int) : Clause

fun f(): Int {
    return 1
}

fun g(): Int {
    return 2
}
        ''')
        self.assertEqual(["f", "g"], [f.name for f in result])

    def test_interface_header_on_several_lines(self):
        result = get_kotlin_function_list('''
interface Repository<T>
    : AutoCloseable {
    fun load(): T
}

fun f(): Int {
    return 1
}
        ''')
        self.assertEqual(["f"], [f.name for f in result])

    def test_interface_name_on_the_next_line(self):
        result = get_kotlin_function_list('''
interface
    get {
    fun f(): Int
}

fun g() {}
        ''')
        self.assertEqual(["g"], [f.name for f in result])

    def test_expect_function_without_body(self):
        result = get_kotlin_function_list('''
expect fun platformName(): String

fun g(): Int {
    return 2
}
        ''')
        self.assertEqual(["g"], [f.name for f in result])
        self.assertEqual([4], [f.start_line for f in result])
        self.assertEqual([6], [f.end_line for f in result])

    def test_abstract_and_external_functions(self):
        result = get_kotlin_function_list('''
abstract class Shape {
    abstract fun area(): Double
    external fun nativeArea(): Double

    fun describe(): String {
        if (area() > 0) return "shape"
        return "empty"
    }
}

abstract class Empty { abstract fun size(): Int }

fun after() { }
        ''')
        self.assertEqual(["describe", "after"], [f.name for f in result])
        self.assertEqual([6, 14], [f.start_line for f in result])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in result])

    def test_declarations_in_an_external_class(self):
        result = get_kotlin_function_list('''
external class Date {
    fun getTime(): Double
    val day: Int
}

fun now(): Double {
    return Date().getTime()
}
        ''')
        self.assertEqual(["now"], [f.name for f in result])
        self.assertEqual([7], [f.start_line for f in result])

    def test_annotations_between_declarations(self):
        result = get_kotlin_function_list('''
abstract class Plugin {
    @Deprecated("use start")
    abstract fun run(): Int
    @JvmOverloads
    fun start(times: Int = 1): Int {
        return run() * times
    }
}
        ''')
        self.assertEqual(["start"], [f.name for f in result])
        self.assertEqual([6], [f.start_line for f in result])

    def test_context_receiver_between_declarations(self):
        result = get_kotlin_function_list('''
abstract class Registry {
    abstract fun old(): Int
    context(String)
    fun register() {}
}
        ''')
        self.assertEqual(["register"], [f.name for f in result])

    def test_function_header_on_several_lines(self):
        result = get_kotlin_function_list('''
fun <T> larger(a: T, b: T): T
    where T : Comparable<T> {
    if (a > b) return a
    return b
}

fun counts(): Map<String,
    Int> {
    return emptyMap()
}

fun allman(x: Int)
{
    if (x > 0) println(x)
}

fun total(): Int
    = 1
        ''')
        self.assertEqual(["larger", "counts", "allman", "total"],
                         [f.name for f in result])
        self.assertEqual([2, 1, 2, 1],
                         [f.cyclomatic_complexity for f in result])

    def test_return_type_on_several_lines(self):
        result = get_kotlin_function_list('''
fun values(): MutableList<out
    CharSequence> {
    if (empty) return mutableListOf()
    return mutableListOf("x")
}

fun marked(): List<
    @Mark
    String
> {
    return emptyList()
}

fun <T> force(x: T): T
    & Any {
    return x!!
}

fun <T> forceAll(x: T): T &
    Any {
    return x!!
}
        ''')
        self.assertEqual(["values", "marked", "force", "forceAll"],
                         [f.name for f in result])
        self.assertEqual([2, 1, 1, 1],
                         [f.cyclomatic_complexity for f in result])

    def test_annotation_or_soft_keyword_in_a_continued_type(self):
        result = get_kotlin_function_list('''
fun projected(): MutableList<out
    @A CharSequence> {
    return mutableListOf("x")
}

fun repeated(): List<@A
    @B String> {
    return emptyList()
}

fun <T> bounded(x: T): T
    where T : @A
              @B CharSequence {
    return x
}

fun soft(): List<out
    data> {
    return emptyList()
}

fun receiver(): context(String)
    suspend () -> Unit {
    return {}
}

fun task(): suspend
    @A () -> Unit {
    return {}
}

fun qualified(): @demo.A
    @B String {
    return ""
}
        ''')
        self.assertEqual(["projected", "repeated", "bounded", "soft", "receiver",
                          "task", "qualified"],
                         [f.name for f in result])
