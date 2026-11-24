import unittest

from .kotlin_helpers import get_kotlin_function_list


class Test_parser_for_Kotlin_interface(unittest.TestCase):

    def test_functions_with_a_body_in_an_interface(self):
        result = get_kotlin_function_list('''
interface Interval {
    val startLabel: LabelNode
    val endLabel: LabelNode

    fun isEmpty(): Boolean = startLabel == endLabel

    fun verify(processor: Processor) {
        if (processor.index(startLabel) > processor.index(endLabel)) {
            throw IllegalStateException("Try block body starts after body end")
        }
    }
}

interface SplittableInterval<out T : Interval> : Interval {
    fun split(splitBy: Interval, keepStart: Boolean): SplitPair<T>
}

fun after() { }
        ''')
        self.assertEqual(
            [("isEmpty", 6, 1), ("verify", 8, 2), ("after", 19, 1)],
            [(f.name, f.start_line, f.cyclomatic_complexity) for f in result],
        )

    def test_companion_object_and_nested_interface(self):
        result = get_kotlin_function_list('''
fun interface Interceptor {
    @Throws(IOException::class)
    fun intercept(chain: Chain): Response

    companion object {
        inline operator fun invoke(crossinline block: (chain: Chain) -> Response): Interceptor =
            Interceptor { block(it) }
    }

    interface Chain {
        fun request(): Request

        fun proceed(request: Request): Response

        fun call(): Call {
            if (request().isHttps) return secureCall()
            return plainCall()
        }
    }
}

fun after() { }
        ''')
        self.assertEqual(
            [("invoke", 7), ("call", 16), ("after", 23)],
            [(f.name, f.start_line) for f in result],
        )
        self.assertEqual(2, result[1].cyclomatic_complexity)

    def test_classes_and_accessors_in_a_sealed_interface(self):
        result = get_kotlin_function_list('''
internal sealed interface MappedRange {
    val rangeStart: Int

    data class Inline1(
        override val rangeStart: Int,
        private val mappedTo: ByteString,
    ) : MappedRange {
        val b1: Int
            get() {
                val b3bit8 = mappedTo[0] and 0x80 != 0
                return if (b3bit8) 123 else 122
            }
    }

    val isEmpty: Boolean
        get() {
            return rangeStart < 0
        }
}
        ''')
        self.assertEqual(
            [("get", 10, 2), ("get", 17, 1)],
            [(f.name, f.start_line, f.cyclomatic_complexity) for f in result],
        )

    def test_generic_interface_with_a_where_clause(self):
        result = get_kotlin_function_list('''
interface Repository<T> : AutoCloseable where T : Entity, T : Comparable<T> {
    fun load(id: Long): T

    fun loadAll(ids: List<Long>): List<T> {
        if (ids.isEmpty()) return emptyList()
        return ids.map { load(it) }.sorted()
    }
}
        ''')
        self.assertEqual(
            [("loadAll", 5, 2)],
            [(f.name, f.start_line, f.cyclomatic_complexity) for f in result],
        )
