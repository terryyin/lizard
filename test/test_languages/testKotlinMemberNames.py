import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags
from .kotlin_helpers import get_kotlin_function_list


def kotlin_metrics(source_code):
    analyzer = FileAnalyzer(get_extensions(
        ['cognitive', 'nd', 'ns', ComplexTags()]))
    return [(f.name, f.cyclomatic_complexity, f.cognitive_complexity,
             f.max_nesting_depth, f.max_nested_structures, f.complex_tags)
            for f in analyzer.analyze_source_code(
                "a.kt", source_code).function_list]


class TestKotlinBranchNestingDepth(unittest.TestCase):

    def test_braced_else_if_body_keeps_the_branch_level(self):
        functions = FileAnalyzer(get_extensions(['nd'])).analyze_source_code(
            'cache.kt', """
        fun refresh(offline: Boolean, cached: Boolean, stale: Boolean): Boolean {
            if (offline) { return false }
            else if (cached) { if (stale) return false }
            return true
        }
        fun recheck(offline: Boolean, cached: Boolean, stale: Boolean): Boolean {
            if (offline) return false
            else if (cached) { if (stale) return false }
            return true
        }
        fun evaluate(offline: Boolean, cached: Boolean, stale: Boolean): Boolean {
            if (offline) { return false }
            else if (cached) return true
            if (stale) { return false }
            return true
        }
        """).function_list
        self.assertEqual([('refresh', 2), ('recheck', 2), ('evaluate', 1)],
                         [(f.name, f.max_nesting_depth) for f in functions])


class Test_Kotlin_keyword_as_the_name_of_a_member(unittest.TestCase):
    """`catch` and `finally` are soft keywords: after a member access they
    name a member, `flow.catch { }`. A hard keyword needs backticks."""

    def test_keyword_after_a_member_access_adds_no_structure(self):
        self.assertEqual(
            [('prices', 1, 0, 0, 0, []),
             ('first', 2, 1, 1, 1, [['if', 10]])],
            kotlin_metrics("""\
fun prices(flow: Flow<Int>, cached: CachedFlow?) {
    flow.catch { emit(0) }
        .onEach { println(it) }
    cached?.catch { emit(1) }
    cached!!.catch { emit(2) }
    handlers.add(Promise<Int>::catch)
    flow.`if`.`for`.`while`.`do`.`else`.`try`.`break`.`continue`
}
fun first(flow: Flow<Int>) {
    if (flow.isEmpty()) {
        flow.catch { emit(0) }.finally { close() }
    }
}
"""))

    def test_keyword_after_a_line_break_or_comment(self):
        self.assertEqual(
            [('prices', 1, 0, 0, 0, [])],
            kotlin_metrics("""\
fun prices(flow: Flow<Int>, cached: CachedFlow?) {
    flow.
        catch { emit(0) }
    cached?. // the cache may be empty
        catch { emit(1) }
    flow./* first */catch { emit(2) }
}
"""))

    def test_keyword_after_a_range_operator_is_a_structure(self):
        self.assertEqual(
            [('span', 2, 2, 1, 1, [['if', 2]])],
            kotlin_metrics("""\
fun span(full: Boolean): IntRange {
    return 0..if (full) 10 else 5
}
"""))

    def test_method_named_like_a_keyword(self):
        self.assertEqual(
            [('catch', 1, 0, 0, 0, []), ('finally', 1, 0, 0, 0, []),
             ('catch', 1, 0, 0, 0, []), ('finally', 1, 0, 0, 0, []),
             ('catch', 1, 0, 0, 0, []), ('finally', 1, 0, 0, 0, []),
             ('`if`', 1, 0, 0, 0, [])],
            kotlin_metrics("""\
class Promise<T> {
    fun catch(handler: (Throwable) -> Unit): Promise<T> {
        return this
    }
    fun finally(handler: () -> Unit): Promise<T> {
        return this
    }
    fun <E : Throwable> catch(type: Class<E>, handler: (E) -> Unit): Promise<T> {
        return this
    }
    fun <R : Comparable<R>> finally(order: R): Promise<T> {
        return this
    }
    fun /* since 2.0 */ <E : Exception> catch(handler: (E) -> Unit): Promise<T> {
        return this
    }
    fun
    <R> finally(result: R): Promise<T> {
        return this
    }
    fun `if`(condition: Boolean): Promise<T> = this
}
"""))

    def test_get_or_set_after_a_member_access_is_no_accessor(self):
        self.assertEqual(
            [('start', 6, 8), ('stop', 11, 13)],
            [(f.name, f.start_line, f.end_line)
             for f in get_kotlin_function_list("""\
class Session(private val cache: Cache) {
    private val created = clock.get()
    init {
        cache.set(KEY, created)
    }
    fun start() {
        run()
    }
}
val defaultSession = sessions.get(0)
fun stop() {
    run()
}
""")])
