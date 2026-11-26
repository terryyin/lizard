import unittest

from lizard import FileAnalyzer, get_extensions
from .kotlin_helpers import get_kotlin_function_list


def nested_structures_and_depth(code):
    return [(f.max_nested_structures, f.max_nesting_depth)
            for f in FileAnalyzer(get_extensions(["ns", "nd"])).analyze_source_code(
                "a.kt", code).function_list]


def rows(result):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in result]


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

    def test_leading_forgiveness_after_an_expression_body(self):
        directives = ("// #lizard forgives\n// Review folded this route.",
                      "/* #lizard forgives\n   The route has its own check. */")
        for directive in directives:
            with self.subTest(directive=directive):
                result = FileAnalyzer(get_extensions(["nd", "ns"])).analyze_source_code("a.kt", f'''\
fun ready(flag: Boolean) = if (flag) 1 else 0
{directive}
fun following(flag: Boolean): Int {{ return if (flag) 1 else 0 }}
fun ordinary(flag: Boolean): Int {{ return if (flag) 1 else 0 }}
                ''').function_list
                self.assertEqual(
                    [("ready", 2, 1, 2), ("ordinary", 2, 1, 2)],
                    [(f.name, f.cyclomatic_complexity, f.max_nesting_depth,
                      f.max_nested_structures) for f in result],
                )
                self.assertEqual((1, 1), (result[0].start_line, result[0].end_line))

    def test_leading_forgiveness_respects_the_comment_hook(self):
        def keep_directive_token(tokens, reader):
            normal_comment = reader.get_comment_from_token

            def classify(token):
                if token.startswith("// #lizard"):
                    return None
                return normal_comment(token)

            reader.get_comment_from_token = classify
            yield from tokens

        result = FileAnalyzer(get_extensions([keep_directive_token])).analyze_source_code("a.kt", '''\
fun ready() = 1
// #lizard forgives
fun following() {}
        ''').function_list
        self.assertEqual(["ready", "following"], [f.name for f in result])

    def test_forgiveness_inside_a_continued_expression_body(self):
        result = get_kotlin_function_list('''\
fun component(): Any =
    // #lizard forgives
    object {}
fun ready() = true
        ''')
        self.assertEqual([("ready", 4, 4, 1)], rows(result))

    def test_trailing_forgiveness_in_an_expression_body(self):
        result = get_kotlin_function_list('''\
fun fallback() = 0 /* #lizard forgives */
fun ready() = true
        ''')
        self.assertEqual([("ready", 2, 2, 1)], rows(result))

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

    def test_expression_body_ends_at_the_end_of_its_line(self):
        result = get_kotlin_function_list('''
class FieldRemapper {
    open val newLambdaInternalName: String
        get() = originalLambdaInternalName!!

    open fun getFieldForInline(node: FieldInsnNode, prefix: StackValue?): StackValue? =
        MethodInliner.findCapturedField(node, this).remapValue

    companion object {
        fun foldName(fieldName: String) =
            CAPTURED_FIELD_FOLD_PREFIX + fieldName
    }
}
        ''')
        self.assertEqual(
            [("get", 4, 4, 1, 5), ("getFieldForInline", 6, 7, 2, 24),
             ("foldName", 10, 11, 2, 11)],
            [(f.name, f.start_line, f.end_line, f.nloc, f.token_count)
             for f in result],
        )

    def test_local_function_with_expression_body(self):
        code = '''
fun outer(xs: List<String>): Int {
    fun score(s: String) = s.length * 2
    if (xs.isEmpty()) return 0
    return xs.sumOf { score(it) }
}
fun after() { }
        '''
        result = FileAnalyzer(get_extensions(["cognitive"])).analyze_source_code(
            "a.kt", code).function_list
        self.assertEqual(
            [("score", 3, 3, 1, 0), ("outer", 2, 6, 2, 1), ("after", 7, 7, 1, 0)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.cognitive_complexity) for f in result],
        )

    def test_expression_continues_on_the_next_line(self):
        result = get_kotlin_function_list('''
fun elvis(x: String?) = x
    ?.trim()
    ?: ""
fun both(a: Boolean, b: Boolean) = a &&
    b
fun either(a: Boolean, b: Boolean) = a
    || b
fun cast(x: Any) = x
    as String
fun choose(c: Boolean) = if (c)
    1
else
    2
fun attempt() = try {
    parse()
} catch (e: Exception) {
    null
} finally {
    close()
}
fun mapped(xs: List<Int>) = xs
    .map { it * 2 }
fun after() { }
        ''')
        self.assertEqual(
            [("elvis", 2, 4, 2), ("both", 5, 6, 2), ("either", 7, 8, 2),
             ("cast", 9, 10, 1), ("choose", 11, 14, 2), ("attempt", 15, 21, 2),
             ("mapped", 22, 23, 1), ("after", 24, 24, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result],
        )

    def test_functions_in_an_object_expression(self):
        result = get_kotlin_function_list('''
fun analyzeLiveness(method: MethodNode): List<Frame> =
    analyze(method, object : BackwardAnalysisInterpreter<Frame> {
        override fun newFrame(maxLocals: Int) = Frame(maxLocals)
        override fun def(frame: Frame, insn: Node) = defVar(frame, insn)
        override fun use(frame: Frame, insn: Node) =
            useVar(frame, insn)
    })

private fun defVar(frame: Frame, insn: Node) {
    if (insn is VarInsnNode) frame.markDead(insn.index)
}
        ''')
        self.assertEqual(
            [("newFrame", 4, 4), ("def", 5, 5), ("use", 6, 7),
             ("analyzeLiveness", 2, 8), ("defVar", 10, 12)],
            [(f.name, f.start_line, f.end_line) for f in result],
        )

    def test_same_functions_and_complexity_as_a_block_body(self):
        nested = '''suspendCancellableCoroutine { cont ->
        fun finish() { cont.resume(Unit) }
        source.subscribe(object : Observer {
            override fun onComplete() = finish()
            override fun onError(e: Throwable) {
                if (e is CancellationException) cont.cancel() else cont.resumeWithException(e)
            }
        })
        source.onValue(fun(value: Int) = value > 0 && cont.isActive)
    }'''
        block = get_kotlin_function_list(
            "fun await(source: Source): Unit {\n    return " + nested + "\n}\n")
        expression = get_kotlin_function_list(
            "fun await(source: Source): Unit =\n    " + nested + "\n")
        self.assertEqual(
            [("finish", 1), ("onComplete", 1), ("onError", 2), ("", 2), ("await", 1)],
            [(f.name, f.cyclomatic_complexity) for f in block],
        )
        self.assertEqual(
            [(f.name, f.cyclomatic_complexity) for f in block],
            [(f.name, f.cyclomatic_complexity) for f in expression],
        )

    def test_generic_call_with_trailing_lambda_on_the_next_line(self):
        result = get_kotlin_function_list('''\
fun items(size: Int) = run<List<Int>>
    {
        fun capacity() = if (size < 0) 0 else size
        List(capacity()) { it }
    }
val empty = items(0)
        ''')
        self.assertEqual([("capacity", 3, 3, 2), ("items", 1, 5, 1)], rows(result))

    def test_cast_does_not_take_a_lambda_on_the_next_line(self):
        result = get_kotlin_function_list('''\
fun register(values: Any) {
    fun snapshot() = values as List<String?>
    { println(snapshot()) }
}
        ''')
        self.assertEqual([("snapshot", 2, 2, 1), ("register", 1, 4, 1)], rows(result))

    def test_nullable_generic_call_with_trailing_lambda_on_the_next_line(self):
        result = get_kotlin_function_list('''\
fun firstOrNull(names: List<String>, enabled: Boolean) = run<String?>
    {
        fun sorted() = names.sorted()
        if (enabled) sorted().firstOrNull() else null
    }
        ''')
        self.assertEqual([("sorted", 3, 3, 1), ("firstOrNull", 1, 5, 2)], rows(result))

    def test_labelled_trailing_lambda_on_the_next_line(self):
        result = get_kotlin_function_list('''\
fun total(values: List<Int>) = values.fold(0)
    add@{ sum, item ->
        if (item < 0) return@add sum
        sum + item
    }
        ''')
        self.assertEqual([("total", 1, 5, 2)], rows(result))

    def test_annotation_before_a_trailing_lambda_or_a_new_function(self):
        result = get_kotlin_function_list('''\
fun accumulate(values: List<Int>) = values.fold(0)
    @kotlin.Suppress("UNUSED_PARAMETER") { sum, value ->
        if (value < 0) sum else sum + value
    }

fun count(values: List<Int>) = values.count()

@Deprecated("Use count")
fun countLegacy(values: List<Int>) {
    println(values.count())
}
        ''')
        self.assertEqual(
            [("accumulate", 1, 4, 2), ("count", 6, 6, 1), ("countLegacy", 9, 11, 1)], rows(result),
        )

    def test_grouped_annotations_before_a_labelled_trailing_lambda(self):
        result = get_kotlin_function_list('''\
fun total(values: List<Int>) = values.fold(0)
    @[Suppress("NAME_SHADOWING") OptIn(ExperimentalStdlibApi::class)]
    add@{ sum, item ->
        if (item < 0) return@add sum
        sum + item
    }
        ''')
        self.assertEqual([("total", 1, 6, 2)], rows(result))

    def test_cast_to_parenthesized_type_does_not_take_next_lambda(self):
        result = get_kotlin_function_list('''\
fun handler(enabled: Boolean, callback: Any): () -> Unit = run {
    @Suppress("UNCHECKED_CAST")
    fun snapshot() = callback as (() -> Unit)
    { if (enabled) snapshot()() }
}
        ''')
        self.assertEqual([("snapshot", 3, 3, 1), ("handler", 1, 5, 2)], rows(result))

    def test_line_break_inside_expression_headers(self):
        result = get_kotlin_function_list('''\
fun conditional(ready: Boolean) = if
    (ready) 1 else 0

fun mapping(ready: Boolean) = when
    (ready) {
        true -> 1
        else -> 0
    }

fun read(enabled: Boolean) = try { error("disabled") } catch
    (error: IllegalStateException) { if (enabled) 1 else 0 }

fun job(): Runnable = object
    : Runnable { override fun run() { println("ready") } }
        ''')
        self.assertEqual(
            [("conditional", 1, 2, 2), ("mapping", 4, 8, 2), ("read", 10, 11, 3),
             ("run", 14, 14, 1), ("job", 13, 14, 1)],
            rows(result),
        )

    def test_expression_body_ends_before_a_closing_bracket_or_a_semicolon(self):
        result = get_kotlin_function_list('''
class Point(val x: Int) { fun double() = x * 2 }
fun register(bus: Bus) {
    bus.listen(fun(event: Event) = event.id > 0)
}
fun first() = 1; fun second() = 2
        ''')
        self.assertEqual(
            [("double", 2, 2, 1, 8), ("", 4, 4, 1, 12), ("register", 3, 5, 1, 15),
             ("first", 6, 6, 1, 6), ("second", 6, 6, 1, 6)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity, f.token_count)
             for f in result],
        )

    def test_expression_body_at_the_end_of_the_file(self):
        result = get_kotlin_function_list('fun last() = 1')
        self.assertEqual([("last", 1, 1)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_object_expression_with_constructor_arguments_in_an_expression_body(self):
        result = get_kotlin_function_list('''
fun codePoints(text: String): CodePointSequence =
    object : CodePointSequence {
        override fun iterator(): CodePointIterator = object : AbstractIterator(0, text.length) {
            override fun codePointAt(index: Int) = this@codePoints.codePointAt(index)
        }
    }

fun after() {}
        ''')
        self.assertEqual(
            [("codePointAt", 5, 5), ("iterator", 4, 6), ("codePoints", 2, 7), ("after", 9, 9)],
            [(f.name, f.start_line, f.end_line) for f in result],
        )

    def test_expression_body_ends_after_a_not_null_assertion(self):
        result = get_kotlin_function_list('''
class MatchGroups(private val match: Match) {
    val first: String get() = match[0]!!
    fun next(): Match? = match.next()
}
        ''')
        self.assertEqual(
            [("get", 3, 3), ("next", 4, 4)],
            [(f.name, f.start_line, f.end_line) for f in result],
        )

    def test_expression_continues_after_a_line_of_annotations(self):
        result = get_kotlin_function_list('''
inline fun <reified T> emptyArray(): Array<T> =
    @Suppress("UNCHECKED_CAST")
    (arrayOfNulls<T>(0) as Array<T>)
fun <E> get(key: Key<E>): E? =
    @Suppress("UNCHECKED_CAST") @kotlin.Deprecated("x")
    if (key == this.key) this as E else null
fun clamp(value: Int) =
    @[Suppress("UNUSED_PARAMETER")]
    if (value < 0) 0 else value
fun after() {}
        ''')
        self.assertEqual(
            [("emptyArray", 2, 4, 1), ("get", 5, 7, 2), ("clamp", 8, 10, 2), ("after", 11, 11, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result],
        )

    def test_expression_continues_after_an_infix_function_at_the_end_of_a_line(self):
        result = get_kotlin_function_list('''
private fun flags(isFinal: Boolean): Int =
    ACC_PUBLIC or
            (if (isFinal) ACC_FINAL else 0) or
            ACC_SUPER
fun upTo(n: Int) = 0 until
    n
fun touch(ready: Boolean) = if (ready) counter
fun pair(a: Int) = a.let { it } to
    a
fun after() {}
        ''')
        self.assertEqual(
            [("flags", 2, 5, 2), ("upTo", 6, 7, 1), ("touch", 8, 8, 2),
             ("pair", 9, 10, 1), ("after", 11, 11, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result],
        )

    def test_greater_than_at_the_end_of_a_line(self):
        result = get_kotlin_function_list('''
fun bigger(a: Int, b: Int) = a >
    b && b > 0
fun cast(x: Any) = x as Map<String, List<Int>>
fun empty() = emptyList<
    String
>()
fun after() {}
        ''')
        self.assertEqual(
            [("bigger", 2, 3, 2), ("cast", 4, 4, 1), ("empty", 5, 7, 1),
             ("after", 8, 8, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result],
        )

    def test_expression_body_ends_before_a_comma_that_it_did_not_open(self):
        result = get_kotlin_function_list('''
fun handlers() = register(
    fun(x: Int) = x > 0 && x != 10,
    Priority.HIGH
)
fun typed() = mapOf<String, Int>()
fun after() {}
        ''')
        self.assertEqual(
            [("", 3, 3, 2), ("handlers", 2, 5, 1), ("typed", 6, 6, 1),
             ("after", 7, 7, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result],
        )

    def test_expression_body_of_a_named_function_goes_on_after_a_comma_in_a_string_template(self):
        result = get_kotlin_function_list('''
fun label() = "${if (a) ", b" else ""}" + (if (c) "x" else "y")
fun after() {}
        ''')
        self.assertEqual(
            [("label", 2, 2, 2), ("after", 3, 3, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in result],
        )

    def test_expression_continues_after_an_annotation_on_the_line_of_the_equals_sign(self):
        result = get_kotlin_function_list('''
fun <T> unchecked(x: Any): T = @Suppress("UNCHECKED_CAST")
    (x as T)
fun after() {}
        ''')
        self.assertEqual(
            [("unchecked", 2, 3), ("after", 4, 4)],
            [(f.name, f.start_line, f.end_line) for f in result],
        )

    def test_expression_body_ends_after_an_annotated_expression_in_brackets(self):
        result = get_kotlin_function_list('''
fun <T> natural(): Comparator<T> = @Suppress("UNCHECKED_CAST") (Natural as Comparator<T>)
fun after() {}
        ''')
        self.assertEqual(
            [("natural", 2, 2), ("after", 3, 3)],
            [(f.name, f.start_line, f.end_line) for f in result],
        )

    def test_object_expression_with_more_than_one_supertype_in_an_expression_body(self):
        result = get_kotlin_function_list('''
fun handler() = object : Base(1), Listener {
    override fun onEvent(e: Int) = e > 0
}
fun after() {}
        ''')
        self.assertEqual(
            [("onEvent", 3, 3, 1), ("handler", 2, 4, 1), ("after", 5, 5, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in result],
        )

    def test_nested_structures_and_depth_end_with_the_expression_body(self):
        expression = nested_structures_and_depth('''
fun a(ready: Boolean) = if (ready) 1 else 2
fun b(ready: Boolean) = if (ready) 1 else 2
fun c(ready: Boolean) = if (ready) 1 else 2
        ''')
        block = nested_structures_and_depth('''
fun a(ready: Boolean): Int { return if (ready) 1 else 2 }
fun b(ready: Boolean): Int { return if (ready) 1 else 2 }
fun c(ready: Boolean): Int { return if (ready) 1 else 2 }
        ''')
        self.assertEqual(block, expression)

    def test_a_comparison_is_not_a_type_argument_list(self):
        result = get_kotlin_function_list('''
fun f(a: Int, b: Int) = a < b || b >
    a && a != 0
fun after() {}
        ''')
        self.assertEqual([("f", 2, 3, 3), ("after", 4, 4, 1)], rows(result))

    def test_anonymous_function_body_with_a_comparison_ends_at_a_comma(self):
        result = get_kotlin_function_list('''
fun f() {
    consume(fun(a: Int) = a < 0,
        if (true) 1 else 0)
}
        ''')
        self.assertEqual([("", 3, 3, 1), ("f", 2, 5, 2)], rows(result))

    def test_expression_continues_after_annotation_arguments_on_more_than_one_line(self):
        result = get_kotlin_function_list('''
fun f() = @Suppress(
    "X")
    1
fun after() {}
        ''')
        self.assertEqual([("f", 2, 4, 1), ("after", 5, 5, 1)], rows(result))

    def test_expression_continues_after_an_infix_call_after_a_not_null_assertion(self):
        result = get_kotlin_function_list('''
fun f(xs: List<String?>) = xs[0]!! to
    (if (true) "a" else "b")
fun after() {}
        ''')
        self.assertEqual([("f", 2, 3, 2), ("after", 4, 4, 1)], rows(result))

    def test_infix_call_named_like_an_accessor_in_a_getter(self):
        result = get_kotlin_function_list('''
class A { infix fun set(n: Int) = n }
val n: Int get() = A() set (if (true) 1 else 2)
        ''')
        self.assertEqual([("set", 2, 2, 1), ("get", 3, 3, 2)], rows(result))

    def test_expression_body_that_is_an_anonymous_function(self):
        result = get_kotlin_function_list('''
fun f() = fun() =
    1
fun after() {}
        ''')
        self.assertEqual([("", 2, 3, 1), ("f", 2, 3, 1), ("after", 4, 4, 1)], rows(result))

    def test_anonymous_functions_in_when_branches(self):
        result = get_kotlin_function_list('''
fun f(b: Boolean) = when (b) {
    true -> fun() = 1
    else -> fun() = 2
}
        ''')
        self.assertEqual([("", 3, 3, 1), ("", 4, 4, 1), ("f", 2, 5, 2)], rows(result))

    def test_expression_body_that_ends_in_brackets_does_not_end_the_statement(self):
        expression = nested_structures_and_depth('''
fun f(x: Boolean, xs: Map<() -> Int, Int>) {
    if (x) xs[fun() = 1] ?: if (x) 2 else 3
}
        ''')
        value = nested_structures_and_depth('''
fun f(x: Boolean, xs: Map<() -> Int, Int>) {
    if (x) xs[one] ?: if (x) 2 else 3
}
        ''')
        self.assertEqual([(0, 0)] + value, expression)

    def test_anonymous_function_body_with_a_comparison_to_a_name_ends_at_a_comma(self):
        result = get_kotlin_function_list('''
fun choose(limit: Int, strict: Boolean): Pair<(Int) -> Boolean, Int> {
    return Pair(fun(value: Int) = value < limit, if (strict) limit else 0)
}
        ''')
        self.assertEqual([("", 3, 3, 1), ("choose", 2, 4, 2)], rows(result))

    def test_uppercase_comparison_keeps_the_following_argument_in_the_caller(self):
        for next_argument in ("if (flag) 1 else 0", "run { if (flag) 1 else 0 }"):
            with self.subTest(next_argument=next_argument):
                result = get_kotlin_function_list(f'''\
private const val LIMIT = 4
fun classify(flag: Boolean) = Pair(fun(n: Int) = n < LIMIT, {next_argument})
                ''')
                self.assertEqual([("", 2, 2, 1), ("classify", 2, 2, 2)], rows(result))

    def test_call_or_index_in_the_next_argument_is_not_a_type(self):
        for operand in ("kotlin.math.abs(value)", "(listOf(value)[0])"):
            with self.subTest(operand=operand):
                result = get_kotlin_function_list(f'''\
private const val LIMIT = 4
fun classify(flag: Boolean, value: Int) = Pair(
    fun(n: Int) = n < LIMIT,
    {operand} > (if (flag) 1 else 0),
)
                ''')
                self.assertEqual([("", 3, 3, 1), ("classify", 2, 5, 2)], rows(result))

    def test_function_type_in_a_generic_call_keeps_the_next_argument_in_the_caller(self):
        result = get_kotlin_function_list('''\
fun handlers(flag: Boolean) = Pair(
    fun() = emptyMap<String, (Int) -> Int>(),
    if (flag) 1 else 0,
)
        ''')
        self.assertEqual([("", 2, 2, 1), ("handlers", 1, 4, 2)], rows(result))

    def test_expression_body_ends_after_nested_type_arguments_with_a_nullable_type(self):
        result = get_kotlin_function_list('''
fun decode(value: Any) = value as? List<Pair<String, Int?>>
fun fallback() {}
        ''')
        self.assertEqual([("decode", 2, 2, 1), ("fallback", 3, 3, 1)], rows(result))

    def test_anonymous_function_body_ends_before_the_else_of_an_if_around_it(self):
        result = get_kotlin_function_list('''
fun select(strict: Boolean): (Int) -> Boolean =
    if (strict) fun(n: Int) = n > 0 else
        fun(n: Int) = n >= 0
        ''')
        self.assertEqual([("", 3, 3, 1), ("", 4, 4, 1), ("select", 2, 4, 2)], rows(result))

    def test_expression_body_that_ends_at_a_bracket_on_the_next_line_does_not_end_the_statement(self):
        expression = nested_structures_and_depth('''
fun select(ready: Boolean, cache: Cache, key: Int): Int {
    return if (ready) cache[
        fun() = key
    ] ?: if (key < 0) 0 else 1
    else 2
}
        ''')
        value = nested_structures_and_depth('''
fun select(ready: Boolean, cache: Cache, key: Int): Int {
    return if (ready) cache[
        key
    ] ?: if (key < 0) 0 else 1
    else 2
}
        ''')
        self.assertEqual([(0, 0)] + value, expression)

    def test_expression_body_ends_after_type_arguments_with_a_package_name(self):
        result = get_kotlin_function_list('''
class Decoder {
    fun decode(value: Any) = value as? List<java.time.Instant>
    val batchSize = if (System.getenv("CI") != null) 10 else 100
}
        ''')
        self.assertEqual([("decode", 3, 3, 1)], rows(result))

    def test_lambda_on_the_line_after_a_complete_expression_body_is_not_part_of_it(self):
        result = get_kotlin_function_list('''
fun makeCounter(enabled: Boolean): () -> Int = run {
    var count = 0
    fun next() = ++count
    { if (enabled) next() else count }
}
        ''')
        self.assertEqual([("next", 4, 4, 1), ("makeCounter", 2, 6, 2)], rows(result))

    def test_lambda_on_the_line_after_a_call_is_part_of_the_expression_body(self):
        result = get_kotlin_function_list('''
fun total(xs: List<Int>) = xs.fold(0)
{ sum, x -> if (x > 0) sum + x else sum }
        ''')
        self.assertEqual([("total", 2, 3, 2)], rows(result))

    def test_object_body_on_the_line_after_its_supertypes_is_part_of_the_expression_body(self):
        result = get_kotlin_function_list('''
fun task(enabled: Boolean) = object : Runnable
{
    override fun run() { if (enabled) println() }
}
        ''')
        self.assertEqual([("run", 4, 4, 2), ("task", 2, 5, 1)], rows(result))
