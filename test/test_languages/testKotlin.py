import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_languages import KotlinReader
from .kotlin_helpers import get_kotlin_function_list


class Test_tokenizing_Kotlin(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(KotlinReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['`a`'], '`a`')


class TestKotlinStringLiterals(unittest.TestCase):

    def test_raw_string_with_an_unpaired_quote_keeps_the_next_function(self):
        functions = get_kotlin_function_list('''\
fun quoteFragment(): String {
    return """The "description field is missing."""
}

fun defaultField(): String {
    return "description"
}
''')
        self.assertEqual(
            [("quoteFragment", 1, 3, 10), ("defaultField", 5, 7, 10)],
            [(f.name, f.start_line, f.end_line, f.token_count) for f in functions],
        )

    def test_template_does_not_count_a_quoted_map_key_as_code(self):
        function, = get_kotlin_function_list('''\
fun localeLabel(labels: Map<String, String>): String {
    if (labels.isEmpty()) return "Default"
    return "Locale: ${labels["if"]}"
}
''')
        self.assertEqual(2, function.cyclomatic_complexity)

    def test_raw_template_with_quoted_delimiters_keeps_its_lines(self):
        functions = get_kotlin_function_list('''\
fun render(names: List<String>): String {
    return """
        Results: ${names.joinToString { """name: "$it"""" }}
        End of results.
    """.trimIndent()
}
fun count(names: List<String>) { println(names.size) }
''')
        self.assertEqual(
            [("render", 1, 6, 6), ("count", 7, 7, 1)],
            [(f.name, f.start_line, f.end_line, f.nloc) for f in functions],
        )
        self.assertEqual(29, functions[0].token_count)

    def test_raw_template_counts_code_and_skips_literal_text(self):
        analyzer = FileAnalyzer(get_extensions(["cognitive", "nd", "ns"]))
        result = analyzer.analyze_source_code("status.kt", '''\
fun statusLabel(active: Boolean): String {
    return """if (active) code="${if (active) 200 else 503}""""
}
''')
        function, = result.function_list
        self.assertEqual(
            (2, 2, 1, 2),
            (function.cyclomatic_complexity, function.cognitive_complexity,
             function.max_nesting_depth, function.max_nested_structures),
        )

    def test_adjacent_template_conditions_are_independent(self):
        analyzer = FileAnalyzer(get_extensions(["nd"]))
        result = analyzer.analyze_source_code("status.kt", '''\
fun reportStatus(active: Boolean, audit: Boolean) {
    println("${if (active) 200 else 503} // audit: ${if (audit) "on" else "off"}")
}
''')
        function, = result.function_list
        self.assertEqual((3, 1), (function.cyclomatic_complexity, function.max_nesting_depth))

    def test_template_keeps_the_surrounding_if_and_else(self):
        analyzer = FileAnalyzer(get_extensions(["ns"]))
        result = analyzer.analyze_source_code("cookies.kt", '''\
fun cookiePart(name: String, value: String?): String {
    return if (value != null) "$name=${value.trim()}" else ""
}
''')
        function, = result.function_list
        self.assertEqual(2, function.max_nested_structures)

    def test_template_in_a_get_call_does_not_start_an_accessor(self):
        analyzer = FileAnalyzer(get_extensions([]))
        result = analyzer.analyze_source_code("sourceSets.kts", '''\
val sourceSets = mapOf("iosMain" to "commonMain")
val target = "ios"
with(sourceSets) {
    get("${target}Main")
}
''')
        self.assertEqual([], result.function_list)

    def test_template_default_keeps_one_parameter(self):
        _, function = get_kotlin_function_list('''\
private const val CLIENT = "client"
private const val TIMEOUT = "timeout"

fun format(kind: String, detail: String) = "$kind-$detail"
fun failureLabel(prefix: String = "${format(CLIENT, detail = TIMEOUT)}") = prefix
''')
        self.assertEqual(["prefix"], function.parameters)

    def test_when_subject_with_a_template_keeps_the_branches(self):
        function, = get_kotlin_function_list('''\
fun protocolVersion(major: Int, minor: Int): Int {
    return when ("v${major}.${minor}") {
        "v1.0" -> 10
        "v1.1" -> 11
        else -> 0
    }
}
''')
        self.assertEqual(3, function.cyclomatic_complexity)

    def test_interpolation_keeps_the_surrounding_logical_sequence(self):
        analyzer = FileAnalyzer(get_extensions(["cognitive"]))
        result = analyzer.analyze_source_code("sessions.kt", '''\
fun mayOpenSession(authenticated: Boolean, userId: Int, sessions: Set<String>, quotaAvailable: Boolean): Boolean {
    return authenticated && "user:${userId}" in sessions && quotaAvailable
}
fun mayOpenSharedSession(
    authenticated: Boolean, staff: Boolean, optedIn: Boolean, sessions: Set<String>, quotaAvailable: Boolean
): Boolean {
    return authenticated && "staff:${staff && optedIn}" in sessions && quotaAvailable
}
''')
        self.assertEqual([1, 2], [f.cognitive_complexity for f in result.function_list])

    def test_interpolation_keeps_the_surrounding_if_body(self):
        analyzer = FileAnalyzer(get_extensions(["cognitive"]))
        result = analyzer.analyze_source_code("jobs.kt", '''\
fun jobLabel(active: Boolean, priority: Boolean, jobId: Int): String {
    return if (active) "${jobId}" + (if (priority) ":urgent" else ":normal") else "paused"
}
fun jobReport(active: Boolean, priority: Boolean, jobId: Int): String {
    return if (active) """Job
        ID: ${jobId}
        Status: """ + (if (priority) "urgent" else "normal") else "paused"
}
''')
        self.assertEqual([5, 5], [f.cognitive_complexity for f in result.function_list])

    def test_interpolation_closes_pending_cognitive_checks(self):
        analyzer = FileAnalyzer(get_extensions(["cognitive"]))
        result = analyzer.analyze_source_code("jobs.kt", '''\
fun logUntilMissing(jobIds: List<String?>) {
    for (jobId in jobIds) {
        println("Job: ${jobId ?: break}")
    }
}
fun logKnownJobs(jobIds: List<String?>) {
    for (jobId in jobIds) {
        println("Job: ${jobId ?: continue}")
    }
}
fun jobLabel(active: Boolean, priority: Boolean, value: Any?): String {
    return if (active) "${value as String ?}" + (if (priority) ":urgent" else ":normal") else "paused"
}
''')
        self.assertEqual([1, 1, 5], [f.cognitive_complexity for f in result.function_list])

    def test_nullable_cast_in_a_template_has_no_nesting(self):
        analyzer = FileAnalyzer(get_extensions(["nd"]))
        result = analyzer.analyze_source_code("jobs.kt", '''\
fun nullableLabel(value: Any?): String {
    return "Job: ${value as String ?}"
}
''')
        function, = result.function_list
        self.assertEqual(0, function.max_nesting_depth)

    def test_nested_string_templates_count_code_at_each_level(self):
        functions = get_kotlin_function_list('''\
fun statusLabel(active: Boolean, delayed: Boolean): String {
    return "Status: ${if (active) "Ready: ${if (delayed) "late" else "on time"}" else "Unavailable"}"
}
fun fallback() = "No status"
''')
        self.assertEqual(
            [("statusLabel", 1, 3, 3), ("fallback", 4, 4, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions],
        )

    def test_raw_string_after_a_nested_comment_keeps_function_boundaries(self):
        functions = get_kotlin_function_list('''\
fun render(): String {
    /*
    if (debug) { /* Log old text. */ println("""Missing "quote""") }
    */
    return """Result "ok"""
}
fun afterRender() = "next"
''')
        self.assertEqual(
            [("render", 1, 6), ("afterRender", 7, 7)],
            [(f.name, f.start_line, f.end_line) for f in functions],
        )

    def test_quotes_in_backtick_template_names_stay_inside_the_string(self):
        for identifier, string in (
                ('`display"name`', '"Label: $`display"name` ${1}"'),
                ('`display"""name`', '"""Label: $`display"""name`"""')):
            with self.subTest(string=string):
                functions = get_kotlin_function_list(f'''\
fun label(): String {{
    val {identifier} = "value"
    return {string}
}}
fun fallback() = "next"
''')
                self.assertEqual(
                    [("label", 1, 4), ("fallback", 5, 5)],
                    [(f.name, f.start_line, f.end_line) for f in functions],
                )


class TestKotlinBlockComments(unittest.TestCase):

    def test_nested_comment_does_not_count_disabled_code(self):
        functions = get_kotlin_function_list('''\
fun statusCode(active: Boolean): Int {
    /*
    /** Old policy. */
    if (!active) return 204
    */
    return if (active) 200 else 503
}

fun healthCode(): Int { return 200 }
''')
        self.assertEqual(
            [("statusCode", 1, 7, 2, 3, 19), ("healthCode", 9, 9, 1, 1, 10)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity, f.nloc, f.token_count)
             for f in functions],
        )

    def test_nested_kdoc_does_not_expose_a_commented_out_function(self):
        functions = get_kotlin_function_list('''\
/*
/** Checks the old broker. /* Retry once. */ */
fun legacyHealthCode(): Int { return 504 }
*/

fun healthCode(): Int { return 200 }
''')
        self.assertEqual(
            [("healthCode", 6, 6)],
            [(f.name, f.start_line, f.end_line) for f in functions],
        )


class Test_parser_for_Kotlin(unittest.TestCase):

    def test_empty(self):
        functions = get_kotlin_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_kotlin_function_list('''
            for name in names {
                println("Hello, \\(name)!")
            }
                ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_kotlin_function_list('''
            fun sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_with_parameter(self):
        result = get_kotlin_function_list('''
            fun sayGoodbye(personName: String, alreadyGreeted: Bool) { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_return_value(self):
        result = get_kotlin_function_list('''
            fun sayGoodbye(): String {return "bye"}
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_one_lambda_with_return_value(self):
        result = get_kotlin_function_list('''
            val sayGoodbye: () -> String = {"bye"}
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("(anonymous)", result[0].name)

    def test_one_function_with_complexity(self):
        result = get_kotlin_function_list('''
            fun sayGoodbye() { if ++diceRoll == 7 { diceRoll = 1 }}
                ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_interface(self):
        result = get_kotlin_function_list('''
            interface p {
                fun f1(): String
                fun f2()
            }
            fun sayGoodbye() { }
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_followed_by_a_class(self):
        result = get_kotlin_function_list('''
            interface p {
                fun f1(): String
                fun f2()
            }
            class c { }
                ''')
        self.assertEqual(0, len(result))

    def test_interface_with_vars(self):
        result = get_kotlin_function_list('''
            interface p {
                fun f1(): String
                fun f2()
                val p1: String
                val p2: String
                    get() = "p2"
            }
            class c { }
                ''')
        self.assertEqual(["get"], [f.name for f in result])

    def test_when_cases(self):
        result = get_kotlin_function_list('''
                fun cases(x: Int) {
                    when (x) {
                        0, 1 -> print("x == 0 or x == 1")
                        else -> print("otherwise")
                    }
                }
                ''')
        self.assertEqual("cases", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_keyword_declarations(self):
        result = get_kotlin_function_list('''
            enum class Func {
                static var `class`: Bool? = false
                static val `interface` = 0
                fun `get`() {}
            }
                ''')
        self.assertEqual("`get`", result[0].name)

    def test_type_identifier_keeps_function_boundaries(self):
        result = get_kotlin_function_list('''\
data class Job(val type: String)

fun label(job: Job): String {
    return job.type
}

fun normalize(type: String): String {
    if (type.isBlank()) return "unknown"
    return type
}

fun fallback(): String {
    return "unknown"
}
        ''')
        self.assertEqual(
            [("label", 3, 5, 1), ("normalize", 7, 10, 2), ("fallback", 12, 14, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in result],
        )

    def test_generic_function(self):
        result = get_kotlin_function_list('''
            fun <T> f() {}
                ''')
        self.assertEqual("f", result[0].name)

    def test_complex_generic_function(self):
        result = get_kotlin_function_list('''
        fun <C1, C2> f (c1: C1, c: C2): Boolean where C2 : Container {return C2.isEmpty()}
                ''')
        self.assertEqual("f", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_elvis_operator(self):
        result = get_kotlin_function_list(''' fun f() {
                val keep = filteredList?.contains(ingredient) ?: true
            }
        ''')
        self.assertEqual("f", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_for_label(self):
        result = get_kotlin_function_list('''
            fun f0() { something(for: .something) }
            fun f1() { something(for :.something) }
            fun f2() { something(for : .something) }
            fun f3() { something(for: if (isValid) true else false) }
            fun f4() { something(label1: .something, label2: .something, for: .something) }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[1].cyclomatic_complexity)
        self.assertEqual(1, result[2].cyclomatic_complexity)
        self.assertEqual(2, result[3].cyclomatic_complexity)
        self.assertEqual(1, result[4].cyclomatic_complexity)

    def test_nested(self):
        result = get_kotlin_function_list('''
        fun bar() : Int {
            fun a() : Int {
                // Do a load of stuff
                return 1
            }
            fun b() : Int {
                // Do a load of stuff
                return 1
            }
            return a() + b()
        }
        ''')
        self.assertEqual(3, len(result))
        self.assertEqual("a", result[0].name)
        self.assertEqual("b", result[1].name)
        self.assertEqual("bar", result[2].name)
