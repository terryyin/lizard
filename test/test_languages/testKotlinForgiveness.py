import unittest

from .kotlin_helpers import get_kotlin_function_list


class TestKotlinForgiveness(unittest.TestCase):

    def test_forgiven_abstract_method_does_not_forgive_the_next_function(self):
        result = get_kotlin_function_list('''\
abstract class Process {
    // #lizard forgives
    abstract fun read(): Int
}

fun samplePid(): Int {
    return 1
}
        ''')
        self.assertEqual([("samplePid", 6, 8)],
                         [(f.name, f.start_line, f.end_line) for f in result])

    def test_forgiveness_after_an_abstract_method_applies_to_the_next_method(self):
        result = get_kotlin_function_list('''\
abstract class Process {
    abstract fun read(
        key: String
    )
    // #lizard forgives
    fun cachedPid(): Int { return 1 }
    fun livePid(): Int { return 2 }
}
        ''')
        self.assertEqual(["livePid"], [f.name for f in result])

    def test_abstract_method_preserves_enclosing_function_forgiveness(self):
        for before, inside in (("// #lizard forgives", ""), ("", "// #lizard forgives")):
            with self.subTest(before=before):
                result = get_kotlin_function_list(f'''\
{before}
fun samplePid(): Int {{
    {inside}
    abstract class Process {{
        abstract fun read(): Int
    }}
    return 1
}}
fun livePid(): Int {{ return 2 }}
                ''')
                self.assertEqual(["livePid"], [f.name for f in result])

    def test_forgiveness_on_a_local_abstract_method_keeps_the_outer_warning(self):
        for outer_directive, expected in (("", ["bodylessOwns"]), ("// #lizard forgives", [])):
            with self.subTest(outer_directive=outer_directive):
                result = get_kotlin_function_list(f'''\
{outer_directive}
fun bodylessOwns(flag: Boolean): Int {{
    abstract class Local {{
        // #lizard forgives
        abstract fun read(): Int
    }}
    return if (flag) 1 else 0
}}
                ''')
                self.assertEqual(expected, [f.name for f in result])

    def test_directive_between_accessors_forgives_the_following_getter(self):
        result = get_kotlin_function_list('''\
class Process {
    var cachedPid: Int = 1
        private set /* #lizard forgives */ get() = field
    fun samplePid(): Int { return 2 }
}
        ''')
        self.assertEqual([("samplePid", 4, 4)],
                         [(f.name, f.start_line, f.end_line) for f in result])
