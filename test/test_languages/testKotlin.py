import unittest

from lizard import analyze_file
from lizard_languages import KotlinReader


def get_kotlin_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.kt", source_code
    ).function_list


class Test_tokenizing_Kotlin(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(KotlinReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['`a`'], '`a`')


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
        self.assertEqual(0, len(result))

    def test_getter(self):
        result = get_kotlin_function_list('''
            class Time
            {
                var seconds: Double = 17.0
                var minutes: Double
                    get() = seconds / 60
            }
                ''')
        self.assertEqual("get", result[0].name)

    def test_getter_setter(self):
        result = get_kotlin_function_list('''
            class Time
            {
                var seconds: Double = 17.0
                var minutes: Double
                    get() = seconds / 60
                    set(newValue) {
                        this.seconds = (newValue * 60)
                    }
            }
                ''')
        self.assertEqual("get", result[1].name)
        self.assertEqual("set", result[0].name)

    # https://docs.kotlin.org/kotlin-book/LanguageGuide/Properties.html#ID259
    def test_explicit_getter_setter(self):
        result = get_kotlin_function_list('''
            var center: Point
                get() = {
                    val centerX = origin.x + (size.width / 2)
                    val centerY = origin.y + (size.height / 2)
                    return Point(x: centerX, y: centerY)
                }
                set(newCenter) {
                    origin.x = newCenter.x - (size.width / 2)
                    origin.y = newCenter.y - (size.height / 2)
                }
            }
                ''')
        self.assertEqual("set", result[0].name)
        self.assertEqual("get", result[1].name)

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
