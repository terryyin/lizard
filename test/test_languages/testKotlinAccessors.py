import unittest

from .kotlin_helpers import get_kotlin_function_list


ACCESSOR_IDENTIFIERS = ("get", "set")


class TestKotlinAccessors(unittest.TestCase):

    def test_get_and_set_in_block_body(self):
        for name in ACCESSOR_IDENTIFIERS:
            for expression in (f"m.{name}(1)", f"m.{name}", f"{name}()"):
                with self.subTest(expression=expression):
                    result = get_kotlin_function_list(f"""
                        fun f() {{
                            val c = {expression}
                            if (c == 1) println()
                        }}
                    """)
                    self.assertEqual(
                        [("f", 2)],
                        [(f.name, f.cyclomatic_complexity) for f in result],
                    )

    def test_get_and_set_in_expression_body(self):
        for name in ACCESSOR_IDENTIFIERS:
            for expression in (f"m.{name}(1)", f"m.{name}", f"{name}()"):
                with self.subTest(expression=expression):
                    result = get_kotlin_function_list(f"""
                        fun f() = {expression} ?: 0
                        fun next() {{}}
                    """)
                    self.assertEqual(
                        [("f", 2), ("next", 1)],
                        [(f.name, f.cyclomatic_complexity) for f in result],
                    )

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
                var minutes: Double = 17.0
                    get() = source.set() ?: field
                    private set(newValue) {
                        source.set(newValue)
                        field = newValue
                    }
            }
                ''')
        self.assertEqual(["get", "set"], [f.name for f in result])
        self.assertEqual([2, 1], [f.cyclomatic_complexity for f in result])

    def test_getter_in_local_class(self):
        result = get_kotlin_function_list('''
            fun outer() {
                class Local(val source: Source) {
                    val minutes: Double
                        get() = source.get() ?: 0.0
                }
                source.set(0.0)
            }
        ''')
        self.assertEqual(
            [("get", 2), ("outer", 1)],
            [(f.name, f.cyclomatic_complexity) for f in result],
        )

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
        self.assertEqual(["get", "set"], [f.name for f in result])
