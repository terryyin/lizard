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

    def test_setter_without_body(self):
        result = get_kotlin_function_list('''
            class A {
                var located: Boolean = false
                    private set

                fun f(): Int {
                    return 1
                }

                var count: Int = 0
                    internal set
            }

            fun g() { }
        ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual([6, 14], [f.start_line for f in result])
        self.assertEqual([8, 14], [f.end_line for f in result])

    def test_setter_without_body_in_local_class(self):
        result = get_kotlin_function_list('''
            fun outer() {
                class Counter {
                    var count = 0
                        private set
                }
                println(Counter().count)
            }
        ''')
        self.assertEqual(
            [("outer", 7, 24)],
            [(f.name, f.nloc, f.token_count) for f in result],
        )

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

    def test_setter_without_body_and_getter_on_one_line(self):
        for getter in ("get()", "public get()"):
            with self.subTest(getter=getter):
                result = get_kotlin_function_list(f"""
                    class Counter {{
                        var count = 0
                            private set {getter} {{ return field }}
                        fun reset() {{ count = 0 }}
                    }}
                """)
                self.assertEqual(["get", "reset"], [f.name for f in result])

    def test_accessor_names_inside_an_accessor_header(self):
        result = get_kotlin_function_list('''
            var count = 0
                set(get) { field = get }
            val size
                get(): set { return 1 }
        ''')
        self.assertEqual(["set", "get"], [f.name for f in result])
        self.assertEqual([3, 5], [f.start_line for f in result])

    def test_annotation_named_like_a_modifier_before_an_accessor_name(self):
        result = get_kotlin_function_list('''
            @Target(AnnotationTarget.TYPE, AnnotationTarget.VALUE_PARAMETER)
            annotation class internal
            typealias `get` = Int
            fun f(): @internal get { return 1 }
            var count = 0
                set(@internal get) { field = get }
        ''')
        self.assertEqual(["f", "set"], [f.name for f in result])
        self.assertEqual([5, 7], [f.start_line for f in result])
