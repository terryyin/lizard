import inspect
import unittest

from .testPython import get_python_function_list


class Test_Python_nesting_level(unittest.TestCase):

    def test_top_level_function(self):
        functions = get_python_function_list(
            "def a():\n" +
            "    pass")
        self.assertEqual(0, functions[0].top_nesting_level)

    def test_second_top_level_functions(self):
        functions = get_python_function_list(
            "def a():\n" +
            "    pass\n" +
            "def b():\n" +
            "    pass"
        )
        self.assertEqual(0, functions[1].top_nesting_level)

    def test_top_level_function_with_leading_space(self):
        functions = get_python_function_list(
            " def a():\n" +
            "    pass\n"
        )
        self.assertEqual(1, functions[0].top_nesting_level)

    def test_2nd_level_function_with_leading_space(self):
        functions = get_python_function_list(
            "class C:\n" +
            "    def f():\n" +
            "        pass\n"
        )
        self.assertEqual(1, functions[0].top_nesting_level)

    def test_miss_indented_comment(self):
        functions = get_python_function_list(
            "class C:\n" +
            " class D:\n" +
            "  def a():\n" +
            "   pass\n" +
            " #\n" +
            "   def b():\n" +
            "    pass")
        self.assertEqual(7, functions[0].end_line)


class Test_Python_function_structure(unittest.TestCase):

    def test_multi_line_function_def_function_end(self):
        source = """
            def foo(arg1,
                arg2,
            ):
                # comment
                return True

            def foo2(arg1,
                arg2,
                arg3
            ):
                if True:
                    return False
            """
        functions = get_python_function_list(source)
        self.assertEqual(6, functions[0].end_line)
        self.assertEqual(13, functions[1].end_line)

    def test_multi_line_function_def_with_indentation_more_than_function_body(self):
        def function(arg1,
                     arg2
                     ):
            if True:
                return False

        functions = get_python_function_list(inspect.getsource(function))
        self.assertEqual(5, functions[0].nloc)
        self.assertEqual(5, functions[0].end_line)

    def test_multiline_return_annotation_keeps_function_body(self):
        source = """\
class Parser:
    def parse_call_args(
        self,
    ) -> tuple[
        list[Expr],
        list[Keyword],
    ]:
        if self.has_args():
            return self.read_args(), []
        return [], []
"""
        functions = get_python_function_list(source)
        self.assertEqual([(2, 10, 2)], [
            (f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions
        ])

    def test_multiline_return_annotation_keeps_inline_suite(self):
        source = """\
class Parser:
    def read(self, enabled) -> (
        int
    ): return 1 if enabled else 0
    def fallback(self):
        return 2
"""
        functions = get_python_function_list(source)
        self.assertEqual([("read", 4, 2), ("fallback", 6, 1)], [
            (f.name, f.end_line, f.cyclomatic_complexity) for f in functions
        ])

    def test_inline_annotation_keeps_bounds_with_reused_fstring_quotes(self):
        source = """\
class Parser:
    def read(self, table) -> (
        str
    ): return f"{table["["]}"
    def fallback(self):
        return "empty"
"""
        functions = get_python_function_list(source)
        self.assertEqual([("read", 4), ("fallback", 6)], [
            (f.name, f.end_line) for f in functions
        ])

    def test_interpolated_return_annotation_preserves_function_bounds(self):
        source = """\
class Parser:
    def read(self, value) -> (
        PREFIX"{types["["]}"
    ):
        if value:
            return 1
        return 0
    def fallback(self):
        return 2
"""
        for prefix in ("f", "t"):
            with self.subTest(prefix=prefix):
                functions = get_python_function_list(source.replace("PREFIX", prefix))
                self.assertEqual([("read", 7, 2), ("fallback", 9, 1)], [
                    (f.name, f.end_line, f.cyclomatic_complexity)
                    for f in functions
                ])

    def test_function_surrounded_by_global_statements(self):
        source = """
        s1 = 'global statement'
        def function(arg1,
                     arg2
                     ):
            if True:
                return False
        s2 = 'global statement'
        """
        functions = get_python_function_list(source)
        self.assertEqual(5, functions[0].nloc)
        self.assertEqual(7, functions[0].end_line)

    def test_function_end(self):
        class namespace3:
            def simple_function(self):
                pass

            blah = 42
        functions = get_python_function_list(inspect.getsource(namespace3))
        self.assertEqual(1, len(functions))
        self.assertEqual("simple_function", functions[0].name)
        self.assertEqual(3, functions[0].end_line)

    def test_nested_functions(self):
        class namespace5:
            def function1(a, b):
                def function2(a, b):
                    pass
                a = 1 if b == 2 else 3
        functions = get_python_function_list(inspect.getsource(namespace5))
        self.assertEqual(2, len(functions))
        self.assertEqual("function1.function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(5, functions[1].end_line)
        self.assertEqual(2, functions[1].cyclomatic_complexity)
        self.assertEqual(2, functions[1].max_nesting_depth)
        # will be fixed, should be equal to 1

    def test_nested_functions_ended_at_eof(self):
        class namespace6:
            def function1(a, b):
                def function2(a, b):
                    pass
        functions = get_python_function_list(inspect.getsource(namespace6))
        self.assertEqual(2, len(functions))
        self.assertEqual("function1.function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(4, functions[1].end_line)

    def test_nested_functions_ended_at_same_line(self):
        class namespace7:
            def function1(a, b):
                def function2(a, b):
                    pass

            def function3():
                pass
        functions = get_python_function_list(inspect.getsource(namespace7))
        self.assertEqual(3, len(functions))
        self.assertEqual("function1.function2", functions[0].name)
        self.assertEqual(4, functions[0].end_line)
        self.assertEqual("function1", functions[1].name)
        self.assertEqual(4, functions[1].end_line)

    def test_inline_methods_are_reported_before_next_definition_and_eof(self):
        source = """\
class Stream:
    def read(self) -> str: ...
    async def close(self) -> None: ..."""
        functions = get_python_function_list(source)
        self.assertEqual([("read", 2, 2), ("close", 3, 3)], [
            (f.name, f.start_line, f.end_line) for f in functions
        ])

    def test_nested_inline_suite_keeps_its_continuation_and_enclosing_function(self):
        source = """\
def reader_factory():
    def read(items): return (
        items[0] if items else None
    )
    if cached:
        return cached
    return read
"""
        functions = get_python_function_list(source)
        self.assertEqual([("reader_factory.read", 4, 2), ("reader_factory", 7, 2)], [
            (f.name, f.end_line, f.cyclomatic_complexity) for f in functions
        ])

    def test_inline_docstring_shares_a_source_line_with_code(self):
        source = 'def value(): """Return the value."""; return 1'
        functions = get_python_function_list(source)
        self.assertEqual([("value", 1)], [(f.name, f.nloc) for f in functions])

    def test_nested_depth_metric_multiple_continuous_loop_statements(self):
        class namespace9:
            def function1():
                if IamOnEarth:
                    if IamOnShip:
                        return toMars()
        functions = get_python_function_list(inspect.getsource(namespace9))
        self.assertEqual(1, len(functions))
        self.assertEqual("function1", functions[0].name)
        self.assertEqual(3, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[0].max_nesting_depth)
        self.assertEqual(5, functions[0].end_line)

    def xtest_nested_depth_metric_multiple_discrete_loop_statement(self):
        class namespace10:
            def function1():
                if IamOnEarth:
                    if not IamOnShip:
                        return toMars()
                elif IamOnMoon:
                    return backEarth()
        functions = get_python_function_list(inspect.getsource(namespace10))
        self.assertEqual(1, len(functions))
        self.assertEqual("function1", functions[0].name)
        self.assertEqual(4, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[0].max_nesting_depth)
        self.assertEqual(7, functions[0].end_line)

    def test_odd_blank_line(self):
        code = "class c:\n" + \
                "    def f():\n" +\
                "  \n" +\
                "         pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].end_line)

    def test_odd_line_with_comment(self):
        code = "class c:\n" + \
                "    def f():\n" +\
                "  #\n" +\
                "         pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(4, functions[0].end_line)

    def test_tab_is_same_as_8_spaces(self):
        code = ' ' * 7 + "def a():\n" + \
               '\t' + "pass\n"
        functions = get_python_function_list(code)
        self.assertEqual(2, functions[0].end_line)

    def xtest_if_elif_and_or_for_while_except_finally(self):
        code = 'def a():\n' + \
                '    if elif and or for while except finally\n'
        functions = get_python_function_list(code)
        self.assertEqual(9, functions[0].cyclomatic_complexity)
        self.assertEqual(8, functions[0].max_nesting_depth)

    def test_return_annotation_interpolations_keep_the_whole_function(self):
        for annotation in (
                'f"{version}"',
                'f"{metadata["["]}"',
                'f"""Schema {metadata["tag"]}"""',
                't"{metadata["["]}"'):
            with self.subTest(annotation=annotation):
                source = (
                    'class Parser:\n'
                    '    def read(self, value) -> Annotated[int, ' + annotation + ',\n'
                    '        "payload",\n'
                    '    ]:\n'
                    '        if value:\n'
                    '            return 1\n'
                    '        return 0\n')
                functions = get_python_function_list(source)
                self.assertEqual([('read', 2, 7, 2)], [
                    (f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                    for f in functions
                ])
