import inspect
import unittest
from textwrap import dedent

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
        self.assertEqual(1, functions[1].max_nesting_depth)

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

    def xtest_one_line_functions(self):
        class namespace8:
            def a(): pass

            def b(): pass
        functions = get_python_function_list(inspect.getsource(namespace8))
        self.assertEqual("a", functions[0].name)
        self.assertEqual("b", functions[1].name)

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

    def test_nesting_depth_closes_suites_at_dedent(self):
        code = dedent("""
            def collect_ready(rows):
                if rows is None:
                    return []
                ready = []
                for row in rows:
                    if row.ready:
                        ready.append(row)
                return ready if ready else rows
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_counts_context_manager_and_sibling_handlers(self):
        code = dedent("""
            async def read_all(client):
                try:
                    async with client.session() as session:
                        return await session.read()
                except OSError:
                    if client.closed:
                        return b''
                    raise
                finally:
                    client.flush()
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_counts_with_suite(self):
        code = dedent("""
            async def read_buffer(client):
                async with client.session() as session:
                    if session.ready:
                        return await session.read()
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_counts_alternative_branch_body(self):
        code = dedent("""
            def emit(record, write):
                if record.skip:
                    if record.log:
                        write("skipped")
                elif record.urgent:
                    write(record)
                else:
                    while record.has_data():
                        write(record.read())
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_keeps_inline_suite_through_semicolon(self):
        code = dedent("""
            def render(record, write):
                if record.visible: write(record); write(record.text if record.loud else "")
                if record.done: write("done")
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_keeps_multiline_expression_in_its_suite(self):
        code = dedent("""
            def available(primary, backup, use_primary):
                if primary is None: return backup
                if (primary
                        if use_primary else backup):
                    return primary
            """)
        function, = get_python_function_list(code)
        self.assertEqual(2, function.max_nesting_depth)

    def test_nesting_depth_separates_sibling_ternaries(self):
        code = """
        def pack(ready, cached):
            return (1 if ready else 0, 2 if cached else 0)

        def choose(enabled, ready, cached):
            return (1 if ready else 0, 2 if cached else 0) if enabled else ()
        """
        functions = get_python_function_list(code)
        self.assertEqual([('pack', 1), ('choose', 2)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_nesting_depth_counts_defaults_with_the_enclosing_function(self):
        code = """
        def create(ready, value):
            def save(item=value if ready else None):
                return item
            return save
        """
        functions = get_python_function_list(code)
        self.assertEqual([('create.save', 0), ('create', 1)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_nesting_depth_counts_one_word_operator_level_in_a_condition(self):
        code = dedent(r"""
            def eligible(account, policy, override):
                if account.active \
                        and (policy.permits(account) or override):
                    if account.verified:
                        return True
                return False
            """)
        function, = get_python_function_list(code)
        self.assertEqual(3, function.max_nesting_depth)

    def test_nesting_depth_counts_word_operators_in_separate_statements(self):
        code = dedent("""
            def choose(account, backup, default):
                enabled = account and account.enabled
                return (account or backup) and enabled or default
            """)
        function, = get_python_function_list(code)
        self.assertEqual(1, function.max_nesting_depth)

    def test_nesting_depth_resets_word_operators_at_an_inline_suite(self):
        code = dedent("""
            def resolve(account, backup, default):
                if account and account.enabled: return backup or default
                return default
            """)
        function, = get_python_function_list(code)
        self.assertEqual(3, function.max_nesting_depth)

    def test_nesting_depth_counts_default_logic_with_its_outer_scope(self):
        code = dedent("""
            def create(ready, value, enabled):
                def save(item=(ready and value) if enabled else False):
                    return item
                return save
            """)
        functions = get_python_function_list(code)
        self.assertEqual([('create.save', 0), ('create', 2)],
                         [(f.name, f.max_nesting_depth) for f in functions])

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
