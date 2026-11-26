import unittest

from .testPython import get_python_function_list


class Test_Python_fstring_complexity(unittest.TestCase):
    """Control flow inside f-string {...} interpolations must be counted (#317)."""

    def test_fstring_comprehension_counts_for(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"{\', \'.join([x for x in items])}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_fstring_ternary_counts_if(self):
        functions = get_python_function_list(
            'def f(cond):\n    return f"{\'a\' if cond else \'b\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_fstring_logical_operators_counted(self):
        functions = get_python_function_list(
            'def f(a, b):\n    return f"{a and b or a}"\n')
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_fstring_floor_division_keeps_following_condition(self):
        functions = get_python_function_list(
            'def f(a, b, c):\n    return f"{a // b or c}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_floor_division_in_fstring_literal_does_not_add_complexity(self):
        functions = get_python_function_list(
            'def f(value):\n    return f"a // b or c {value}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_fstring_matches_non_fstring_equivalent(self):
        with_fs = get_python_function_list(
            'def f(i):\n    return f"{[x for x in i]}"\n')
        without = get_python_function_list(
            'def f(i):\n    return [x for x in i]\n')
        self.assertEqual(without[0].cyclomatic_complexity,
                         with_fs[0].cyclomatic_complexity)

    def test_keyword_inside_nested_string_not_counted(self):
        # the 'if' lives inside a nested string literal; only 'or' is a real condition
        functions = get_python_function_list(
            'def f(x):\n    return f"{x or \'use this if empty\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_escaped_braces_are_not_interpolation(self):
        # {{ and }} are literal braces, so the 'if' is plain text, not a condition
        functions = get_python_function_list(
            'def f():\n    return f"{{ keep this if you can }}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_plain_fstring_has_base_complexity(self):
        functions = get_python_function_list(
            'def f(name):\n    return f"hello {name}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_nested_fstring_counts_inner_control_flow(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"{f\'{[x for x in items]}\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_brace_inside_nested_string_in_interpolation(self):
        # '}' inside a nested string must not terminate the interpolation early
        functions = get_python_function_list(
            'def f(x):\n    return f"{x or \'}\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_ternary_after_string_containing_brace(self):
        functions = get_python_function_list(
            'def f(cond):\n    return f"{foo(\'}\') if cond else bar}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_format_spec_does_not_add_complexity(self):
        functions = get_python_function_list(
            'def f(x):\n    return f"{x:.2f}"\n')
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_triple_quoted_fstring_with_interpolation(self):
        functions = get_python_function_list(
            'def f(items):\n    return f"""{\', \'.join([x for x in items])}"""\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_bytes_fstring_counts_control_flow(self):
        functions = get_python_function_list(
            'def f(items):\n    return bf"{b\'x\' if items else b\'\'}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_reused_quotes_keep_nested_fstring_decision(self):
        functions = get_python_function_list(
            "def render(decorator):\n"
            "    return f'{f'@{decorator}\\n' if decorator else ''}def run()'\n"
            "\n"
            "def next_step():\n"
            "    return 1\n")
        self.assertEqual(
            [('render', 2, 2), ('next_step', 5, 1)],
            [(f.name, f.end_line, f.cyclomatic_complexity) for f in functions])

    def test_hash_flag_before_dynamic_width_keeps_decision(self):
        functions = get_python_function_list(
            'def render(value, padded):\n'
            '    return f"{value:#{8 if padded else 4}x}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_comment_inside_dynamic_width_hides_braces(self):
        functions = get_python_function_list(
            'def render(value, padded):\n'
            '    return f"""{value:#{(8 if padded else 4)  # } if ignored\n'
            '    }x}"""\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_quote_fill_before_dynamic_width_keeps_decision(self):
        functions = get_python_function_list(
            'def render(value, padded):\n'
            '    return f"{value:\'>{8 if padded else 4}x}"\n')
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_triple_quoted_format_text_counts_its_code_line(self):
        functions = get_python_function_list(
            'def message(value):\n'
            '    return f"""The value is\n'
            '    {value:x}\n'
            '    """\n')
        self.assertEqual(4, functions[0].nloc)

    def test_fstring_fields_use_the_custom_token_factory(self):
        from lizard_languages.python import PythonReader

        positions = {}

        def uppercase(match):
            text = match.group()
            positions[text] = match.span()
            return text if text.startswith('"') else text.upper()

        tokens = list(PythonReader.generate_tokens(
            'f"{ready if enabled else fallback}"', token_class=uppercase))

        self.assertEqual(
            ['F', 'READY', ' ', 'IF', ' ', 'ENABLED', ' ', 'ELSE', ' ', 'FALLBACK'],
            tokens)
        self.assertEqual((0, 1), positions['f'])
        self.assertEqual((0, 5), positions['ready'])
        self.assertEqual((6, 8), positions['if'])

    def test_explicit_crlf_join_is_a_single_python_token(self):
        from lizard_languages.python import PythonReader

        source = 'return 1 + \\\r\n    2\r\n'
        tokens = list(PythonReader.generate_tokens(source))
        self.assertIn('\\\n', tokens)
        self.assertNotIn('\\', tokens)

    def test_token_factory_transforms_fstring_literal_text(self):
        from lizard_languages.python import PythonReader

        tokens = list(PythonReader.generate_tokens(
            'f"status: {value}"', token_class=lambda match: match.group().upper()))
        self.assertEqual(['F', '"STATUS: "', 'VALUE'], tokens)
