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

    def test_token_factory_replays_dynamic_format_fields_in_source_order(self):
        from lizard_languages.python import PythonReader

        for field, pieces in (
                ('value:{width}', ('value', ':', '{', 'width', '}')),
                ('value!s:>{width}.{precision}',
                 ('value', '!', 's', ':', '>', '{', 'width', '}', '.',
                  '{', 'precision', '}'))):
            with self.subTest(field=field):
                source = 'f"{' + field + '}"'
                calls = []

                def rename_width(match):
                    token = match.group()
                    rename = token == 'width' and any(call[0] == 'value' for call in calls)
                    calls.append((token, match.span(), match.string, match.pos, match.endpos))
                    return f'width_at_{match.start() - match.pos}' if rename else token

                tokens = list(PythonReader.generate_tokens(source, token_class=rename_width))

                expected_calls = [
                    ('f', (0, 1), source, 0, len(source)),
                    (source[1:], (1, len(source)), source, 0, len(source)),
                ]
                offset = 0
                for piece in pieces:
                    end = offset + len(piece)
                    expected_calls.append((piece, (offset, end), field, 0, len(field)))
                    offset = end
                self.assertEqual(expected_calls, calls)
                width_token = f'width_at_{field.index("width")}'
                self.assertEqual(
                    ['f'] + [width_token if piece == 'width' else piece for piece in pieces],
                    tokens)

    def test_token_factory_keeps_source_scan_origin_across_fstrings(self):
        from lizard_languages.python import PythonReader

        source = 'f"{value}" + f"{count}" + label'
        calls = []
        replacements = {'"{value}"': '"{price}"', '"{count}"': '"{quantity}"', 'label': 'unit'}

        def rename_label(match):
            token = match.group()
            calls.append((token, match.span(), match.string, match.pos, match.endpos))
            return replacements.get(token, token) if match.pos == 0 else token

        tokens = list(PythonReader.generate_tokens(source, token_class=rename_label))

        self.assertEqual(['f', 'price', ' ', '+', ' ', 'f', 'quantity', ' ', '+', ' ', 'unit'], tokens)
        self.assertEqual([
            ('f', (0, 1), source, 0, len(source)),
            ('"{value}"', (1, 10), source, 0, len(source)),
            ('price', (0, 5), 'price', 0, 5),
            (' ', (10, 11), source, 0, len(source)),
            ('+', (11, 12), source, 0, len(source)),
            (' ', (12, 13), source, 0, len(source)),
            ('f', (13, 14), source, 0, len(source)),
            ('"{count}"', (14, 23), source, 0, len(source)),
            ('quantity', (0, 8), 'quantity', 0, 8),
            (' ', (23, 24), source, 0, len(source)),
            ('+', (24, 25), source, 0, len(source)),
            (' ', (25, 26), source, 0, len(source)),
            ('label', (26, 31), source, 0, len(source)),
        ], calls)

    def test_token_factory_keeps_source_scan_origin_after_reused_quotes(self):
        from lizard_languages.python import PythonReader

        source = 'f"{record["value"]}" + label'
        calls = []

        def rename_label(match):
            token = match.group()
            if token == 'label':
                calls.append((match.span(), match.string, match.pos, match.endpos))
                if match.pos == 0:
                    return 'unit'
            return token

        tokens = list(PythonReader.generate_tokens(source, token_class=rename_label))

        self.assertEqual(['f', 'record', '[', '"value"', ']', ' ', '+', ' ', 'unit'], tokens)
        self.assertEqual([((23, len(source)), source, 0, len(source))], calls)

    def test_custom_token_pattern_keeps_precedence_and_capture_groups(self):
        from lizard_languages.python import PythonReader

        for source, addition, expected in (
                ('f"{value}"', r'|f"[^"]*"', ['f"{value}"']),
                ('cache.cache', r'|(\w+)\.\1', ['cache.cache']),
                ('kind = "text"', r'|(?P<interpolated>kind)', ['kind', ' ', '=', ' ', '"text"'])):
            with self.subTest(addition=addition):
                self.assertEqual(expected, list(PythonReader.generate_tokens(source, addition)))

    def test_token_factory_folds_hash_values_in_source_and_expressions(self):
        from lizard_languages.python import PythonReader

        def comment_out_debug(match):
            return '#' if match[0] == 'debug' else match[0]

        for source, expected in (
                ('debug = 1\nvalue = 2', ['# = 1', '\n', 'value', ' ', '=', ' ', '2']),
                ('f"{debug + 1}"', ['f', '# + 1'])):
            with self.subTest(source=source):
                tokens = list(PythonReader.generate_tokens(source, token_class=comment_out_debug))
                self.assertEqual(expected, tokens)

    def test_format_hash_flag_leaves_width_visible_to_the_factory(self):
        from lizard_languages.python import PythonReader

        def rename_width(match):
            return 'columns' if match[0] == 'width' else match[0]

        tokens = list(PythonReader.generate_tokens('f"{value:#{width}x}"', token_class=rename_width))

        self.assertEqual(['f', 'value', ':', '#', '{', 'columns', '}', 'x'], tokens)

    def test_field_factory_waits_until_the_consumer_advances_past_the_prefix(self):
        from lizard_languages.python import PythonReader

        calls = []

        def record_value(match):
            calls.append(match[0])
            return match[0]

        tokens = iter(PythonReader.generate_tokens('f"{value}"', token_class=record_value))

        self.assertEqual('f', next(tokens))
        self.assertEqual(['f', '"{value}"'], calls)
        self.assertEqual(['value'], list(tokens))
        self.assertEqual(['f', '"{value}"', 'value'], calls)

    def test_token_factory_uses_custom_patterns_only_in_the_source_scan(self):
        from lizard_languages.python import PythonReader

        source = 'f"{value}" + value'
        addition = r'|(?P<metric>value)|(?P<format_prefix>f)'
        calls = []

        def rename_metric(match):
            token = match.group()
            if token in ('f', 'value'):
                calls.append((token, match.lastgroup, match.span(), match.string, match.pos, match.endpos))
            return 'metric' if match.lastgroup == 'metric' else token

        tokens = list(PythonReader.generate_tokens(source, addition, rename_metric))

        self.assertEqual(['f', 'value', ' ', '+', ' ', 'metric'], tokens)
        self.assertEqual([
            ('f', 'format_prefix', (0, 1), source, 0, len(source)),
            ('value', None, (0, 5), 'value', 0, 5),
            ('value', 'metric', (13, 18), source, 0, len(source)),
        ], calls)

    def test_token_factory_keeps_explicit_quote_pattern(self):
        from lizard_languages.python import PythonReader

        source = 'f"{value}"'
        matches = []

        def mark_quote(match):
            if match.lastgroup == 'quote':
                matches.append((match.span(), match.string, match.pos, match.endpos))
                return 'quote'
            return match.group()

        tokens = list(PythonReader.generate_tokens(source, r'|(?P<quote>")', mark_quote))

        self.assertEqual(['f', 'quote', '{', 'value', '}', 'quote'], tokens)
        self.assertEqual([((1, 2), source, 0, 10), ((9, 10), source, 0, 10)], matches)

    def test_token_factory_transforms_fstring_literal_text(self):
        from lizard_languages.python import PythonReader

        tokens = list(PythonReader.generate_tokens(
            'f"status: {value}"', token_class=lambda match: match.group().upper()))
        self.assertEqual(['F', '"STATUS: "', 'VALUE'], tokens)

    def test_token_factory_transforms_nested_quoted_tokens_in_order(self):
        from lizard_languages.python import PythonReader

        calls = []

        def rename_label(match):
            text = match.group()
            calls.append((text, match.span()))
            return text.replace('ready', 'available') if text.startswith("'") else text

        tokens = list(PythonReader.generate_tokens(
            'f"{f\'{ready}\'}"', token_class=rename_label))

        self.assertEqual(['f', 'f', 'available'], tokens)
        self.assertEqual([
            ('f', (0, 1)), ('"{f\'{ready}\'}"', (1, 15)),
            ('f', (0, 1)), ("'{ready}'", (1, 10)), ('available', (0, 9)),
        ], calls)

    def test_token_factory_can_replace_an_fstring_prefix(self):
        from lizard_languages.python import PythonReader

        def replace_prefix(match):
            text = match.group()
            return {'f': 'prefix', 'r': 'f'}.get(text, text)

        for source, expected in (
                ('f"{ready}"', ['prefix', '"{ready}"']),
                ('r"{ready}"', ['f', 'ready']),
                ('F"{r\'{ready}\'}"', ['F', 'f', 'ready']),
                ('F"{f\'{ready}\'}"', ['F', 'prefix', "'{ready}'"])):
            with self.subTest(source=source):
                tokens = list(PythonReader.generate_tokens(source, token_class=replace_prefix))
                self.assertEqual(expected, tokens)
