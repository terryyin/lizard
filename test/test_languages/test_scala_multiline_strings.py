from lizard import analyze_file


def test_multiline_string_keeps_embedded_quotes_and_definitions_as_text():
    code = 'def real(): String = {\n""" a " if (x) { def fake() = 1 }\ntext """\n}\ndef after() = 1'
    functions = analyze_file.analyze_source_code('sample.scala', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('real', 1, 4), ('after', 1, 5)]


def test_quote_before_multiline_terminator_does_not_swallow_following_function():
    code = 'def real() = """quoted""""\ndef after() = if (ok) 1 else 0'
    functions = analyze_file.analyze_source_code('sample.scala', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 1), ('after', 2)]
