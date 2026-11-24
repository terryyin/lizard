from lizard import analyze_file


def test_text_block_contents_are_not_counted_as_java_code():
    code = ('class A { void real() { String text = """\n'
            'a " if (x) { fake(); }\n'
            '"""; }\nvoid after() {} }')
    functions = analyze_file.analyze_source_code('sample.java', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('A::real', 1, 3), ('A::after', 1, 4)]


def test_escaped_triple_quote_does_not_end_a_text_block():
    code = ('class A { void real() { String text = """\n'
            r'\""" if (x) { fake(); }' '\n'
            '"""; if (ok) go(); } }')
    functions = analyze_file.analyze_source_code('sample.java', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('A::real', 2)]
