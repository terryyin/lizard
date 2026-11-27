from lizard import analyze_file


def test_backtick_function_name_with_spaces_is_discovered():
    code = 'fun `if you can`() { return }\nfun after() {}'
    functions = analyze_file.analyze_source_code('sample.kt', code).function_list
    assert len(functions) == 2
    assert 'if you can' in functions[0].name
    assert [f.cyclomatic_complexity for f in functions] == [1, 1]
    assert functions[1].name == 'after'


def test_keywords_inside_quoted_property_names_are_not_decisions():
    code = 'fun real() { println(obj.`while it lasts`); if (ok) return }'
    functions = analyze_file.analyze_source_code('sample.kt', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 2)]
