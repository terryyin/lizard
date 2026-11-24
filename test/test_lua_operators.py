import pytest

from lizard import analyze_file


@pytest.mark.parametrize('expression', ['#items > 0', 'value // 2 > 0'])
def test_lua_operators_do_not_hide_the_rest_of_the_line(expression):
    code = 'function real(items, value) if ' + expression + ' then return 1 end end'
    functions = analyze_file.analyze_source_code('sample.lua', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 2)]


def test_length_operator_in_loop_header_preserves_body():
    code = 'function real(t)\nfor i = 1, #t do\nif t[i] then f() end\nend\nend'
    functions = analyze_file.analyze_source_code('sample.lua', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [('real', 3, 5)]
