import pytest

from lizard import analyze_file


@pytest.mark.parametrize('equals', ['', '=', '==', '====='])
def test_long_comments_do_not_create_functions(equals):
    code = ('function real()\n--[' + equals + '[\n'
            'function fake() if x then end end\n'
            ']' + equals + ']\nreturn 1\nend')
    functions = analyze_file.analyze_source_code('sample.lua', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [('real', 1, 6)]


@pytest.mark.parametrize('equals', ['=', '==', '====='])
def test_long_string_only_ends_at_matching_bracket_level(equals):
    code = ('function real()\nlocal text = [' + equals + '[\n'
            ']] if x then end\n]' + equals + ']\nreturn text\nend')
    functions = analyze_file.analyze_source_code('sample.lua', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [('real', 1, 6)]
