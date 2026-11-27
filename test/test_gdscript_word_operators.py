import pytest

from lizard import FileAnalyzer, analyze_file, get_extensions


@pytest.mark.parametrize('condition, expected', [
    ('a and b', 3),
    ('a or b', 3),
    ('a and b or c', 4),
])
def test_word_form_boolean_operators_add_decisions(condition, expected):
    source = f'''func real(a, b, c):
    if {condition}:
        return 1
    return 0
'''
    functions = analyze_file.analyze_source_code(
        'example.gd', source).function_list
    assert len(functions) == 1
    assert functions[0].cyclomatic_complexity == expected


def test_operator_words_in_strings_comments_and_names_are_inert():
    functions = analyze_file.analyze_source_code('example.gd', '''
func real():
    # and or
    var ordinary = "and or"
    var candy = 'or and'
    return ordinary + candy
''').function_list
    assert functions[0].cyclomatic_complexity == 1


def test_nonstrict_mode_omits_word_form_boolean_operators():
    analyzer = FileAnalyzer(get_extensions(['nonstrict']))
    functions = analyzer.analyze_source_code('example.gd', '''
func real(a, b, c):
    if a and b or c:
        return 1
    return 0
''').function_list
    assert functions[0].cyclomatic_complexity == 2
