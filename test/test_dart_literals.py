import pytest

from lizard import analyze_file


def assert_literal_does_not_hide_code(literal, quote):
    source = (
        'void real() {\n'
        f'  var text = {literal};\n'
        '  if (ready) next();\n'
        f'  var tail = {quote}end{quote};\n'
        '}\n'
        'void after() {}\n')
    functions = analyze_file.analyze_source_code(
        'example.dart', source).function_list
    assert [f.name for f in functions] == ['real', 'after']
    assert [f.cyclomatic_complexity for f in functions] == [2, 1]


@pytest.mark.parametrize('quote', ["'", '"'])
def test_backslash_does_not_escape_the_end_of_a_raw_string(quote):
    assert_literal_does_not_hide_code('r' + quote + 'C:\\' + quote, quote)


@pytest.mark.parametrize('quote', ["'", '"'])
def test_escaped_quote_run_does_not_end_a_regular_triple_string(quote):
    delimiter = quote * 3
    literal = (delimiter + '\ntext \\' + delimiter +
               '\nif (hidden) { while (x) {} }\n' + delimiter)
    assert_literal_does_not_hide_code(literal, quote)


@pytest.mark.parametrize('quote', ["'", '"'])
def test_raw_triple_string_ends_even_after_a_backslash(quote):
    delimiter = quote * 3
    literal = 'r' + delimiter + '\nif (hidden) { }\\' + delimiter
    assert_literal_does_not_hide_code(literal, quote)
