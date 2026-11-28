import pytest

from lizard import analyze_file


def analyze_literal(literal):
    source = (
        'class Example {\n'
        ' void real() {\n'
        f'  var text = {literal};\n'
        '  if (ready) next();\n'
        '  var tail = "end";\n'
        ' }\n'
        ' void after() {}\n'
        '}\n')
    return analyze_file.analyze_source_code('example.cs', source).function_list


@pytest.mark.parametrize('literal', [
    r'@"C:\"',
    '@"he said ""if (hidden) { }"""',
    '@"first ""\nif (hidden) { }\nlast"',
])
def test_verbatim_string_contents_are_inert(literal):
    functions = analyze_literal(literal)
    assert [f.name for f in functions] == ['Example::real', 'Example::after']
    assert [f.cyclomatic_complexity for f in functions] == [2, 1]


@pytest.mark.parametrize('length', [3, 4, 5])
@pytest.mark.parametrize('multiline', [False, True])
def test_raw_strings_match_the_opening_quote_count(length, multiline):
    delimiter = '"' * length
    content = '"' * (length - 2) + ' if (hidden) { while (x) {} }'
    if multiline:
        content = '\n' + content + '\n'
    else:
        content = 'text ' + content + ' text'
    functions = analyze_literal(delimiter + content + delimiter)
    assert [f.name for f in functions] == ['Example::real', 'Example::after']
    assert [f.cyclomatic_complexity for f in functions] == [2, 1]
