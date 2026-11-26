import pytest

from lizard import analyze_file


@pytest.mark.parametrize('prefix', ['', 'u8', 'u', 'U', 'L'])
@pytest.mark.parametrize('delimiter', ['tag', '_END_', 'abcdefghijklmnop'])
def test_custom_raw_delimiters_preserve_function_boundaries(prefix, delimiter):
    literal = prefix + 'R"' + delimiter + '(text )" if (x) { fake(); })' + delimiter + '"'
    code = 'void real() {\nauto s = ' + literal + ';\n}\nvoid after() {}'
    functions = analyze_file.analyze_source_code('sample.cpp', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('real', 1, 3), ('after', 1, 4)]
