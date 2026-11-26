import pytest

from lizard import analyze_file


@pytest.mark.parametrize('prefix', ['r', 'br', 'cr'])
@pytest.mark.parametrize('hashes', ['#', '##', '#####'])
def test_raw_strings_keep_quotes_comments_and_keywords_as_text(prefix, hashes):
    literal = prefix + hashes + '"\ntext " if x { /* text */ fn fake() {}\n"' + hashes
    code = 'fn real() {\nlet text = ' + literal + ';\n}\nfn after() {}'
    functions = analyze_file.analyze_source_code('sample.rs', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('real', 1, 5), ('after', 1, 6)]


def test_raw_string_without_hashes_does_not_treat_backslash_as_quote_escape():
    code = 'fn real() { let s = r"path\\"; if true {} let t = "end"; }\nfn after() {}'
    functions = analyze_file.analyze_source_code('sample.rs', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 2), ('after', 1)]
