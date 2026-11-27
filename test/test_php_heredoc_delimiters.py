import pytest

from lizard import analyze_file


@pytest.mark.parametrize('quote', ['', '"', "'"])
def test_heredoc_label_inside_text_does_not_end_the_literal(quote):
    code = ('<?php\nfunction real() {\n$s = <<<' + quote + 'TXT' + quote + '\n'
            'prefix TXT suffix\nif ($x) { fake(); } ?>\nTXT;\nreturn $s;\n}\n'
            'function after() { if ($x) return 1; }')
    functions = analyze_file.analyze_source_code('sample.php', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('real', 1, 8), ('after', 2, 9)]


def test_indented_terminator_does_not_match_a_longer_identifier():
    code = '<?php function real() {\n$s = <<<TXT\n  TXT_MORE\n  if ($x) f();\n  TXT;\n}'
    functions = analyze_file.analyze_source_code('sample.php', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [('real', 1, 6)]
