import pytest

from lizard import analyze_file


@pytest.mark.parametrize('name', ['if', 'while', 'for', 'a non syntactic name'])
def test_quoted_member_names_do_not_add_decisions(name):
    code = 'real <- function(x) { return(x$`' + name + '`) }'
    functions = analyze_file.analyze_source_code('sample.r', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 1)]


def test_real_conditions_still_count_after_a_quoted_name():
    code = 'real <- function(x) { y <- x$`if`; if (y) return(y) }'
    functions = analyze_file.analyze_source_code('sample.r', code).function_list
    assert [(f.name, f.cyclomatic_complexity) for f in functions] == [('real', 2)]
