import pytest

from lizard import analyze_file


@pytest.mark.parametrize('quote', ["'", '"'])
@pytest.mark.parametrize('contents', [
    'path\\',
    'if (hidden) then ! not a comment',
    'doubled {quote}{quote} if (hidden) then',
])
def test_character_literals_do_not_hide_following_statements(quote, contents):
    literal = quote + contents.format(quote=quote) + quote
    source = (
        '  subroutine real(x)\n'
        '    integer :: x\n'
        '    character(len=80) :: text\n'
        f'    text = {literal}\n'
        '    if (x > 0) then\n'
        f'      text = {quote}yes{quote}\n'
        '    end if\n'
        '  end subroutine real\n'
        '  subroutine after()\n'
        '  end subroutine after\n')
    functions = analyze_file.analyze_source_code(
        'example.f90', source).function_list
    assert [f.name.lower() for f in functions] == ['real', 'after']
    assert [f.cyclomatic_complexity for f in functions] == [2, 1]
    assert [f.end_line for f in functions] == [8, 10]
