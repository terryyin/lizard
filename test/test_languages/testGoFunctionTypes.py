import pytest
from .testGo import get_go_function_list


@pytest.mark.parametrize('conversion', [
    '(func(int) int)(nil)',
    '(func() func() int)(nil)',
    '(func() struct{ F func() })(nil)',
])
def test_function_type_conversion_preserves_following_functions(conversion):
    result = get_go_function_list('package p\nvar handler = ' + conversion + '''
func after() { if true {} }
func tail() {}
''')
    assert [f.name for f in result] == ['after', 'tail']
    assert result[0].start_line == 3
    assert result[0].cyclomatic_complexity == 2


@pytest.mark.parametrize('separator', ['\n', '; '])
def test_local_function_type_does_not_consume_callback(separator):
    result = get_go_function_list('''package p
func consume(f func()) { f() }
func outer() {
    var handler func()''' + separator + '''    consume(func() { if true {} })
    _ = handler
}
func after() {}
''')
    assert [f.name for f in result] == ['consume', '', 'outer', 'after']
    assert result[1].cyclomatic_complexity == 2
    assert result[1].start_line == (5 if separator == '\n' else 4)
    assert result[1].nloc == 1
    assert result[2].cyclomatic_complexity == 1


@pytest.mark.parametrize('declared_type', [
    '', 'func(int) error', 'Handler', 'http.HandlerFunc',
])
def test_literal_uses_declared_variable_name(declared_type):
    result = get_go_function_list(
        'package p\nvar handler ' + declared_type + ''' =
func(a int) error { if a > 0 {} ; return nil }
func after() {}
''')
    assert [f.name for f in result] == ['handler', 'after']
    assert result[0].start_line == 3
    assert result[0].parameter_count == 1
    assert result[0].cyclomatic_complexity == 2


@pytest.mark.parametrize('function_type', [
    'func()',
    'func() func()',
    'func() func() func(int) error',
    'func() struct{ F func() }',
    'func() interface{ Handle(func()) }',
    'func() map[string][]chan<- func()',
    'func() *Box[func()]',
])
def test_complete_function_type_ends_before_next_declaration(function_type):
    result = get_go_function_list('package p\nvar factory ' + function_type + '''
func after() { if true {} }
''')
    assert [f.name for f in result] == ['after']
    assert result[0].cyclomatic_complexity == 2


def test_function_types_in_assertions_and_type_switches_are_not_functions():
    result = get_go_function_list('''package p
func outer(x interface{}) {
    _ = x.(func())
    switch x.(type) {
    case func():
        if true {}
    }
    if true {}
}
func after() {}
''')
    assert [f.name for f in result] == ['outer', 'after']
    assert result[0].cyclomatic_complexity == 4


def test_signature_lookahead_preserves_comments_lines_and_raw_strings():
    result = get_go_function_list('''package p
var handler /* variable type */ func(
    int,
) error = func( /* parameter */ a int) error {
    query := `func() { if true {} }`
    if a > 0 { return nil }
    return nil
}
func after() {}
''')
    assert [f.name for f in result] == ['handler', 'after']
    assert result[0].start_line == 4
    assert result[0].end_line == 8
    assert result[0].nloc == 5
    assert result[0].parameter_count == 1
    assert result[0].cyclomatic_complexity == 2


@pytest.mark.parametrize('return_type', [
    'struct{ F func() }', 'interface{ Handle(func()) }',
    'map[string][]func()', 'func() func()',
    'chan<- func()', '<-chan func()', '**Handler',
])
def test_literal_body_follows_complete_result_type(return_type):
    result = get_go_function_list('var handler = func() ' + return_type + ''' {
    if true {}
    panic("unused")
}
func after() {}
''')
    assert [f.name for f in result] == ['handler', 'after']
    assert result[0].end_line == 4
    assert result[0].cyclomatic_complexity == 2


def test_newline_after_func_keyword_does_not_terminate_literal():
    result = get_go_function_list('var handler = func\n(a int) { if true {} }')
    assert [f.name for f in result] == ['handler']
    assert result[0].parameter_count == 1
    assert result[0].cyclomatic_complexity == 2
