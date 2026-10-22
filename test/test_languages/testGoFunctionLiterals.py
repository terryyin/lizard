import pytest
from lizard import analyze_file
from .testGo import get_go_function_list


def test_anonymous_function_as_argument_keeps_its_signature_and_complexity():
    result = get_go_function_list('''package p
func main() {
    consume(func(n int) error {
        if n > 0 { return nil }
        return nil
    })
}
''')
    assert [f.name for f in result] == ['anonymous__0', 'main']
    assert result[0].long_name == 'anonymous__0 n int'
    assert result[0].full_parameters == ['n int']
    assert result[0].cyclomatic_complexity == 2
    assert (result[0].start_line, result[0].end_line) == (3, 6)


def test_anonymous_numbers_follow_source_order_across_nested_scopes():
    result = get_go_function_list('''package p
func outer() {
    if true {
        f := func() { consume(func() {}) }
        _ = f
    }
    func() {}()
}
func other() { func() {}() }
''')
    assert [f.name for f in result] == [
        'anonymous__1', 'anonymous__0', 'anonymous__2', 'outer',
        'anonymous__3', 'other']


def test_anonymous_numbers_restart_on_each_file_analysis():
    for filename in ['first.go', 'second.go', 'first.go']:
        result = analyze_file.analyze_source_code(filename, '''package p
func outer() { consume(func() {}); consume(func() {}) }
''').function_list
        assert [f.name for f in result] == [
            'anonymous__0', 'anonymous__1', 'outer']


def test_named_literals_do_not_use_anonymous_numbers():
    result = get_go_function_list('''package p
var handler = func(n int) int { return n }
var table = map[string]func(){ "a": func() {} }
var other Handler = func() {}
var list = []func(){ func() {} }
func (m *T) method() {}
''')
    assert [f.name for f in result] == [
        'handler', 'anonymous__0', 'other', 'anonymous__1', 'method']


@pytest.mark.parametrize('expression', [
    'defer func() {}()',
    'go func() {}()',
    '_ = <-func() chan int { return nil }()',
])
def test_invoked_literals_receive_anonymous_names(expression):
    result = get_go_function_list('func outer() { ' + expression + ' }')
    assert [f.name for f in result] == ['anonymous__0', 'outer']
