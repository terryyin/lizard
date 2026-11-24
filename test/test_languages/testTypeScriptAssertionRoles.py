"""Type annotations and type arguments preserve runtime owners."""
import pytest

from lizard import FileAnalyzer, get_extensions

from .testJavaScriptBindingBoundaries import SUFFIXES, functions, summaries


AFTER = 'function after(tail) {return tail?1:0;}\n'
CALL_CONTEXTS = [('ternary', 'const result = flag ? CALL : value;\n', 1, None),
 ('logical', 'const result = flag || CALL;\n', 1, None),
 ('export-default', 'export default CALL;\n', 1, None),
 ('unary', 'const result = !CALL;\n', 1, None),
 ('new', 'const result = new CALL;\n', 1, None),
 ('return', 'function outer(flag) { return CALL; }\n', 1, 1),
 ('throw', 'function outer(flag) { throw CALL; }\n', 1, 1),
 ('case',
  'function outer(flag) { switch(flag) { case CALL: return 1; } }\n',
  1,
  2),
 ('else', 'function outer(flag) { if(flag) use(); else CALL; }\n', 1, 2),
 ('if', 'function outer(flag) { if(flag) CALL; }\n', 1, 2),
 ('await', 'async function outer(flag) { return await CALL; }\n', 1, 1),
 ('semicolon-line', 'f();\nCALL;\n', 2, None),
 ('call-line', 'f()\nCALL;\n', 2, None)]


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('word', ['as', 'satisfies'])
@pytest.mark.parametrize('name,template,line,outer', CALL_CONTEXTS,
                         ids=[case[0] for case in CALL_CONTEXTS])
def test_contextual_identifier_is_an_operand(
        suffix, word, name, template, line, outer):
    source = template.replace('CALL', word + '((arg) => arg || 0)') + AFTER
    expected = [('(anonymous)', 2, line, line, ['arg'])]
    if outer is not None:
        expected.append(('outer', outer, 1, 1, ['flag']))
    successor_line = template.count('\n') + 1
    assert summaries(functions(source, suffix)) == expected + [
        ('after', 2, successor_line, successor_line, ['tail'])]


@pytest.mark.parametrize('suffix', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('word', ['as', 'satisfies'])
@pytest.mark.parametrize('operand', ['value', '(value)', 'make()',
                                     'new Value()', 'value!', 'as'])
def test_assertion_follows_a_completed_operand(suffix, word, operand):
    source = ('const run = () => ' + operand + ' ' + word
              + ' {b: (n:number)=>number};\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('run', 1, 1, 1, []), ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('type_', ['number', 'Map<string, number[]>'])
@pytest.mark.parametrize('operator', ['>', '>=', '<=', 'instanceof', 'in',
                                      '^', '>>', '>>>', '<<'])
def test_assertion_returns_root_runtime_operator(suffix, type_, operator):
    source = ('const run = () => value as ' + type_ + ' ' + operator
              + ' call((arg) => arg || 0);\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 2, 1, 1, ['arg']), ('run', 1, 1, 1, []),
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('type_', [
    'Box<<T>(x:T)=>T>', 'import("m")<(n:number)=>number>',
    'typeof import("m").factory<(n:number)=>number>'])
@pytest.mark.parametrize('form', ['assertion', 'return'])
def test_nested_generic_function_type_keeps_its_openers(suffix, type_, form):
    if form == 'assertion':
        source = 'const run = () => value as ' + type_ + ';\n' + AFTER
        expected = ('run', 1, 1, 1, [])
    else:
        source = 'h((arg): ' + type_ + ' => arg);\n' + AFTER
        expected = ('(anonymous)', 1, 1, 1, ['arg'])
    assert summaries(functions(source, suffix)) == [
        expected, ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('factory', [
    'Array<string>',
    'Array<{ name: string }>',
    'Array<<T = string>(value: T) => T>',
])
def test_arrow_ends_after_instantiation_expression(suffix, factory):
    source = """function build(cached: boolean) {
  const empty = () => %s
  if (cached) return empty
}
""" % factory
    functions = analyze(suffix, source)
    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions] == [
        ('empty', 2, 2, 1), ('build', 1, 4, 2)]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('factory', ['Array<string[]>', 'Array<typeof Math.PI>'])
def test_arrow_ends_before_call_statement(suffix, factory):
    source = """function build() {
  const empty = () => %s
  console.log('Factory ready')
  return empty
}
""" % factory
    arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('empty', 2, 2)


@pytest.mark.parametrize('suffix,params', [('ts', '<T>'), ('tsx', '<T,>')])
@pytest.mark.parametrize('name', ['Factory', r'\u0046actory', r'F\u{61}ctory'])
def test_generic_function_type_alias_has_no_function_body(suffix, params, name):
    source = """type Result<T> = { value: T }
type %s = %s(
  initializer: T,
) => Result<T>
const factory: Factory = (value) => ({ value })
""" % (name, params)
    functions = analyze(suffix, source)
    assert [(f.start_line, f.end_line) for f in functions] == [(5, 5)]
    if name == 'Factory':
        assert functions[0].parameters == ['value']


def test_adjacent_function_type_aliases_do_not_add_functions():
    source = """type Make<Input, Output> = (input: Input) => Output
type MakeAsync<Input, Output> = (
  input: Input,
  options?: { debug?: boolean },
) => Promise<Output>

const make = <T>(value: T): T => value
"""
    function, = analyze('ts', source)
    assert (function.start_line, function.end_line, function.cyclomatic_complexity) == (7, 7, 1)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('value', [
    'Array<string>',
    'Array<ReadonlyArray<string>>',
    'Array<{ name: string }>',
    'value as Box<string>',
])
def test_sibling_conditions_after_type_arguments(suffix, value):
    source = """type Box<T> = { value: T }

function select(value: unknown, cached: boolean) {
  if (cached) return %s
  if (value) return String(value)
}
""" % value
    function, = analyze(suffix, source)
    assert (function.max_nesting_depth, function.max_nested_structures) == (1, 1)


def test_non_null_receiver_can_take_type_arguments():
    source = """function identity<T>(value: T) {
  return value
}
function select(source: typeof identity | undefined, cached: boolean) {
  if (cached) return source!<string>
  if (source) return source<number>
}
"""
    _, function = analyze('ts', source)
    assert (function.name, function.start_line, function.end_line, function.cyclomatic_complexity) == (
        'select', 4, 7, 3)
    assert (function.max_nesting_depth, function.max_nested_structures) == (1, 1)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_greater_than_comparison_can_continue_on_next_line(suffix):
    source = """function isOverLimit(total: number, limit: number) {
  const exceeds = () => total >
    limit
  return exceeds
}
"""
    arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('exceeds', 2, 3)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_right_shift_keeps_its_operand_after_a_line_break(suffix):
    source = """function isWithinLimit(count: number, limit: number, scale: number) {
  const withinLimit = () => count < limit >>
    scale
  return withinLimit
}
"""
    arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('withinLimit', 2, 3)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_shift_in_a_comparison_operand_preserves_arrow(suffix):
    source = """function withinLimits(total: number, offset: number, enabled: boolean) {
  const allowed = () => total < (offset >> 1) >
    !enabled
  return allowed
}
"""
    arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('allowed', 2, 3)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('operand', [
    'Boolean(offset + 1)',
    '(offset instanceof Error)',
    'offset.valueOf()',
])
def test_comparison_operand_can_continue_on_next_line(suffix, operand):
    # Each expression inside the angle brackets is a value, so '>' compares
    # it with the next line's operand.
    source = """function withinLimits(total: boolean, offset: any, enabled: boolean) {
  const allowed = () => total < %s >
    !enabled
  return allowed
}
""" % operand
    arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('allowed', 2, 3)


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('operand', [
    '(key in flags)',
    '({ value: enabled }).value',
    'delete flags[key]',
])
def test_comparison_retains_condition_in_arrow(suffix, operand):
    source = """function build(low: boolean, key: string, flags: Record<string, boolean>, enabled: boolean) {
  const passes = () => low < %s >
    !(enabled && key !== '')
  return passes
}
""" % operand
    functions = analyze(suffix, source)
    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions] == [
        ('passes', 2, 3, 2), ('build', 1, 5, 1)]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
@pytest.mark.parametrize('operand', [
    '`build-${revision + 1}`',
    'typeof "build"',
    'typeof 1',
    'typeof typeof ready',
])
def test_comparison_with_string_operand_preserves_arrow(suffix, operand):
    source = """function build(lower: string, revision: number, enabled: boolean, ready: boolean) {
  const passes = () => lower < %s >
    !(enabled && ready)
  return passes
}
""" % operand
    functions = analyze(suffix, source)
    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions] == [
        ('passes', 2, 3, 2), ('build', 1, 5, 1)]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_super_property_is_a_comparison_operand(suffix):
    source = """class Base {
  get minimum() { return true }
}
class Derived extends Base {
  check(failed: boolean, enabled: boolean, ready: boolean) {
    const allowed = () => failed < super.minimum >
      !(enabled && ready)
    return allowed
  }
}
"""
    functions = analyze(suffix, source)
    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions if f.name in ('allowed', 'check')] == [
                ('allowed', 6, 7, 2), ('check', 5, 9, 1)]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_call_can_continue_after_instantiation_expression(suffix):
    source = """function wrap<T>(value: T) { return { value } }

function transfer(value: string) {
  const send = () => wrap<string>
    (value)
  return send
}
"""
    _, arrow, _ = analyze(suffix, source)
    assert (arrow.name, arrow.start_line, arrow.end_line) == ('send', 4, 5)


def analyze(suffix, source):
    analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
    return analyzer.analyze_source_code('example.' + suffix, source).function_list
