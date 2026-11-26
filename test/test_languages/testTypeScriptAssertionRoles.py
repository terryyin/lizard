"""Contextual identifiers and type assertions retain their runtime owners."""
import pytest

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


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'svelte'))
@pytest.mark.parametrize('value_type', ['() => void', '(() => void)', 'Array<() => void>', '{ run: () => void }'])
def test_constructor_type_arguments_keep_only_runtime_callbacks(suffix, value_type):
    source = """const pending = new Map<string, %s>();
const timers = new globalThis.Map<string, () => void>([["retry", () => close()]]);
function close() { return pending.size }
""" % value_type
    offset = 0
    if suffix == 'svelte':
        source = '<script lang="ts">\n' + source + '</script>'
        offset = 1
    assert [(f.name, f.start_line, f.end_line) for f in functions(source, suffix)
            if f.name != '(component)'] == [
                ('(anonymous)', 2 + offset, 2 + offset), ('close', 3 + offset, 3 + offset)]


@pytest.mark.parametrize('suffix', ('ts', 'tsx'))
def test_constructor_comparison_keeps_a_runtime_arrow(suffix):
    source = """function isLater(limit: any) {
  return new Date < (() => new Date())() > limit
}
"""
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 1, 2, 2, []), ('isLater', 1, 1, 3, ['limit'])]
