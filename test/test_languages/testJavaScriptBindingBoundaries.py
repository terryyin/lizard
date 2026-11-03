"""Binding brackets keep callbacks, expression arrows and their successors apart."""
import csv
import io
import subprocess
import sys

import pytest

from lizard import FileAnalyzer, analyze_file, get_extensions


SUFFIXES = ('js', 'ts', 'jsx', 'tsx', 'mjs', 'cjs', 'vue')
ANONYMOUS = ('(anonymous)', 1, 1, 1, [])
AUTHORED = [
    ('map', "it('t', () => {\n  const m = new Map([\n"
     "    ['a', () => f(), { id: 1 }],\n  ]);\n  g(r);\n});\n",
     [('(anonymous)', 1, 3, 3, []), ('(anonymous)', 1, 1, 6, [])]),
    ('call', 'h(() => f(), { id: 1 });\ng(r);\n', [ANONYMOUS]),
    ('nested-array', 'h([[() => f(), { id: 1 }]]);\ng(r);\n', [ANONYMOUS]),
    ('array-object', 'const a = [() => f(), { id: 1 }];\ng(r);\n',
     [ANONYMOUS]),
    ('array-name', 'const a = [() => f(), x];\ng(r);\n', [ANONYMOUS]),
    ('block-control', 'const a = [() => { f(); }, { id: 1 }];\ng(r);\n',
     [ANONYMOUS]),
    ('default', 'h((a = 1) => f(), { id: 1 });\ng(r);\n',
     [('(anonymous)', 2, 1, 1, ['a'])]),
    ('object-arrow', 'h(() => f(), { id: () => g() });\ng(r);\n',
     [ANONYMOUS, ('id', 1, 1, 1, [])]),
    ('subscript', "it('t', () => {\n  e(r[0]);\n  g(x);\n});\n",
     [('(anonymous)', 1, 1, 4, [])]),
    ('subscript-chain', "it('t', () => {\n  e(r[0]).t([{}]);\n});\n",
     [('(anonymous)', 1, 1, 3, [])]),
    ('object-array', 'function f() {\n  return { w: [m[n - 1]] };\n}\n'
     'function g() {}\n', [('f', 1, 1, 3, []), ('g', 1, 4, 4, [])]),
]


def functions(source, suffix, extension=None):
    if suffix == 'vue':
        source = '<script lang="ts">' + source + '</script>'
    analyzer = (FileAnalyzer(get_extensions([extension]))
                if extension else analyze_file)
    return analyzer.analyze_source_code('bindings.' + suffix, source).function_list


def summaries(result):
    # These cases have named formals, with no empty destructuring slots.
    assert all(f.parameter_count == len(f.parameters) for f in result)
    return [(f.name, f.cyclomatic_complexity, f.start_line, f.end_line,
             f.parameters) for f in result]


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('name,source,expected', AUTHORED,
                         ids=[case[0] for case in AUTHORED])
def test_authored_binding_boundaries(suffix, name, source, expected):
    assert summaries(functions(source, suffix)) == expected


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('call', ['e(r[0]);', 'e(r[0]).t([{}]);',
                                  'e?.(r?.[0]).t([{}]);'])
def test_callback_keeps_decisions_and_following_formals(suffix, call):
    source = ("it('t', () => {\n " + call + '\n if(x) g(x);\n});\n'
              'function after(y) { return y ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 2, 1, 4, []), ('after', 2, 5, 5, ['y'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_object_array_keeps_decisions_and_following_formals(suffix):
    source = ('function f(x) {\n return {w: [m[x ? 1 : 0]]};\n}\n'
              'function after(y) { return y ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('f', 2, 1, 3, ['x']), ('after', 2, 4, 4, ['y'])]


@pytest.mark.parametrize('suffix', ('js', 'ts'))
@pytest.mark.parametrize('separator', (' ', '\n    '), ids=('single-line', 'multiline'))
def test_object_property_comparison_preserves_function_boundaries(suffix, separator):
    source = ('function a(xs) {\n'
              '  return { ok:' + separator + 'xs.length < 3 };\n'
              '}\n'
              'function b(x) {\n'
              '  if (x) return 1;\n'
              '  return 2;\n'
              '}\n')
    extra_line = separator.count('\n')
    assert summaries(functions(source, suffix)) == [
        ('a', 1, 1, 3 + extra_line, ['xs']),
        ('b', 2, 4 + extra_line, 7 + extra_line, ['x'])]


@pytest.mark.parametrize('separator', (' ', '\n    '), ids=('single-line', 'multiline'))
def test_typescript_generic_arrow_property_keeps_parameters(separator):
    source = 'const obj = { f:' + separator + '<T>(x: T) => x };\n'
    line = 1 + separator.count('\n')
    assert summaries(functions(source, 'ts')) == [('f', 1, line, line, ['x'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_template_substitutions_keep_members_and_enclosing_callback(suffix):
    source = ("it('t', () => {\n"
              ' h(() => f(), {value: `${`${r[0]}`}`, id: x => x || 0});\n'
              ' if(x) g(x);\n});\n'
              'function after(y) { return y ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 1, 2, 2, []), ('id', 2, 2, 2, ['x']),
        ('(anonymous)', 2, 1, 4, []), ('after', 2, 5, 5, ['y'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_typed_generic_and_ternary_arrows_keep_signature_owners(suffix):
    source = ('h(<T,>(x: T, tail: T): T => x, {id: (y: number): number => y});\n'
              'const f = ok ? (x): number => x : (y): number => y;\n'
              'function after(z: number) { return z ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 1, 1, 1, ['x', 'tail']), ('id', 1, 1, 1, ['y']),
        ('(anonymous)', 1, 2, 2, ['x']), ('(anonymous)', 1, 2, 2, ['y']),
        ('after', 2, 3, 3, ['z'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_default_initializer_and_comma_keep_expression_arrow_owners(suffix):
    source = ('h((x = flag ? 1 : 2, y = make(1, 2)) => x || y, '
              '{id: z => z && 1});\n'
              'function after(tail) { return tail ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 5, 1, 1, ['x', 'y']), ('id', 2, 1, 1, ['z']),
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('callee', ['h', 'api.h', 'new Factory', 'api?.h', 'api?.'])
@pytest.mark.parametrize('in_object', [False, True])
def test_argument_readers_finish_arrow_before_next_object(suffix, callee, in_object):
    call = callee + '(() => f(), {id: (x = 1) => x || 0})'
    source = (('const o = {value: ' + call + '}' if in_object else call) + ';\n'
              'function after(tail) { return tail ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ANONYMOUS, ('id', 3, 1, 1, ['x']), ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_global_arrow_generic_call_keeps_continued_expression(suffix):
    source = ('const f = (x) => foo<A,\n B>(x)\n'
              'function after(y) { return y ? 1 : 0; }\n')
    assert summaries(functions(source, suffix)) == [
        ('f', 1, 1, 2, ['x']), ('after', 2, 3, 3, ['y'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('extension,ccn', [('NS', 3), ('nonstrict', 2)])
def test_metric_extensions_keep_callback_decisions(suffix, extension, ccn):
    source = ("it('t', () => {\n e(r[0]);\n if(x && y) g(x);\n});\n"
              'function after(y) { return y ? 1 : 0; }\n')
    result = functions(source, suffix, extension)
    assert summaries(result) == [
        ('(anonymous)', ccn, 1, 4, []), ('after', 2, 5, 5, ['y'])]
    if extension == 'NS':
        assert [f.max_nested_structures for f in result] == [1, 0]


def test_cli_reports_complete_callback_and_following_metrics(tmp_path):
    path = tmp_path / 'bindings.tsx'
    path.write_text("it('t', () => {\n e(r[0]);\n if(x) g(x);\n});\n"
                    'function after(y) { return y ? 1 : 0; }\n')
    result = subprocess.run([sys.executable, '-m', 'lizard', '--csv', str(path)],
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    rows = list(csv.reader(io.StringIO(result.stdout)))
    assert [(row[7], int(row[1]), int(row[3]), int(row[9]), int(row[10]))
            for row in rows] == [('(anonymous)', 2, 0, 1, 4),
                                 ('after', 2, 1, 5, 5)]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_http_api_exposes_complete_callback_and_following_metrics(suffix):
    from index import app

    source = ("it('t', () => {\n e(r[0]);\n if(x) g(x);\n});\n"
              'function after(y) { return y ? 1 : 0; }\n')
    if suffix == 'vue':
        source = '<script lang="ts">' + source + '</script>'
    response = app.test_client().post('/analyze', json={
        'filename': 'bindings.' + suffix, 'code': source})
    assert response.status_code == 200
    assert [(f['name'], f['cyclomatic_complexity'], f['parameter_count'],
             f['start_line'], f['end_line'])
            for f in response.get_json()['function_list']] == [
                ('(anonymous)', 2, 0, 1, 4), ('after', 2, 1, 5, 5)]
