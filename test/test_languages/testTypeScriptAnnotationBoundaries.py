"""Runtime labels and TypeScript types keep their own syntax and delimiters."""
import csv
import io
import subprocess
import sys

import pytest

from .testJavaScriptBindingBoundaries import SUFFIXES, functions, summaries


AFTER = 'function after(tail) { return tail ? 1 : 0; }\n'


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('statement', ['a: 1', 'again: { if(flag) use(flag); }',
                                       'again: while(flag) { break again; }'])
def test_labels_keep_actual_body_and_successor(suffix, statement):
    source = ('const x = [() => {\n ' + statement + '\n'
              ' if(next) use(next);\n}];\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 2 + ('if(' in statement or 'while(' in statement),
         1, 4, []), ('after', 2, 5, 5, ['tail'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_minimal_label_retains_anonymous_function(suffix):
    result = functions('const x = [() => { a: 1 }];\n' + AFTER, suffix)
    assert summaries(result) == [
        ('(anonymous)', 1, 1, 1, []), ('after', 2, 2, 2, ['tail'])]
    assert [(f.nloc, f.token_count, f.parameter_count) for f in result] == [
        (1, 5, 0), (1, 12, 1)]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('field', ['private d: (() => void)[] = [];',
                                  'd: (n: number) => number;',
                                  'd: { b: (n: number) => string };',
                                  'd: (() => void)[]',
                                  'd: Map<string, number[]>'])
def test_type_only_fields_do_not_create_runtime_functions(suffix, field):
    source = ('class A {\n ' + field + '\n'
              ' constructor(value) { if(value) use(value); }\n'
              ' s(tail) { return tail ? 1 : 0; }\n}\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('constructor', 2, 3, 3, ['value']), ('s', 2, 4, 4, ['tail']),
        ('after', 2, 6, 6, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_typed_field_retains_its_actual_runtime_initializer(suffix):
    source = ('const C = class extends Base {\n'
              ' d: (n: number) => number = (n) => n ? 1 : 0;\n'
              ' s(tail) { return tail ? 1 : 0; }\n};\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('d', 2, 2, 2, ['n']), ('s', 2, 3, 3, ['tail']),
        ('after', 2, 5, 5, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('type_', ['number', '{ b: string }',
                                 '{ b: (n: number) => string }',
                                 'Map<string, (() => number)[]>', '`p${string}`'])
@pytest.mark.parametrize('container', ['[() => x as TYPE]',
                                     '[() => x as TYPE, (y) => y || 0]',
                                     '{ run() { return x as TYPE }}'])
def test_assertion_returns_runtime_delimiters(suffix, type_, container):
    source = 'const x = ' + container.replace('TYPE', type_) + ';\n' + AFTER
    expected = ([('run', 1, 1, 1, [])] if container.startswith('{') else
                [('(anonymous)', 1, 1, 1, [])])
    if ', (y)' in container:
        expected.append(('(anonymous)', 2, 1, 1, ['y']))
    assert summaries(functions(source, suffix)) == expected + [
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('type_,ccn', [
    ('number[]', 3), ('(n: number) => number', 3),
    ('Map<string, number[]>', 3),
    ('[number, { callback: () => number }]', 3),
    ('T extends number ? {v: T} : string[]', 5), ('`p${string}`', 3)])
def test_nested_annotation_types_preserve_formals_and_runtime_arrows(suffix, type_, ccn):
    source = ('const f = (x: ' + type_ + ', y = flag ? 1 : 0): '
              + type_ + ' => x;\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('f', ccn, 1, 1, ['x', 'y']), ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_variable_and_object_property_annotations_keep_runtime_values(suffix):
    source = ('const f: (x: number) => number = (x) => x || 0;\n'
              'const o = {class: (x: number): number => x, '
              'value: {run: () => 1}};\n'
              'const t = () => `${f(1) ? 1 : 0}` as string;\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('f', 2, 1, 1, ['x']), ('class', 1, 2, 2, ['x']),
        ('run', 1, 2, 2, []), ('t', 2, 3, 3, []),
        ('after', 2, 4, 4, ['tail'])]


@pytest.mark.parametrize('suffix', SUFFIXES)
@pytest.mark.parametrize('extension,ccn', [('NS', 3), ('nonstrict', 2)])
def test_labeled_callback_extensions_keep_decisions(suffix, extension, ccn):
    source = ('const x = [() => {\n again: 1;\n'
              ' if(a && b) use(a);\n}];\n' + AFTER)
    result = functions(source, suffix, extension)
    assert summaries(result) == [
        ('(anonymous)', ccn, 1, 4, []), ('after', 2, 5, 5, ['tail'])]
    if extension == 'NS':
        assert [f.max_nested_structures for f in result] == [1, 0]


@pytest.mark.parametrize('suffix', SUFFIXES)
def test_label_cli_csv_and_output_option(tmp_path, suffix):
    source = 'const x = [() => { a: 1 }];\n' + AFTER
    if suffix == 'vue':
        source = '<script lang="ts">' + source + '</script>'
    path, output = tmp_path / ('labels.' + suffix), tmp_path / 'result.csv'
    path.write_text(source)
    result = subprocess.run([sys.executable, '-m', 'lizard', '--csv',
                             '-o', str(output), str(path)],
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    rows = list(csv.reader(io.StringIO(output.read_text())))
    assert [(r[7], int(r[1]), int(r[3]), int(r[9]), int(r[10])) for r in rows] == [
        ('(anonymous)', 1, 0, 1, 1), ('after', 2, 1, 2, 2)]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_http_returns_only_actual_functions_after_type_annotation(suffix):
    from index import app

    source = ('class A { d: (() => void)[] = []; s(x) {return x?1:0;} }\n'
              + AFTER)
    if suffix == 'vue':
        source = '<script lang="ts">' + source + '</script>'
    response = app.test_client().post('/analyze', json={
        'filename': 'types.' + suffix, 'code': source})
    assert response.status_code == 200
    assert [(f['name'], f['cyclomatic_complexity'], f['parameter_count'],
             f['start_line'], f['end_line'])
            for f in response.get_json()['function_list']] == [
                ('s', 2, 1, 1, 1), ('after', 2, 1, 2, 2)]


@pytest.mark.parametrize('suffix', ('js', 'jsx', 'mjs', 'cjs'))
def test_javascript_class_colon_values_remain_runtime_members(suffix):
    source = ('class A { g:(x,y)=>x+1; h:function(){return 1;} }\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('g', 1, 1, 1, ['x', 'y']), ('h', 1, 1, 1, []),
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('tag', ['<script>', '<script lang="js">',
                                 '<script setup lang="ts">'])
def test_vue_script_mode_owns_class_member_colons(tag):
    typed = 'lang="ts"' in tag
    member = 'd: (() => void)[] = [];' if typed else 'd:(x)=>x;'
    source = tag + 'class A {' + member + ' s() {} }' + '</script>'
    from lizard import analyze_file

    assert [f.name for f in analyze_file.analyze_source_code(
        'modes.vue', source).function_list] == (['s'] if typed else ['d', 's'])


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
def test_generic_constraint_brace_does_not_own_class_members(suffix):
    source = ('class A<T extends {x:number}> { d: (()=>void)[]=[]; '
              'constructor(v:T){if(v)use(v);} '
              's(tail:number){return tail?1:0;} }\n' + AFTER)
    assert summaries(functions(source, suffix)) == [
        ('constructor', 2, 1, 1, ['v']), ('s', 2, 1, 1, ['tail']),
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('type_,expression', [('-1', '-1'),
                                             ('`p${string}`', '`p${x}`')])
def test_literal_return_type_keeps_arrow_formal(suffix, type_, expression):
    source = 'h((x): ' + type_ + ' => ' + expression + ');\n' + AFTER
    assert summaries(functions(source, suffix)) == [
        ('(anonymous)', 1, 1, 1, ['x']), ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('separator', [' ', '\n '])
def test_declarator_comma_keeps_type_ownership_across_line_break(suffix, separator):
    source = ('const x: number = 1,' + separator
              + 'run: (n: number) => number = n => n || 0;\n' + AFTER)
    line = 1 + separator.count('\n')
    assert summaries(functions(source, suffix)) == [
        ('run', 2, line, line, ['n']), ('after', 2, line + 1, line + 1, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('condition', ['x as T extends U ? V : W',
                                       '(x as T extends U ? V : W)'])
def test_conditional_type_returns_following_runtime_ternary(suffix, condition):
    source = ('const choose = ' + condition
              + ' ? function yes(a) {return a;} : function no(b) {return b;};\n'
              'function after(c) {return c;}\n')
    assert summaries(functions(source, suffix)) == [
        ('yes', 1, 1, 1, ['a']), ('no', 1, 1, 1, ['b']),
        ('after', 1, 2, 2, ['c'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('field,expected', [
    ('d?: (()=>void)[];', []), ('d ? : (()=>void)[];', []),
    ('$d?: (()=>void)[];', []),
    ('d ? : (n:number)=>number = (n)=>n||0;', [('d', 2, 1, 1, ['n'])]),
    ('$d?: (n:number)=>number = (n)=>n||0;', [('$d', 2, 1, 1, ['n'])]),
    ('d?: (n:number)=>number = (n)=>n||0;', [('d', 2, 1, 1, ['n'])]),
    ('d = flag ? function yes(n){return n||0;} : function no(n){return n&&1;};',
     [('yes', 2, 1, 1, ['n']), ('no', 2, 1, 1, ['n'])]),
    ('d = () => flag ? 1 : 0;', [('d', 2, 1, 1, [])])])
def test_optional_members_and_runtime_initializers_own_question_marks(
        suffix, field, expected):
    source = ('class A { ' + field
              + ' constructor(v){if(v)use(v);} s(tail){return tail?1:0;} }\n'
              'function after(tail){return tail?1:0;}\n')
    assert summaries(functions(source, suffix)) == expected + [
        ('constructor', 2, 1, 1, ['v']), ('s', 2, 1, 1, ['tail']),
        ('after', 2, 2, 2, ['tail'])]


@pytest.mark.parametrize('suffix', ('ts', 'tsx', 'vue'))
@pytest.mark.parametrize('extension', [None, 'NS', 'nonstrict', 'complextags'])
def test_optional_member_marker_is_not_an_enclosing_runtime_decision(suffix, extension):
    source = ('function outer(flag) { class A { $d ? : (()=>void)[]; '
              'd = flag ? function yes(n){return n||0;} : '
              'function no(n){return n&&1;}; } return flag?1:0; }\n' + AFTER)
    ccn = 1 if extension == 'nonstrict' else 2
    result = functions(source, suffix, extension)
    assert summaries(result) == [
        ('yes', ccn, 1, 1, ['n']), ('no', ccn, 1, 1, ['n']),
        ('outer', 3, 1, 1, ['flag']), ('after', 2, 2, 2, ['tail'])]
    if extension == 'complextags':
        assert result[2].complex_tags == [['?', 1], ['?', 1]]
        assert result[3].complex_tags == [['?', 2]]
