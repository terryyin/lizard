"""Formal slots count independently of names and annotation syntax."""
import subprocess
import sys

import pytest

from lizard import AllResult, analyze_file, parse_args, warning_filter
from lizard_ext.checkstyleoutput import checkstyle_output
from lizard_ext.csvoutput import csv_output
from lizard_ext.htmloutput import html_output
from lizard_ext.lizardduplicated_param_list import LizardExtension


def functions(source, extension='js'):
    if extension == 'vue':
        source = '<script>\n' + source + '\n</script>'
    return analyze_file.analyze_source_code('slots.' + extension,
                                            source).function_list


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue',
                                      'cjs', 'mjs'])
def test_named_and_anonymous_empty_patterns_occupy_slots(extension):
    result = functions(
        'const named = ({}, [], tail,) => { return tail; };\n'
        'run(({}, [], tail,) => { return tail; });', extension)
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('named', 3, ['tail']), ('(anonymous)', 3, ['tail'])]


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_receiver_and_trailing_commas_do_not_invent_slots(extension):
    result = functions(
        'function receiverOnly(this: Window,) {}\n'
        'function empty(this: Window, {}, [],) {}\n'
        'const named = (value,) => { return value; };\n'
        'run(() => {});', extension)
    assert [(f.parameter_count, f.parameters) for f in result] == [
        (0, []), (2, []), (1, ['value']), (0, [])]


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_callback_type_annotation_has_no_runtime_function(extension):
    result = functions(
        'run((number: number, $: Map<string, number>, '
        'cb: (value: number, index: number) => boolean, '
        'tail?: string): number => { return number; });\n'
        'function after(value) { return value; }', extension)
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('(anonymous)', 4, ['number', '$', 'cb', 'tail']),
        ('after', 1, ['value'])]


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_function_return_annotation_keeps_outer_arrow(extension):
    result = functions(
        'run((x: number, y: number): ((n: number) => number) => '
        '{ return n => n + x + y; });\n'
        'function after(value) { return value; }', extension)
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('(anonymous)', 1, ['n']), ('(anonymous)', 2, ['x', 'y']),
        ('after', 1, ['value'])]


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_generic_and_object_return_annotations_preserve_formals(extension):
    result = functions(
        'run(<T,>(x: T, tail: T): T => { return x; });\n'
        'const named = <T,>(x: T, tail: T): T => { return x; };\n'
        'run((x: number, y: number): {value: number} => '
        '{ return {value: x + y}; });', extension)
    assert [(f.parameter_count, f.parameters) for f in result] == [
        (2, ['x', 'tail']), (2, ['x', 'tail']), (2, ['x', 'y'])]


def test_nested_and_ternary_defaults_keep_complexity_owners():
    result = functions(
        'run(({key: renamed = flag ? 1 : 2}, tail = fallback || 0) => '
        '{ return tail; });\n'
        'function outer(flag) { const a = flag ? '
        '(x = fallback || 0, y) => { return x && y; } : null; return a; }')
    assert [(f.name, f.parameters, f.cyclomatic_complexity) for f in result] == [
        ('(anonymous)', ['renamed', 'tail'], 5),
        ('(anonymous)', ['x', 'y'], 4), ('outer', ['flag'], 2)]


def test_empty_slots_reach_count_outputs_and_warning_threshold(capsys):
    info = analyze_file.analyze_source_code(
        'slots.js', 'run(({}, [], tail) => { return tail; });')
    options = parse_args(['lizard', '-a', '2'])
    csv_output(AllResult([info]), options)
    assert capsys.readouterr().out.split(',')[3] == '3'
    assert '3 PARAM' in checkstyle_output(AllResult([info]), True)
    assert list(warning_filter(options, [info])) == info.function_list
    html_output([info], options)
    assert '<td class="greater-value">3</td>' in capsys.readouterr().out


def test_empty_slots_do_not_add_duplicate_parameter_keys():
    info = analyze_file.analyze_source_code(
        'slots.js', 'const named = ({}, [], a, b, c, d, e) => {};\n'
        'run(({}, [], a, b, c, d, e) => {});')
    extension = LizardExtension()
    list(extension.cross_file_process([info]))
    assert [f.parameter_count for f in info.function_list] == [7, 7]
    assert [f.parameter_list_duplicates for f in info.function_list] == [2, 2]
    assert dict(extension.all_count) == {'a,b,c,d,e': 2}


def test_cli_counts_empty_slots_for_parameter_warnings(tmp_path):
    source = tmp_path / 'slots.js'
    source.write_text('run(({}, [], tail) => { return tail; });')
    result = subprocess.run(
        [sys.executable, '-m', 'lizard', '-a', '2', str(source)],
        capture_output=True, text=True)
    assert result.returncode == 1
    rows = [line for line in result.stdout.splitlines()
            if '(anonymous)@' in line]
    assert rows and all(row.split()[3] == '3' for row in rows)
    assert 'parameter_count > 2' in result.stdout


@pytest.mark.parametrize('extension', ['.js', '.ts'])
def test_website_json_counts_empty_slots(extension):
    from index import app

    response = app.test_client().post('/analyze', json={
        'lang': extension,
        'code': 'run(({}, [], tail) => { return tail; });'})
    assert response.status_code == 200
    function, = response.get_json()['function_list']
    assert (function['name'], function['parameter_count']) == ('(anonymous)', 3)
