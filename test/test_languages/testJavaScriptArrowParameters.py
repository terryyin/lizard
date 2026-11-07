"""Arrow signatures share formal parameter names across their consumers."""
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
    return analyze_file.analyze_source_code('arrows.' + extension,
                                            source).function_list


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue',
                                      'cjs', 'mjs'])
def test_anonymous_and_named_bindings_agree(extension):
    source = (
        'const named = ({key: renamed}, tail) => { return tail; };\n'
        'run(({key: renamed}, tail) => { return tail; });')
    result = functions(source, extension)
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('named', 2, ['renamed', 'tail']),
        ('(anonymous)', 2, ['renamed', 'tail'])]


@pytest.mark.parametrize('pattern,names', [
    ('{key: renamed, other}, [head, ...tail], last', ['other', 'tail', 'last']),
    ('{outer: {key: renamed}}, tail', ['renamed', 'tail']),
    ("{'key': renamed}, tail", ['renamed', 'tail']),
    ('{[key]: renamed}, tail', ['renamed', 'tail']),
    ('$, $$, a$, a$b, _, _x, $x, x1, café',
     ['$', '$$', 'a$', 'a$b', '_', '_x', '$x', 'x1', 'café']),
])
def test_anonymous_patterns_preserve_named_convention(pattern, names):
    function, = functions('run((' + pattern + ') => { return 1; });')
    assert function.parameters == names
    assert function.parameter_count == len(names)


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_reported_arrow_forms(extension):
    result = functions(
        'const total = list.reduce((sum, item) => sum + item.price, 0);\n'
        'const names = list.map(item => item.name);\n'
        'const pick = flag ? (a, b) => a : null;\n'
        'const sizes = list.map((x: Item, i: number): number => x.size * i);',
        extension)
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('(anonymous)', 2, ['sum', 'item']),
        ('(anonymous)', 1, ['item']),
        ('(anonymous)', 2, ['a', 'b']),
        ('(anonymous)', 2, ['x', 'i'])]


def test_bare_named_and_anonymous_arrows():
    result = functions(
        'const f = x => { return x; };\n'
        'const g = async x => { return x; };\n'
        'run($ => { return $; });\n'
        'run(a$b => { return a$b; });')
    assert [(f.name, f.parameter_count, f.parameters) for f in result] == [
        ('f', 1, ['x']), ('g', 1, ['x']),
        ('(anonymous)', 1, ['$']), ('(anonymous)', 1, ['a$b'])]


def test_defaults_and_wrapper_call():
    default, wrapped = functions(
        'list.map((a = flag ? 1 : 2, b = make(1, 2), ...rest) => '
        '{ return a || b; });\n'
        'const load = api.wrap((x, y) => { return x + y; });')
    assert (default.parameter_count, default.parameters,
            default.cyclomatic_complexity) == (3, ['a', 'b', 'rest'], 5)
    assert (wrapped.name, wrapped.parameter_count,
            wrapped.parameters) == ('(anonymous)', 2, ['x', 'y'])


def test_arrow_parameter_metric_consumers(capsys):
    info = analyze_file.analyze_source_code(
        'arrows.js', 'run(($, $$, a$) => { return 1; });')
    options = parse_args(['lizard', '-a', '2'])
    csv_output(AllResult([info]), options)
    assert capsys.readouterr().out.split(',')[3] == '3'
    assert '3 PARAM' in checkstyle_output(AllResult([info]), True)
    assert list(warning_filter(options, [info])) == info.function_list
    html_output([info], options)
    assert '<td class="greater-value">3</td>' in capsys.readouterr().out


def test_arrow_duplicate_lists_use_established_alias_names():
    info = analyze_file.analyze_source_code(
        'arrows.js',
        'const named = ({key: renamed}, a, b, c, d) => {};\n'
        'run(({key: renamed}, a, b, c, d) => {});')
    extension = LizardExtension()
    list(extension.cross_file_process([info]))
    assert [f.parameter_list_duplicates for f in info.function_list] == [2, 2]
    assert extension.all_count['renamed,a,b,c,d'] == 2


def test_cli_reports_arrow_parameters_and_threshold(tmp_path):
    source = tmp_path / 'arrows.js'
    source.write_text('run(($, $$, a$) => { return 1; });')
    result = subprocess.run(
        [sys.executable, '-m', 'lizard', '-a', '2', str(source)],
        capture_output=True, text=True)
    assert result.returncode == 1
    rows = [line for line in result.stdout.splitlines()
            if '(anonymous)@' in line]
    assert rows
    assert all(row.split()[3] == '3' for row in rows)
    assert 'parameter_count > 2' in result.stdout


@pytest.mark.parametrize('extension', ['.js', '.ts'])
def test_website_returns_actual_arrow_parameter_count(extension):
    from index import app

    response = app.test_client().post('/analyze', json={
        'lang': extension,
        'code': 'run(({key: renamed}, tail) => { return tail; });'})
    assert response.status_code == 200
    function, = response.get_json()['function_list']
    assert (function['name'], function['parameter_count']) == ('(anonymous)', 2)
