"""TypeScript receiver declarations do not add caller-supplied parameters."""
import subprocess
import sys
from pathlib import Path

import pytest

from lizard import AllResult, analyze_file, parse_args, warning_filter
from lizard_ext.checkstyleoutput import checkstyle_output
from lizard_ext.csvoutput import csv_output
from lizard_ext.htmloutput import html_output
from lizard_ext.lizardduplicated_param_list import LizardExtension


def functions(source, extension='ts'):
    return analyze_file.analyze_source_code('receiver.' + extension,
                                            source).function_list


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_authored_receiver_examples(extension):
    code = (
        "function a(this: Window, x: number) { return x; }\n"
        "const b = async function* (this: Reader) { yield 1; }\n"
        "function c(this: Map<string, number>, x: number) { return x; }\n"
    )
    result = functions(code, extension)
    assert [(f.name, f.parameter_count) for f in result] == [
        ('a', 1), ('b', 0), ('c', 1)]
    assert [f.parameters for f in result] == [['x'], [], ['x']]


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
@pytest.mark.parametrize('source', [
    'function read(this: Reader, value: string) { return this.value + value; }',
    'class Reader { read(this: Reader, value: string) '
    '{ return this.value + value; } }',
    'const reader = { read(this: Reader, value: string) '
    '{ return this.value + value; } };',
])
def test_receiver_in_functions_and_methods(source, extension):
    function, = functions(source, extension)
    assert (function.name, function.parameters, function.parameter_count) == (
        'read', ['value'], 1)
    assert function.cyclomatic_complexity == 1
    assert (function.start_line, function.end_line) == (1, 1)


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx'])
@pytest.mark.parametrize('binding', [
    '{first, this: receiver}', '{this: receiver}',
    '{this: receiver = this.fallback}',
])
def test_this_property_is_part_of_a_real_binding(binding, extension):
    function, = functions('function unpack(' + binding + ', tail) '
                          '{ return this.value; }', extension)
    assert function.parameters == ['receiver', 'tail']
    assert function.parameter_count == 2


def test_receiver_does_not_hide_actual_defaults_or_body_complexity():
    function, after = functions(
        "function defaults(this: Reader, number: number = flag ? 1 : 2, "
        "$y: Map<string, number> = new Map(), tail: string = 'x') "
        "{ return this.value; }\nfunction after(value) { return this.value; }")
    assert function.parameters == ['number', '$y', 'tail']
    assert function.parameter_count == 3
    assert function.cyclomatic_complexity == 5
    assert (function.start_line, function.end_line) == (1, 1)
    assert (after.name, after.parameters, after.cyclomatic_complexity) == (
        'after', ['value'], 1)
    assert (after.start_line, after.end_line) == (2, 2)


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue'])
def test_body_and_default_member_access_preserve_real_parameters(extension):
    source = ('function defaults(value = this.fallback, '
              'tail = this.make(1, 2)) { return this.value; }')
    if extension == 'vue':
        source = '<script>\n' + source + '\n</script>'
    function, = functions(source, extension)
    assert function.parameters == ['value', 'tail']
    assert function.parameter_count == 2
    assert function.cyclomatic_complexity == 3


def test_receiver_parameter_consumers(capsys):
    info = analyze_file.analyze_source_code('receiver.ts',
        'function first(this: Reader, a, b, c, d, e) {}\n'
        'function same(a, b, c, d, e) {}')
    extension = LizardExtension()
    list(extension.cross_file_process([info]))
    assert [f.parameter_list_duplicates for f in info.function_list] == [2, 2]
    assert extension.all_count['a,b,c,d,e'] == 2
    options = parse_args(['lizard', '-a', '5'])
    assert list(warning_filter(options, [info])) == []
    csv_output(AllResult([info]), options)
    assert capsys.readouterr().out.split(',')[3] == '5'
    assert '5 PARAM' in checkstyle_output(AllResult([info]), True)
    html_output([info], options)
    assert '<td class="lesser-value">5</td>' in capsys.readouterr().out


def test_receiver_cli_warning_threshold(tmp_path):
    source = tmp_path / 'receiver.ts'
    source.write_text('function onClick(this: HTMLElement, event: Event) {\n'
                      '  return event;\n}\n')
    facade = Path(__file__).resolve().parents[2] / 'lizard.py'
    result = subprocess.run([sys.executable, str(facade), '-a', '1',
                             str(source)], text=True, capture_output=True)
    assert result.returncode == 0, result.stdout + result.stderr
    row = next(line for line in result.stdout.splitlines()
               if 'onClick@1-3@' in line)
    assert row.split()[:5] == ['3', '1', '14', '1', '3']
