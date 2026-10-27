"""Parameter identifiers remain names across readers and metric consumers."""
import pytest

from lizard import AllResult, analyze_file, parse_args, warning_filter
from lizard_ext.checkstyleoutput import checkstyle_output
from lizard_ext.csvoutput import csv_output
from lizard_ext.htmloutput import html_output
from lizard_ext.lizardduplicated_param_list import LizardExtension


def functions(source, extension='js'):
    return analyze_file.analyze_source_code('parameters.' + extension,
                                            source).function_list


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue'])
def test_identifier_boundaries(extension):
    source = 'function names($, $$, a$, a$b, _, _x, $x, x1, café) {}'
    if extension == 'vue':
        source = '<script>\n' + source + '\n</script>'
    function, = functions(source, extension)
    assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',
                                   '$x', 'x1', 'café']
    assert function.parameter_count == 9


@pytest.mark.parametrize('extension', ['ts', 'tsx'])
def test_keyword_names_and_annotations(extension):
    source = (
        'function round(number: number, precision: number) { return 1; }\n'
        'function widest(string: string): number { return 1; }\n'
        'function json(object: any, _: unknown): T { return 1; }\n'
        'function typed(value: Map<string, number>, '
        'cb: (item: string, index: number) => boolean, '
        'data: { a: number, b: string }, tail?: string) {}')
    result = functions(source, extension)
    assert [(f.name, f.parameter_count) for f in result] == [
        ('round', 2), ('widest', 1), ('json', 2), ('typed', 4)]
    assert [f.parameters for f in result] == [
        ['number', 'precision'], ['string'], ['object', '_'],
        ['value', 'cb', 'data', 'tail']]


@pytest.mark.parametrize('binding', ["{'key': renamed}", '{1: renamed}',
                                     '{[key]: renamed}',
                                     '{key: renamed, other}',
                                     '[head, ...rest]'])
@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx'])
def test_binding_colons_are_not_annotations(binding, extension):
    function, = functions('function unpack(' + binding + ', tail) {}',
                          extension)
    assert function.parameter_count == 2


def test_typed_defaults_keep_initializer_complexity():
    function, = functions(
        'function defaults(number: number = flag ? 1 : 2, '
        '$y: Map<string, number> = new Map(), tail: string = "x") {}', 'ts')
    assert function.parameters == ['number', '$y', 'tail']
    assert function.parameter_count == 3
    assert function.cyclomatic_complexity == 5


@pytest.mark.parametrize('extension,source,names', [
    ('py', 'def plain(value: int = 1, *args, **kwargs):\n return value',
     ['value', 'args', 'kwargs']),
    ('cpp', 'int plain(int value, int tail = 1) {}', ['value', 'tail']),
    ('rb', 'def plain(value, tail = 1)\n value\nend', ['value', 'tail']),
    ('php', '<?php function plain($value) {} ?>', ['$value']),
])
def test_shared_parameter_names(extension, source, names):
    function, = functions(source, extension)
    assert function.parameters == names
    assert function.parameter_count == len(names)


def test_duplicated_parameter_lists_preserve_dollar_names():
    info = analyze_file.analyze_source_code('parameters.js',
        'function first($a, $b, $c, $d, $e) {}\n'
        'function same($a, $b, $c, $d, $e) {}\n'
        'function distinct(a, b, c, d, e) {}')
    extension = LizardExtension()
    list(extension.cross_file_process([info]))
    assert [f.parameter_list_duplicates for f in info.function_list] == [2, 2, 1]
    assert extension.all_count['$a,$b,$c,$d,$e'] == 2


def test_parameter_outputs_and_warning_threshold(capsys):
    info = analyze_file.analyze_source_code(
        'parameters.js', 'function names($, $$, a$) {}')
    options = parse_args(['lizard', '-a', '2'])
    csv_output(AllResult([info]), options)
    assert capsys.readouterr().out.split(',')[3] == '3'
    assert '3 PARAM' in checkstyle_output(AllResult([info]), True)
    assert list(warning_filter(options, [info])) == info.function_list
    html_output([info], options)
    assert '<td class="greater-value">3</td>' in capsys.readouterr().out
