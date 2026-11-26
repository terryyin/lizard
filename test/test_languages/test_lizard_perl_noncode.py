import subprocess
import sys

import lizard
import pytest
from lizard import analyze_file


@pytest.mark.parametrize('directive', ['=pod', '=head1 EXAMPLES', '=over 4'])
def test_pod_does_not_define_functions(directive):
    source = directive + '\nsub fake { if ($x) {} }\n=cut\nsub real { return 1; }\n'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 4, 1)]
    assert result.nloc == 1


@pytest.mark.parametrize('marker', ['__DATA__', '__END__', '  __DATA__', '\t__END__'])
def test_data_markers_end_executable_source(marker):
    source = 'sub real { return 1; }\n' + marker + '\nsub fake { if ($x) {} }\n'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [f.name for f in result.function_list] == ['real']
    assert result.nloc == 1


def test_unterminated_pod_is_documentation_to_eof():
    source = 'sub real { 1; }\n=pod\nsub fake { if ($x) {} }'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [f.name for f in result.function_list] == ['real']
    assert result.nloc == 1


def test_data_marker_prefixes_are_normal_names():
    source = 'sub __DATA__value { 1; }\nsub __END__value { 2; }\n'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [f.name for f in result.function_list] == ['__DATA__value', '__END__value']


def test_generated_code_text_in_pod_does_not_end_analysis():
    source = '=head1 NOTES\nGENERATED CODE\n=cut\nsub real { if ($x) { 1 } }\n'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 4, 2)]
    assert result.nloc == 1


def test_generated_code_comment_still_ends_analysis():
    source = 'sub real { 1; }\n# GENERATED CODE\nsub skipped { 2; }\n'
    result = analyze_file.analyze_source_code('sample.pl', source)
    assert [f.name for f in result.function_list] == ['real']


@pytest.mark.parametrize(('prefix', 'line'), [
    ('=pod\n#lizard forgive\n=cut\n', 4),
    ('#lizard forgive\n', 2),
])
def test_forgive_text_keeps_perl_warning(tmp_path, prefix, line):
    source = tmp_path / 'warning.pl'
    source.write_text(prefix + 'sub real { if ($x) { 1 } if ($y) { 2 } if ($z) { 3 } }\n')
    result = subprocess.run(
        [sys.executable, lizard.__file__, '-C', '3', '-w', str(source)],
        capture_output=True, text=True, cwd=tmp_path,
    )
    assert result.returncode == 1, result.stderr
    assert f'{source}:{line}: warning: real has' in result.stdout
