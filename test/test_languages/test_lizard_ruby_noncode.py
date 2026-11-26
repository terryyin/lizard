import subprocess
import sys

import lizard
import pytest
from lizard import analyze_file


@pytest.mark.parametrize(('source', 'line'), [
    ('def real\n 1\nend\n__END__\ndef fake\n if x\n end\nend\n', 1),
    ('def real\n=begin\nif fake\n=end\n1\nend\n', 1),
    ('=begin example\ndef fake\n if x\n end\nend\n=end\ndef real\n1\nend\n', 7),
])
def test_documentation_and_data_do_not_count_as_code(source, line):
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', line, 1)]
    assert result.nloc == 3


@pytest.mark.parametrize('expression', ['__END__value', '__END__ = 1'])
def test_only_a_bare_data_marker_line_ends_executable_source(expression):
    source = f'def real\n{expression}\n if x\n  1\n end\nend\n'
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert result.nloc == 6
    assert [(f.name, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 2)]


def test_generated_code_text_in_documentation_does_not_end_analysis():
    source = '=begin\nGENERATED CODE\n=end\ndef real\n if x\n  1\n end\nend\n'
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 4, 2)]
    assert result.nloc == 5


def test_generated_code_comment_still_ends_analysis():
    source = 'def real\n 1\nend\n# GENERATED CODE\ndef skipped\n 2\nend\n'
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [f.name for f in result.function_list] == ['real']


def test_documentation_quote_does_not_hide_following_regex():
    source = "=begin\nWhat's this?\n=end\ndef real\n if /if/ =~ x\n  1\n end\nend\n"
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 4, 2)]
    assert result.nloc == 5


def test_documentation_marker_inside_percent_literal_is_still_text():
    source = 'def real\n  value = %q(\n=begin\n)\n  if /if/ =~ value\n    1\n  end\nend\n'
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('real', 1, 2)]
    assert result.nloc == 8


def test_interpolation_does_not_make_a_data_marker_start_a_line():
    source = 'def real\n  __END__ = 1\n  "#{__END__\n}"\nend\ndef following\n  1\nend\n'
    result = analyze_file.analyze_source_code('sample.rb', source)
    assert [(f.name, f.start_line) for f in result.function_list] == [
        ('real', 1), ('following', 6),
    ]


def _run_forgiveness_check(tmp_path, prefix):
    source = tmp_path / 'warning.rb'
    source.write_text(prefix + 'def real\n if x then 1 end\n if y then 2 end\n if z then 3 end\nend\n')
    result = subprocess.run(
        [sys.executable, lizard.__file__, '-C', '3', '-w', str(source)],
        capture_output=True, text=True, cwd=tmp_path,
    )
    return source, result


def test_documentation_does_not_forgive_a_ruby_warning(tmp_path):
    source, result = _run_forgiveness_check(tmp_path, '=begin\n#lizard forgive\n=end\n')
    assert result.returncode == 1, result.stderr
    assert f'{source}:4: warning: real has' in result.stdout


def test_a_ruby_comment_can_forgive_a_warning(tmp_path):
    _, result = _run_forgiveness_check(tmp_path, '#lizard forgive\n')
    assert result.returncode == 0, result.stderr
    assert 'warning: real has' not in result.stdout
