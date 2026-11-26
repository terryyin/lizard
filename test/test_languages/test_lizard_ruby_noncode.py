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


def test_standalone_heredoc_body_counts_as_literal_text():
    source = '''
def notice
  <<~MESSAGE
    Use the new settings if you changed teams.
    Keep your presets until your trial ends.
  MESSAGE
end

def template
  body = <<~'TEMPLATE'
    Account: #{account.name if account}
  TEMPLATE
  body
end

def shift_pages(pages, mask)
  limit = pages <<~mask
  mask
  limit
end
'''
    result = analyze_file.analyze_source_code('notices.rb', source)

    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in result.function_list] == [
        ('notice', 2, 7, 1), ('template', 9, 14, 1), ('shift_pages', 16, 20, 1),
    ]


def test_continued_shift_does_not_consume_the_following_branch():
    source = r'''
def shift_pages(pages, mask, display)
  limit = pages \
    <<~mask
  acknowledge(limit) if display
  mask
  limit
end
'''

    function, = analyze_file.analyze_source_code('pages.rb', source).function_list

    assert function.cyclomatic_complexity == 2


def test_heredoc_with_simple_references_keeps_following_method():
    source = '''
def overview(label)
  help = <<~HELP
    You'll see your version in the settings.
  HELP
  info = <<~INFO
    Here's the #{label}: #{Bundle::VERSION}
  INFO
  [help, info]
end

def verified
  'ready'
end
'''
    result = analyze_file.analyze_source_code('bundle.rb', source)

    assert [(f.name, f.start_line, f.end_line) for f in result.function_list] == [
        ('overview', 2, 10), ('verified', 12, 14),
    ]


def test_heredoc_before_a_call_closing_parenthesis_preserves_later_code():
    source = '''
def prepare(checker)
  checker.verify(<<~RUBY)
    if requested
      authorize
    end
  RUBY
  use_fallback if enabled?
end
'''

    result = analyze_file.analyze_source_code('verify.rb', source)

    assert [(f.name, f.start_line, f.end_line, f.nloc, f.cyclomatic_complexity)
            for f in result.function_list] == [('prepare', 2, 9, 8, 2)]


def test_heredoc_keeps_the_named_arguments_after_the_opener():
    source = '''
def prepare(checker, keyword)
  checker.verify(<<~RUBY, keyword: keyword)
    Use the new settings if the trial ends.
  RUBY
  use_fallback if enabled?
end
'''

    function, = analyze_file.analyze_source_code('verify.rb', source).function_list

    assert (function.end_line, function.cyclomatic_complexity) == (7, 2)


@pytest.mark.parametrize('call', [
    'checker.verify(<<~RUBY.squish)',
    'checker.verify "rules.rb", <<~RUBY',
    'checker.verify(<<~"RUBY")',
    'checker.verify(<<~RUBY) # generated help',
])
def test_heredoc_keeps_text_in_command_arguments_and_receiver_calls(call):
    source = f'''def prepare(checker)
  {call}
    verify if configured
  RUBY
  use_fallback if enabled?
end
'''

    function, = analyze_file.analyze_source_code('verify.rb', source).function_list

    assert (function.end_line, function.cyclomatic_complexity) == (6, 2)


def test_heredoc_counts_expressions_and_skips_surrounding_text():
    source = '''def status(enabled)
  format(<<~BODY)
    try again if the service is busy
    result: #{enabled ? "on" : "off"}
    end
  BODY
end

def ready
  true
end
'''

    result = analyze_file.analyze_source_code('status.rb', source)

    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('status', 1, 7, 2), ('ready', 9, 11, 1)]


def test_bare_heredoc_command_and_shift_with_a_local_receiver():
    source = '''def install
  run <<-RUBY
    use_defaults if configured?
  RUBY
  restore if failed?
end

def by_parameter(pages)
  pages <<~MASK
  reload if changed?
  MASK
end

def by_assignment
  pages = 16
  pages <<-MASK
  reload if changed?
  MASK
end
'''

    result = analyze_file.analyze_source_code('install.rb', source)

    assert [(f.name, f.end_line, f.cyclomatic_complexity) for f in result.function_list] == [
        ('install', 6, 2), ('by_parameter', 12, 2), ('by_assignment', 19, 2),
    ]


def test_heredoc_local_receivers_follow_method_and_pattern_scope():
    source = '''def output_from(puts)
  puts
end

def generated_help
  puts <<~TEXT
    Report if the order shipped.
  TEXT
end

def report(entry)
  case entry
  in {pages:}
    pages <<~MASK
    print pages if pages.any?
    MASK
  end
end
'''

    analyzer = lizard.FileAnalyzer(lizard.get_extensions(['nd']))
    result = analyzer.analyze_source_code('report.rb', source)

    assert [(f.name, f.cyclomatic_complexity) for f in result.function_list] == [
        ('output_from', 1), ('generated_help', 1), ('report', 2),
    ]
    assert result.function_list[-1].max_nesting_depth == 2


@pytest.mark.parametrize('pattern', [
    '{ body: copy }', '{ body:\n      copy }', 'body: copy',
    '{\n    copy: ^(body())\n  }',
])
def test_heredoc_receiver_can_share_a_nonbinding_pattern_name(pattern):
    source = '''def email_body(payload)
  case payload
  in PATTERN
    render(copy)
  else
    return
  end
  body <<~TEXT
    return copy if ready
  TEXT
end
'''.replace('PATTERN', pattern)

    function, = analyze_file.analyze_source_code('email.rb', source).function_list

    assert function.cyclomatic_complexity == 1


def test_pinned_expression_keeps_the_following_pattern_binding():
    source = '''def save_pages(payload, enabled, expected)
  case payload
  in { status: ^(expected if enabled), pages: }
    pages <<~MASK
    save(pages) if enabled
    MASK
  end
end
'''

    analyzer = lizard.FileAnalyzer(lizard.get_extensions(['nd']))
    function, = analyzer.analyze_source_code('pages.rb', source).function_list

    assert (function.cyclomatic_complexity, function.max_nesting_depth) == (3, 2)


@pytest.mark.parametrize('pin', [
    '^(request in pages:)',
    '^((request in { pages: }))',
    '^((request in { matched: ^(expected), pages: }))',
])
def test_nested_match_in_a_pin_can_bind_a_shift_receiver(pin):
    source = '''def save_pages(response, request, expected)
  case response
  in { cached: PIN }
    pages <<~MASK
    save(pages) if pages
    MASK
  end
end
'''.replace('PIN', pin)

    analyzer = lizard.FileAnalyzer(lizard.get_extensions(['nd']))
    function, = analyzer.analyze_source_code('pages.rb', source).function_list

    assert (function.cyclomatic_complexity, function.max_nesting_depth) == (2, 2)


def test_alternative_pattern_keeps_the_following_local_binding():
    source = '''def save_pages(response, expected_status)
  case response
  in { status: ^(expected_status.to_i), format: :html | :text, pages: }
    pages <<~MASK
    save(pages) if writable?
    MASK
  end
end
'''

    function, = analyze_file.analyze_source_code('pages.rb', source).function_list

    assert function.cyclomatic_complexity == 2


@pytest.mark.parametrize('subject', ['response', '@response'])
def test_rightward_match_binds_locals_but_a_hash_pair_does_not(subject):
    source = '''=begin
Save the pages returned by the request.
=end
def save_pages(response, expected_status)
  prepare(response)
  SUBJECT => { status: ^(expected_status.to_i), pages: }
  pages <<~MASK
  save(pages) if writable?
  MASK
end

def archive(response)
  handlers = {
    response => { body: }
  }
  body <<~TEXT
    Skip if already archived.
  TEXT
end
'''.replace('SUBJECT', subject)

    result = analyze_file.analyze_source_code('pages.rb', source)

    assert [(f.name, f.cyclomatic_complexity) for f in result.function_list] == [
        ('save_pages', 2), ('archive', 1),
    ]


@pytest.mark.parametrize('call', [
    'configure :report,\n    format => default_format',
    'configure :report, # Explicit format\n    format => default_format',
    'configure(\n    format => default_format\n  )',
    'configure \\' '\n    format => default_format',
])
def test_continued_hash_argument_does_not_bind_a_heredoc_receiver(call):
    source = '''def render_report(format)
  CALL
  default_format <<~TEXT
    skip if disabled?
  TEXT
end
'''.replace('CALL', call)

    function, = analyze_file.analyze_source_code('report.rb', source).function_list

    assert function.cyclomatic_complexity == 1


@pytest.mark.parametrize('last_argument', ['<<~\'FOOTER\'', 'footer: <<~\'FOOTER\''])
def test_multiple_heredoc_bodies_keep_the_call_guard_and_interpolation(last_argument):
    source = '''def status(enabled)
  format(<<~HEADER, <<~BODY, ARGUMENT) if enabled
    Include the report if available.
  HEADER
    Status: #{enabled ? "on" : "off"}
    end
  BODY
    #{warn("offline") if enabled}
  FOOTER
end

def ready
  true
end
'''.replace('ARGUMENT', last_argument)

    result = analyze_file.analyze_source_code('status.rb', source)

    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in result.function_list] == [('status', 1, 10, 3), ('ready', 12, 14, 1)]


def test_bare_command_can_use_several_heredocs():
    source = '''def command
  format <<~FIRST, <<~SECOND
    Cancel if the order shipped.
  FIRST
    Report if requested.
  SECOND
end
'''

    function, = analyze_file.analyze_source_code('command.rb', source).function_list

    assert (function.end_line, function.cyclomatic_complexity, function.token_count) == (7, 1, 6)


def test_bare_queue_keeps_single_quoted_body_interpolation_literal():
    source = '''def generated_help(show_details)
  format <<~'TEMPLATE', <<~DETAILS
    Show #{show_details ? 'account' : 'summary'}.
  TEMPLATE
    Account details.
  DETAILS
end
'''

    analyzer = lizard.FileAnalyzer(lizard.get_extensions(['nd']))
    function, = analyzer.analyze_source_code('help.rb', source).function_list

    assert (function.end_line, function.cyclomatic_complexity, function.max_nesting_depth) == (7, 1, 0)


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


def test_percent_string_counts_only_executable_interpolation():
    source = r'''
def lines(enabled, framework)
  %(#{"# " if enabled}require "#{framework}" if configured)
end

def literal(enabled, framework)
  %q(#{"# " if enabled}require "#{framework}" if configured)
end

def escaped
  %Q(\#{legacy if active} report #{name if available})
end
'''

    result = analyze_file.analyze_source_code('config.rb', source)

    assert [(f.name, f.cyclomatic_complexity)
            for f in result.function_list] == [('lines', 2), ('literal', 1), ('escaped', 2)]


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
