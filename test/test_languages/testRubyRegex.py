import unittest
from lizard import analyze_file
from lizard_languages.ruby import RubyReader


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_Ruby_regex_literal_context(unittest.TestCase):

    def test_heredoc_apostrophe_does_not_hide_later_regexp(self):
        source = '''def fallback(enabled)
  notice = <<~TEXT
    Here's the fallback notice.
  TEXT
  return // if enabled
  notice
end
'''

        function, = get_ruby_function_list(source)

        self.assertEqual((7, 2), (function.end_line, function.cyclomatic_complexity))
        self.assertIn('//', list(RubyReader.generate_tokens(source)))

    def test_empty_regex_keeps_later_conditions(self):
        source = '''
def fallback(queue, enabled)
  queue << // if enabled
  return //x if queue.empty?
end

def match_rules(text, enabled)
  empty = //
  text =~ // && enabled
end
'''

        functions = get_ruby_function_list(source)

        self.assertEqual([('fallback', 3), ('match_rules', 2)],
                         [(f.name, f.cyclomatic_complexity) for f in functions])
        regexps = [t for t in RubyReader.generate_tokens(source) if t in ('//', '//x')]
        self.assertEqual(['//', '//x', '//', '//'], regexps)

    def test_division_can_touch_a_nonempty_regexp(self):
        source = 'def ratio(value, units)\n value//unit/.match?(units) if units\nend\n'

        function, = get_ruby_function_list(source)

        self.assertEqual(2, function.cyclomatic_complexity)

    def test_statement_start_regex_and_continued_division_keep_separate_tokens(self):
        source = r'''
def cleanup_rule
  fallback = 'tmp'
  /\A#{fallback}(?:-end)?\z/
end

def empty_rule(enabled)
  notify
  // if enabled
end

def ratio(value, factor, scale)
  value \
    / factor / scale
end
'''

        tokens = [t for t in RubyReader.generate_tokens(source) if t.startswith('/')]
        functions = get_ruby_function_list(source)

        self.assertEqual([r'/\A#{fallback}(?:-end)?\z/', '//', '/', '/'], tokens)
        self.assertEqual([1, 2, 1], [f.cyclomatic_complexity for f in functions])

    def test_extended_multiline_regex_keeps_keywords_in_pattern_and_comments(self):
        regexp = r'''/\A
    (?<name>[a-z]+) # captured field
    (?:end|next)?
  \z/x'''
        source = 'def header_rule\n  ' + regexp + '\nend\n'

        function, = get_ruby_function_list(source)

        self.assertEqual((6, 1), (function.end_line, function.cyclomatic_complexity))
        self.assertIn(regexp, list(RubyReader.generate_tokens(source)))

    def test_regexp_with_nested_literal_keeps_method_boundary(self):
        source = r'''def key_pattern(key, namespace)
  /.+\/#{Regexp.escape namespace.to_s}#{/%3A/i if namespace}#{Regexp.escape key}/
end
'''

        function, = get_ruby_function_list(source)

        self.assertEqual((1, 3), (function.start_line, function.end_line))

    def test_statement_start_regexp_counts_executable_interpolation(self):
        regexp = '/#{prefix if enabled}(?:-end)?/'
        source = "def substitution(enabled)\n  prefix = 'tmp'\n  " + regexp + '\nend\n'

        function, = get_ruby_function_list(source)

        self.assertEqual((4, 2), (function.end_line, function.cyclomatic_complexity))
        self.assertIn(regexp, list(RubyReader.generate_tokens(source)))

    def test_return_regex_exposes_ternary_condition(self):
        functions = get_ruby_function_list(
            'def a(s)\n return /#/ =~ s ? 1 : 0\nend\n')
        self.assertEqual([('a', 2)], [
            (f.name, f.cyclomatic_complexity) for f in functions])

    def test_plain_regex_after_an_append(self):
        source = r'''
def append_defaults(rules, enabled)
  rules << %r{#} << /\A(?:end|quit)?\z/ if enabled
end

def append_guard(rules, input, enabled)
  rules << /#/ if /#/ =~ input && enabled
end

def append_prefix(rules, prefix)
  rules << /#{prefix.delete_suffix('/')}/
end

def normalized_shift(enabled)
  self.<< / (enabled ? 2 : 1) / 3
end
'''

        functions = get_ruby_function_list(source)

        self.assertEqual(
            [('append_defaults', 4, 2), ('append_guard', 8, 3),
             ('append_prefix', 12, 1), ('normalized_shift', 16, 2)],
            [(function.name, function.end_line, function.cyclomatic_complexity)
             for function in functions])

    def test_backtick_operator_does_not_hide_later_regexps(self):
        definitions = (
            'def capture(shell, command)\n  shell.`(command)\nend\n',
            'def `(command)\n  capture(command)\nend\n',
            'module ShellOperators\n  alias_method :execute, :`\nend\n',
            'expect(shell).to receive(:`).with(command)\n',
        )
        usage = r'''
def ignore_rules(rules, count)
  rules << /\Awhen\z/
  return /#/ if count > 0
end

def dispatch(command)
  command if /#/ =~ command
end
'''
        for definition in definitions:
            with self.subTest(definition=definition):
                functions = get_ruby_function_list(definition + usage)

                self.assertEqual(
                    [('ignore_rules', 2), ('dispatch', 2)],
                    [(function.name, function.cyclomatic_complexity)
                     for function in functions[-2:]])
                self.assertEqual(11, functions[-1].token_count)

    def test_member_division_keeps_condition_and_tokens(self):
        functions = get_ruby_function_list(
            'def divide(obj,a,b,c)\n obj.return / (a || b) / c\nend\n')
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(22, functions[0].token_count)

    def test_split_and_gsub_regex_controls_keep_conditions(self):
        split_fn = get_ruby_function_list(
            'def a(s)\n s.split(/"/).include?(s) || s.nil?\nend\n')[0]
        gsub_fn = get_ruby_function_list(
            'def a(s)\n s.gsub(/#/, "") || s\nend\n')[0]
        self.assertEqual(2, split_fn.cyclomatic_complexity)
        self.assertEqual(2, gsub_fn.cyclomatic_complexity)


class Test_Ruby_regex_after_a_keyword(unittest.TestCase):
    """After a keyword such as `if` or `when`, a `/` starts a regex."""

    def tokens(self, code):
        return [t for t in RubyReader.generate_tokens(code) if not t.isspace()]

    def test_end_in_a_regex_after_if_does_not_end_the_block(self):
        functions = get_ruby_function_list(r'''
def strip_end(g)
  if /^\s*end\s*$/.match?(g)
    g
  end
  1
end

def last
  1
end
''')
        self.assertEqual(
            [('strip_end', 2, 7), ('last', 9, 11)],
            [(f.name, f.start_line, f.end_line) for f in functions])

    def test_question_marks_in_a_regex_after_a_keyword_add_no_complexity(self):
        functions = get_ruby_function_list(r'''
def check(a, b)
  return 1 if /^(ab)?$/ =~ a
  return 2 unless /[cd]?/.match?(b)
  while /(x)?y/ =~ a
    a = a[1..]
  end
  a and /(z)?/.match?(b)
end
''')
        self.assertEqual([4], [f.cyclomatic_complexity for f in functions])

    def test_regex_after_each_keyword_is_one_token(self):
        for keyword in ('if', 'unless', 'elsif', 'when', 'while', 'until',
                        'and', 'or', 'not', 'then', 'return', 'next',
                        'break'):
            self.assertEqual([keyword, '/a b/i', '.', 'match?', 's'],
                             self.tokens(keyword + ' /a b/i.match? s'))

    def test_division_stays_division(self):
        self.assertEqual(['a', '/', 'b', '/', 'c'], self.tokens('a / b / c'))
        self.assertEqual(['x', '/', '=', '2'], self.tokens('x /= 2'))
        self.assertEqual(['def', '/', '(', 'o', ')'], self.tokens('def /(o)'))
        self.assertEqual(['1', '/', '2', 'if', 'c'], self.tokens('1 / 2 if c'))
        self.assertEqual(['@when', '/', 'a', '/', 'b'],
                         self.tokens('@when / a / b'))
        self.assertEqual(['@@if', '/', 'a', '/', 'b'],
                         self.tokens('@@if / a / b'))
