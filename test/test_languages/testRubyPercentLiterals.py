import unittest

from lizard import analyze_file
from lizard_languages.ruby import RubyReader


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_Ruby_percent_literal(unittest.TestCase):
    """A % literal ends at its own closing delimiter. A nested pair of the
    same delimiter or an interpolation inside it does not end it."""

    def check_tokens(self, expect, source):
        self.assertEqual(expect, list(RubyReader.generate_tokens(source)))

    def check_ccn(self, expect, source):
        self.assertEqual(
            expect,
            [(f.name, f.cyclomatic_complexity)
             for f in get_ruby_function_list(source)])

    def test_interpolation_in_a_literal_does_not_end_the_function(self):
        result = get_ruby_function_list(r'''
class Store
  def product(s, a)
    x = s.match?(%r{/dp/#{a}\z}i)
    x
  end

  def labels(a)
    %W(#{a}-x #{a}-y).each do |l|
      puts l
    end
  end

  def finish
    1
  end
end
''')
        self.assertEqual(
            [('product', 3, 6), ('labels', 8, 12), ('finish', 14, 16)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_a_percent_sign_in_an_unread_regex_does_not_run_on(self):
        result = get_ruby_function_list('''
def kind(s)
  if /^%w/ =~ s
    1
  end
end

def half(a)
  a.sum / 2
end
''')
        self.assertEqual(
            [('kind', 2, 6), ('half', 8, 10)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_a_regex_with_another_delimiter_can_span_lines(self):
        result = get_ruby_function_list(r'''
def details(sql)
  match = %r/
    TABLE\s+(\w+)?\s+
    (end)?
  /xi.match(sql)
  match
end
''')
        self.assertEqual(
            [('details', 2, 8, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_modulo_before_a_decision_counts_the_decision(self):
        for modulo in ('a%=2', 'a %= 2', 'a = a % 2', 'a%i==0'):
            self.check_ccn(
                [('f', 2)],
                'def f(a, i); %s; if a==0 then a else 0 end; end' % modulo)

    def test_any_punctuation_delimits_a_literal(self):
        for delimiter in ('@', '#', '=', '^', '-'):
            self.check_ccn(
                [('f', 2), ('g', 1)],
                'def f\n  value = %%q%s if fake end %s\n'
                '  if real then 1 end\nend\ndef g\n  1\nend\n'
                % (delimiter, delimiter))

    def test_a_nested_pair_keeps_a_literal_opaque(self):
        self.check_ccn(
            [('f', 2), ('g', 1)],
            'def f\n  value = %q{ if {fake} end }\n'
            '  if real then 1 end\nend\ndef g\n  1\nend\n')

    def test_nested_pairs_and_interpolation_stay_in_the_literal(self):
        self.check_tokens([r'%r{/dp/#{a}\z}i'], r'%r{/dp/#{a}\z}i')
        self.check_tokens(['%r{#{h.fetch(:a) { |k| k }}}x'],
                          '%r{#{h.fetch(:a) { |k| k }}}x')
        self.check_tokens(['%w(a (b) c)'], '%w(a (b) c)')
        self.check_tokens(['%i[a [b] c]'], '%i[a [b] c]')
        self.check_tokens(['%(<b>#{f(x)}</b>)'], '%(<b>#{f(x)}</b>)')
        self.check_tokens(['%Q<a <b> #{c}>'], '%Q<a <b> #{c}>')

    def test_every_percent_form(self):
        self.check_tokens(['%W[#{a} b]'], '%W[#{a} b]')
        self.check_tokens(['%I[a b]'], '%I[a b]')
        self.check_tokens(['%x{ls #{dir}}'], '%x{ls #{dir}}')
        self.check_tokens(['%s(a)'], '%s(a)')

    def test_delimiters_that_are_not_pairs(self):
        self.check_tokens(['%r!^/end!i'], '%r!^/end!i')
        self.check_tokens(['%w|a b|'], '%w|a b|')
        self.check_tokens(['%q/a b/'], '%q/a b/')
        self.check_tokens(['%Q"a #{b} \\" c"'], '%Q"a #{b} \\" c"')
        self.check_tokens(['%|a "b|'], '%|a "b|')

    def test_modulo_stays_an_operator(self):
        self.check_tokens(['a', ' ', '%', ' ', 'b'], 'a % b')
        self.check_tokens(['x', ' ', '%', 'w'], 'x %w')
        self.check_tokens(['"%d"', ' ', '%', ' ', 'n'], '"%d" % n')
        self.check_tokens(['x', ' ', '%', '=', ' ', '2'], 'x %= 2')
