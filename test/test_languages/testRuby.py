import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages.ruby import RubyReader, MyToken
import pickle
import re


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list

class Test_tokenizing_Ruby(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(RubyReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_tokenizing_ruby_regular_expression(self):
        self.check_tokens(['/ab/'], '/ab/')
        self.check_tokens([r'/\//'], r'/\//')
        self.check_tokens([r'/a/igm'], r'/a/igm')

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_tokenizing_ruby_regular_expression(self):
        self.check_tokens(['a', '=', '/ab/'], 'a=/ab/')

    def test_tokenizing_ruby_comments(self):
        self.check_tokens(['/**a/*/'], '''/**a/*/''')

    def test_tokenizing_pattern(self):
        self.check_tokens(['/\//'], r'''/\//''')

    def test_tokenizing_brackets(self):
        self.check_tokens(['{', '}'], r'''{}''')

    def test_tokenizing_string_with_formatter(self):
        self.check_tokens(['""', '${', '1', '}', '"a"' ], r'''"#{1}a"''')

    def test_tokenizing_string_with_string(self):
        self.check_tokens(['""', '${', '"a"', '}', '""' ], r'''"#{"a"}"''')

    def test_tokenizing_string_with_string2(self):
        self.check_tokens(['""', '${', '"/"', '${', '}', '""', '}', '""'], r'''"#{"/#{}"}"''')

    def test_tokenizing_symbol(self):
        self.check_tokens([':class'], r''':class''')
        self.check_tokens([':class?'], r''':class?''')
        self.check_tokens([':@class'], r''':@class''')

    def test_shorthand_symbol(self):
        self.check_tokens(['class:', 'a'], r'''class:a''')

    def test_percent_i_symbol_array(self):
        """Ruby %i[] and %I[] - symbol array literals (issue #457)"""
        self.check_tokens(
            ['only:', ' ', '%i[add check_duplicate update_type_fields]'],
            'only: %i[add check_duplicate update_type_fields]')
        self.check_tokens(['%I[a b c]'], '%I[a b c]')

    def test_tokenizing_string_expression(self):
        self.check_tokens(['%{"}'], r'''%{"}''')
        self.check_tokens(['%{""}'], r'''%{""}''')
        self.check_tokens(['%{\}}'], r'''%{\}}''')
        self.check_tokens(['%{\}}'], r'''%{\}}''')
        self.check_tokens(['%q{\}}'], r'''%q{\}}''')
        self.check_tokens(['%q[\]]'], r'''%q[\]]''')
        self.check_tokens(['%q<\>>'], r'''%q<\>>''')

    def test_vars(self):
        self.check_tokens(['$a'], r'''$a''')
        self.check_tokens(['@a'], r'''@a''')
        self.check_tokens(['@@a'], r'''@@a''')

    def test_ranges(self):
        self.check_tokens(['..'], r'''..''')
        self.check_tokens(['...'], r'''...''')

    def test_special_method_names(self):
        self.check_tokens(['a!'], r'''a!''')
        self.check_tokens(['a?'], r'''a?''')

    def test_MyToken_serialization(self):
        # Test with regex match object
        match = re.match(r'\w+', 'test123')
        token = MyToken(match)
        pickled = pickle.dumps(token)
        unpickled = pickle.loads(pickled)
        self.assertEqual(str(token), str(unpickled))
        self.assertEqual(token.begin, unpickled.begin)
        
        # Test with plain string
        token_str = MyToken('test123')
        pickled_str = pickle.dumps(token_str)
        unpickled_str = pickle.loads(pickled_str)
        self.assertEqual(str(token_str), str(unpickled_str))
        self.assertEqual(token_str.begin, unpickled_str.begin)


class Test_parser_for_Ruby_ccn(unittest.TestCase):

    def test_basic_complexity(self):
        result = get_ruby_function_list('''
            def f
                if a
                elsif b
                end
            end
                ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)


class Test_parser_for_Ruby_def(unittest.TestCase):
    def test_class_method(self):
        result = get_ruby_function_list('''
            def a.b
            end
                ''')
        self.assertEqual("a.b", result[0].name)
        self.assertEqual(0, result[0].parameter_count)

    def test_empty_parameters(self):
        result = get_ruby_function_list('''
            def a()
            end
                ''')
        self.assertEqual(0, result[0].parameter_count)

    def test_more_parameters(self):
        result = get_ruby_function_list('''
            def a(b,c)
            end
                ''')
        self.assertEqual(2, result[0].parameter_count)

    def test_rails_controller_with_percent_i_and_safe_navigation(self):
        """Issue #457: Parser must not hang on %i[] and &. constructs"""
        code = '''
before_action :adapt_widget_params, only: %i[add check_duplicate update_type_fields]
def adapt_widget_params
  @account_group_id = params[:presenter]&.delete(:account_group_id)
  @widget_params = params[:presenter] || {}
end
def adapt_search_params
  @account_group_id = params[:search]&.delete(:account_group_id)
end
'''
        result = get_ruby_function_list(code)
        self.assertEqual(2, len(result))
        self.assertEqual("adapt_widget_params", result[0].name)
        self.assertEqual("adapt_search_params", result[1].name)


class Test_Ruby_comment_line_continuation(unittest.TestCase):
    """A '#' comment ending in a backslash must not continue onto the next
    line and swallow it (issue #317, the shared-tokenizer half)."""

    def test_backslash_at_end_of_comment_keeps_next_line(self):
        code = (
            "def foo(x)\n"
            "  # a trailing comment\\\n"
            "  if x > 10\n"
            "    return 1\n"
            "  end\n"
            "  return 0\n"
            "end\n"
        )
        result = get_ruby_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)


class Test_Ruby_keyword_as_a_method_name(unittest.TestCase):
    """After '.', '&.' or 'def' a keyword names a method: `m.end(0)` is a
    call, and `def self.for(user)` defines a method named `for`."""

    def test_end_after_a_dot_does_not_end_the_function(self):
        result = get_ruby_function_list('''
class Span
  def size
    self.end - self.begin
  end

  def length(m)
    a = m.end(0)
    b = m.begin(0)
    a - b
  end

  def last(range)
    range&.end
  end

  def offsets(match)
    match
      .end(0)
  end

  def first(list)
    list = [] if list.then { |l| l.nil? }
    list.first
  end

  def finish
    1
  end
end
''')
        self.assertEqual(
            [('size', 3, 5), ('length', 7, 11), ('last', 13, 15),
             ('offsets', 17, 20), ('first', 22, 25), ('finish', 27, 29)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_keyword_named_methods_add_no_complexity(self):
        code = '''
class Policy
  def self.for(user)
    new(user)
  end

  def if(record)
    record
  end

  def check(p, user)
    p.catch { |e| e }
    x = p.if
    p.for
    user.try(:name)&.when
    if x
      p.unless
    end
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        result = analyzer.analyze_source_code('a.rb', code).function_list
        self.assertEqual(
            [('self.for', 1, 0, 0, 0), ('if', 1, 0, 0, 0),
             ('check', 2, 1, 1, 1)],
            [(f.name, f.cyclomatic_complexity, f.cognitive_complexity,
              f.max_nesting_depth, f.max_nested_structures)
             for f in result])

    def test_structure_after_a_keyword_named_call_still_counts(self):
        code = '''
def pick(rule, x)
  rule.else
  if x
    return rule.return
  end
  rule.else
  x
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'exitcount']))
        result = analyzer.analyze_source_code('a.rb', code).function_list
        self.assertEqual([(1, 1)], [(f.cognitive_complexity, f.exit_count)
                                    for f in result])

    def test_do_after_a_dot_does_not_open_the_loop_body(self):
        result = get_ruby_function_list('''
def drain(m)
  while m.do do
    m.next
  end
  m
end
''')
        self.assertEqual(
            [('drain', 2, 7, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_yield_after_a_dot_keeps_one_logical_sequence(self):
        code = '''
def resume(a, b)
  a && Fiber.yield && b
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive']))
        result = analyzer.analyze_source_code('a.rb', code).function_list
        self.assertEqual([1], [f.cognitive_complexity for f in result])


class Test_Ruby_percent_literal(unittest.TestCase):
    """A % literal ends at its own closing delimiter. A nested pair of the
    same delimiter or an interpolation inside it does not end it."""

    def check_tokens(self, expect, source):
        self.assertEqual(expect, list(RubyReader.generate_tokens(source)))

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
