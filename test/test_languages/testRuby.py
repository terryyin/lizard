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


class Test_Ruby_blocks_that_end_a_function_early(unittest.TestCase):
    """Each block keyword pairs with its own `end`, so that `end` must not
    close the function."""

    def test_case_and_until_blocks_do_not_end_the_function(self):
        result = get_ruby_function_list('''
class Shape
  def area(kind, size)
    case kind
    when :square then size * size
    else 0
    end
    size
  end

  def grade(score)
    case
    when score > 90 then "A"
    end
  end

  def unpack(config)
    case config
    in { name: String => name }
      name
    end
    config
  end

  def label(kind)
    text = case kind
           when :a then "A"
           end
    return case kind
           when :b then text
           end
  end

  def grow(size)
    until size > 10
      size += 1
    end
    size += 1 until size > 20
    begin
      size -= 1
    end until size < 15
    size
  end

  def finish
    1
  end
end
''')
        self.assertEqual(
            [('area', 3, 9), ('grade', 11, 15), ('unpack', 17, 23),
             ('label', 25, 32), ('grow', 34, 43), ('finish', 45, 47)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_loop_body_that_starts_with_a_block(self):
        result = get_ruby_function_list('''
def drain(x)
  while x
    if x > 1
      x -= 1
    end
  end
  for y in [x]
    begin
      x += y
    end
  end
  x
end

def last
  1
end
''')
        self.assertEqual(
            [('drain', 2, 14), ('last', 16, 18)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_loop_condition_with_a_block_on_more_lines(self):
        result = get_ruby_function_list('''
def wait(jobs)
  while jobs.any? { |job|
    job.busy?
  }
    tick
  end
  jobs
end

def drain = until jobs.none? { |job|
  job.busy?
}
  tick
end

def last
  1
end
''')
        self.assertEqual(
            [('wait', 2, 9), ('drain', 11, 15), ('last', 17, 19)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_case_and_until_add_the_same_metrics_as_before(self):
        code = '''
def route(kind, n)
  case kind
  when :a then n += 1
  when :b then n += 2
  end
  until n > 10
    n += 1
  end
  n
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        result = analyzer.analyze_source_code('a.rb', code).function_list
        self.assertEqual(
            [('route', 2, 11, 4, 2, 1, 0)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.cognitive_complexity, f.max_nesting_depth,
              f.max_nested_structures) for f in result])

    def test_if_expression_is_a_block_and_a_modifier_is_not(self):
        result = get_ruby_function_list('''
class Pick
  def choose(c)
    x = if c
          1
        else
          2
        end
    y = unless c
          3
        end
    x + y
  end

  def first(c)
    return if c
    return (if c then 1 else 2 end)
  end

  def call(c)
    foo(if c then 1 else 2 end)
    bar(key: if c
               1
             end)
    1
  end

  def drain(queue)
    n = while queue.any?
          queue.pop
        end
    cache ||= begin
      queue.size
    end
    n + cache
  end

  def last
    1
  end
end
''')
        self.assertEqual(
            [('choose', 3, 13), ('first', 15, 18), ('call', 20, 26),
             ('drain', 28, 36), ('last', 38, 40)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_modifier_if_inside_a_one_line_block(self):
        result = get_ruby_function_list('''
def find(items)
  items.each { |x| return x if x }
  nil
end

def names(items)
  items.map { |i| i.name unless i.nil? }
end

def title(item)
  "#{item.name if item}"
end

def last
  1
end
''')
        self.assertEqual(
            [('find', 2, 5), ('names', 7, 9), ('title', 11, 13),
             ('last', 15, 17)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_endless_method_ends_with_its_expression(self):
        result = get_ruby_function_list('''
class Clock
  def usec = time.usec
  def plus(x) = x + 1
  def ==(other) = other.is_a?(Clock) && usec == other.usec
  def self.zero = new(0)
  def sign = if usec > 0
               1
             else
               -1
             end
  def parts =
    [usec, sign]

  def wday
    1
  end
end
''')
        self.assertEqual(
            [('usec', 3, 3, 1, 1), ('plus', 4, 4, 1, 1),
             ('==', 5, 5, 1, 2), ('self.zero', 6, 6, 1, 1),
             ('sign', 7, 11, 5, 2), ('parts', 12, 13, 2, 1),
             ('wday', 15, 17, 3, 1)],
            [(f.name, f.start_line, f.end_line, f.nloc,
              f.cyclomatic_complexity) for f in result])

    def test_endless_method_body_on_more_lines(self):
        result = get_ruby_function_list('''
class Feed
  def drain = while ready?
    tick
  end

  def names = items
    .map { |item| item.name if item }
    .compact

  def wday
    1
  end
end
''')
        self.assertEqual(
            [('drain', 3, 5, 2), ('names', 7, 9, 2), ('wday', 11, 13, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_parenthesized_first_statement_is_no_parameter_list(self):
        result = get_ruby_function_list('''
def check
  (a || b) if ready?
  (a || b).compact
end

def last
  1
end
''')
        self.assertEqual(
            [('check', 2, 5, 0), ('last', 7, 9, 0)],
            [(f.name, f.start_line, f.end_line, f.parameter_count)
             for f in result])

    def test_modifier_if_after_a_percent_literal(self):
        result = get_ruby_function_list('''
def label(ready)
  %q<value> if ready
end

def last
  1
end
''')
        self.assertEqual(
            [('label', 2, 4), ('last', 6, 8)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_modifier_if_after_a_method_named_like_a_keyword(self):
        result = get_ruby_function_list('''
def check(x)
  x.not if ready?
  x.in unless done?
  x
end

def last
  1
end
''')
        self.assertEqual(
            [('check', 2, 6), ('last', 8, 10)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_forgive_comment_after_an_endless_method(self):
        result = get_ruby_function_list('''
def first = 1
#lizard forgive
def second = 2
''')
        self.assertEqual(
            [('first', 2, 2)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_setter_and_operator_methods_keep_their_body(self):
        result = get_ruby_function_list('''
class Box
  def x=(value)
    @x = value
  end

  def ==(other)
    other.x == @x
  end

  def []=(key, value)
    @h[key] = value
  end

  def size
    1
  end
end
''')
        self.assertEqual(
            [(3, 5), (7, 9), (11, 13), (15, 17)],
            [(f.start_line, f.end_line) for f in result])

    def test_generated_methods_in_a_heredoc_keep_the_method(self):
        result = get_ruby_function_list('''
def define_writer(name)
  class_eval <<-RUBY
    def #{name}=(value)
      @value = value
    end
  RUBY
end

def define_reader(name)
  class_eval <<-RUBY
    def self.#{name}
      value = @value
      value
    end
  RUBY
end

def last
  1
end
''')
        self.assertEqual(
            [('define_writer', 2, 8), ('define_reader', 10, 17),
             ('last', 19, 21)],
            [(f.name, f.start_line, f.end_line) for f in result
             if f.name in ('define_writer', 'define_reader', 'last')])

    def test_rspec_example_with_a_description_on_two_lines(self):
        result = get_ruby_function_list('''
describe Clock do
  it "ticks once
      a second" do
    1
  end
end

def last
  1
end
''')
        self.assertEqual(
            [(4, 6), (9, 11)],
            [(f.start_line, f.end_line) for f in result])

    def test_rspec_example_with_a_percent_string(self):
        result = get_ruby_function_list('''
describe Clock do
  it %q(ticks) do
    1
  end
end
''')
        self.assertEqual(
            [('%q(ticks)', 3, 5)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_it_block_parameter_is_no_rspec_example(self):
        result = get_ruby_function_list('''
def doubled(items)
  items.map { it * 2 }
end

def save_all(users)
  users.each do
    it.save
  end
  users.map { it.name }
  users.each { it(1) }
end

describe Clock do
  it "doubles" do
    expect([1].map { it * 2 }).to eq([2])
  end
  context("now") { it "ticks #{1}" do
    1
  end }
end

def last
  1
end
''')
        self.assertEqual(
            [('doubled', 2, 4), ('save_all', 6, 12), ('"doubles"', 15, 17),
             ('""', 18, 20), ('last', 23, 25)],
            [(f.name, f.start_line, f.end_line) for f in result])

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
