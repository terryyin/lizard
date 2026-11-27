import unittest

from lizard import analyze_file


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_Ruby_endless_and_special_method_definitions(unittest.TestCase):

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

    def test_star_operator_symbols_are_single_values(self):
        code = '''
def product
  @factors.reduce(:*)
end

def multiply = :*
def exponentiate = :**

def finish
  :done
end
'''

        functions = get_ruby_function_list(code)

        self.assertEqual(
            [('product', 2, 4), ('multiply', 6, 6),
             ('exponentiate', 7, 7), ('finish', 9, 11)],
            [(fn.name, fn.start_line, fn.end_line) for fn in functions])
        self.assertEqual(8, functions[0].token_count)

    def test_colon_character_can_be_multiplied(self):
        code = r'''
def repeat_separator = ?:*
  2

def repeat_escaped_separator = ?\:*
  3
'''

        functions = get_ruby_function_list(code)

        self.assertEqual(
            [('repeat_separator', 2, 3), ('repeat_escaped_separator', 5, 6)],
            [(fn.name, fn.start_line, fn.end_line) for fn in functions])

    def test_load_path_global_can_be_joined_with_star(self):
        code = '''
def load_path = $:*
  File::PATH_SEPARATOR
'''

        functions = get_ruby_function_list(code)

        self.assertEqual(
            [('load_path', 2, 3)],
            [(fn.name, fn.start_line, fn.end_line) for fn in functions])

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
