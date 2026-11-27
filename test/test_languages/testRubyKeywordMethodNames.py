import unittest

from lizard import analyze_file, FileAnalyzer, get_extensions


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


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
