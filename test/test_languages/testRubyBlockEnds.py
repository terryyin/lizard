import unittest

from lizard import analyze_file, FileAnalyzer, get_extensions


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


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
