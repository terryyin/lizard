import unittest

from lizard import analyze_file


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_Ruby_rspec_examples(unittest.TestCase):

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
