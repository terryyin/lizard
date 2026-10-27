import unittest
from lizard import analyze_file


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_parser_for_Ruby_if_while_for(unittest.TestCase):
    def test_basic_if_block(self):
        result = get_ruby_function_list('''
            def f
                if a
                end
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_basic_if_oneliner_block(self):
        result = get_ruby_function_list('''
            def f; if a; end; end
                ''')
        self.assertEqual(1, result[0].nloc)

    def test_basic_if_modifier(self):
        result = get_ruby_function_list('''
            def f
                a if b
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_basic_if_with_then_on_one_line(self):
        result = get_ruby_function_list('''
            def f
                a = if b then c else d end
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_unless(self):
        result = get_ruby_function_list('''
            def f
                a = unless b then c else d end
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_basic_while_block(self):
        result = get_ruby_function_list('''
            def f
                while a
                end
                for a
                end
            end
                ''')
        self.assertEqual(6, result[0].nloc)

    def test_basic_while_modifier(self):
        result = get_ruby_function_list('''
            def f
                a while a
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_while_with_do(self):
        result = get_ruby_function_list('''
            def f
                while a do
                end
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_while_modifier_with_block(self):
        result = get_ruby_function_list('''
            def f
                begin
                end while a
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_class_as_an_symbol(self):
        result = get_ruby_function_list('''
            def f
                begin
                end while a
            end
                ''')
        self.assertEqual(4, result[0].nloc)


    def test_rspec_it(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it "does something" do
              end
              it "does something else" do
              end
              context "xx" do
                it "xxxx" do
                    xxx
                end
              end
            end
                ''')
        self.assertEqual(3, result[2].nloc)

    def test_rspec_it_with_brackets(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it { }
              it "does something else" do
                a if b
              end
              context "xx" do
                it "xxxx" do
                    xxx
                end
              end
            end
                ''')
        self.assertEqual(3, len(result))
        self.assertEqual(1, result[0].nloc)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_it_as_variable(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it = 3
            end
                ''')
        self.assertEqual(0, len(result))
