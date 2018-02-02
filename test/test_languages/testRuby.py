import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import RubyReader


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



class Test_parser_for_Ruby(unittest.TestCase):

    def test_empty(self):
        functions = get_ruby_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_ruby_function_list(''' p "1" ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_ruby_function_list('''
            def f
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].length)

    def test_one_function_loc(self):
        result = get_ruby_function_list('''
            def f
                something
            end
                ''')
        self.assertEqual(3, result[0].length)
        self.assertEqual(3, result[0].nloc)

    def test_two_functions(self):
        result = get_ruby_function_list('''
            def f
            end
            def g
            end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("g", result[1].name)

    def test_one_with_begin_and_end(self):
        result = get_ruby_function_list('''
            def f
                begin
                    something
                end
            end
                ''')
        self.assertEqual(5, result[0].nloc)

    def test_one_with_begin_and_end_outside(self):
        result = get_ruby_function_list('''
        begin
            def f
                begin
                end
            end
        end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_one_with_brackets_and_end_outside(self):
        result = get_ruby_function_list('''
        {
            def f
                begin
                end
            end
        }
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_string(self):
        result = get_ruby_function_list('''
  def path_with_locale(params, to)
    xx
    "#{"a"}"
    xx
  end

  mount JasmineRails::Engine => '/specs' if defined?(JasmineRails)
end
                ''')
        self.assertEqual(5, result[0].nloc)

    def test_one_with_class_in_it(self):
        result = get_ruby_function_list('''
            def f
                class a
                end
                module a
                end
            end
                ''')
        self.assertEqual(6, result[0].nloc)

    def test_one_with_class_as_identifier(self):
        result = get_ruby_function_list('''
            def f
                a.class
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_one_with_do(self):
        result = get_ruby_function_list('''
            def f
                x do
                    something
                end
            end
                ''')
        self.assertEqual(5, result[0].nloc)

    def test_one_within_do(self):
        result = get_ruby_function_list('''
            x do
                def f
                    something
                end
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_one_within_embedded_doc(self):
        result = get_ruby_function_list('''
=begin
    def f
    end
=end
                ''')
        self.assertEqual(0, len(result))

    def test_one_within_embedded_doc_harder(self):
        result = get_ruby_function_list('''
=begin
the everything between a line beginning with =begin and
that with =end will be skipped by the interpreter.
    def f
    end
=end
def f
end
                ''')
        self.assertEqual(1, len(result))


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
