import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from languages import RubyReader


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

    def test_tokenizing_string_expression(self):
        self.check_tokens(['%{"}'], r'''%{"}''')



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
        self.assertEqual(1, result[0].length)

    def test_one_function_loc(self):
        result = get_ruby_function_list('''
            def f
                something
            end
                ''')
        self.assertEqual(2, result[0].length)
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

