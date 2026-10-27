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
