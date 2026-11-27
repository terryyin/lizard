import pickle
import re
import unittest

from lizard_languages.ruby import MyToken, RubyReader


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
