import unittest
from lizard import CodeReader
generate_tokens = CodeReader.generate_tokens


class Test_generate_tonken(unittest.TestCase):

    def check_tokens(self, source, *expect):
        tokens = generate_tokens(source)
        self.assertEqual(list(expect), tokens)

    def test_empty_string(self):
        self.check_tokens("")

    def test_spaces(self):
        self.check_tokens("\n", "\n")
        self.check_tokens("\n\n", "\n", "\n")
        self.check_tokens(" \n", " ", "\n")

    def test_digits(self):
        self.check_tokens("1", "1")
        self.check_tokens("123", "123")

    def test_operators(self):
        self.check_tokens("-;", '-', ';')
        self.check_tokens("-=", '-=')
        self.check_tokens(">=", '>=')
        self.check_tokens("<=", '<=')
        self.check_tokens("||", '||')

    def test_more(self):
        self.check_tokens("int a{}", 'int', ' ', "a", "{", "}")

    def test_string(self):
        self.check_tokens(r'""', '""')
        self.check_tokens(r'"x\"xx")', '"x\\"xx"', ')')
        self.check_tokens("'\\''", "'\\''")
        self.check_tokens(r"'\\\'", *["'", '\\', '\\', '\\', "'"])

    def test_line_number(self):
        self.check_tokens(r'abc', 'abc')

    def test_line_number2(self):
        tokens = generate_tokens('abc\ndef')
        self.assertTrue('def' in tokens)

    def test_with_mutiple_line_string(self):
        tokens = generate_tokens('"sss\nsss" t')
        self.assertTrue('t' in tokens)


class Test_generate_tonken_for_marcos(unittest.TestCase):

    def test_define(self):
        define =  '''#define xx()\
                       abc'''
        tokens = generate_tokens(define+'''
                    int''')
        self.assertEqual([define, '\n', ' ' * 20, 'int'], tokens)

    def test_include(self):
        tokens = generate_tokens('''#include "abc"''')
        self.assertEqual(['#include "abc"'], tokens)

    def test_if(self):
        tokens = generate_tokens('''#if abc\n''')
        self.assertEqual(['#if abc', '\n'], tokens)

    def test_ifdef(self):
        tokens = generate_tokens('''#ifdef abc\n''')
        self.assertEqual(['#ifdef abc', '\n'], tokens)

    def test_with_line_continuer_define(self):
        tokens = generate_tokens('#define a \\\nb\n t')
        self.assertTrue('t' in tokens)

    def test_define2(self):
        tokens = generate_tokens(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
                    ''')
        self.assertTrue('t' in tokens)

    def test_half_comment_following(self):
        comment = '''#define A/*\n*/'''
        tokens = generate_tokens(comment)
        self.assertEqual(2, len(tokens))

    def test_block_comment_in_define(self):
        comment = '''#define A \\\n/*\\\n*/'''
        tokens = generate_tokens(comment)
        self.assertEqual(1, len(tokens))

class Test_generate_tonken_for_comments(unittest.TestCase):

    def test_c_style_comment(self):
        tokens = generate_tokens("/***\n**/")
        self.assertEqual(["/***\n**/"], tokens)

    def test_cpp_style_comment(self):
        tokens = generate_tokens("//aaa\n")
        self.assertEqual(['//aaa', '\n'], tokens)

    def test_cpp_style_comment_with_multiple_lines(self):
        tokens = generate_tokens("//a\\\nb")
        self.assertEqual(['//a\\\nb'], tokens)

    def test_commentedComment(self):
        tokens = generate_tokens(" /*/*/")
        self.assertEqual([' ', "/*/*/"], tokens)

    def test_with_cpp_comments(self):
        tokens = generate_tokens('//abc\n t')
        self.assertTrue('t' in tokens)

    def test_with_c_comments(self):
        tokens = generate_tokens('/*abc\n*/ t')
        self.assertTrue('t' in tokens)

    def test_with_c_comments_with_backslash_in_it(self):
        comment = '/**a/*/'
        tokens = generate_tokens(comment)
        self.assertListEqual([comment], tokens)

