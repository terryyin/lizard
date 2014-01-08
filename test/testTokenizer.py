import unittest
from lizard import generate_tokens


class Test_generate_tonken(unittest.TestCase):

    def test_empty_string(self):
        result = [t for t in generate_tokens("")]
        self.assertEqual(0, len(result))

    def test_with_one_return(self):
        result = [t for t in generate_tokens("\n")]
        self.assertEqual(1, len(result))

    def test_with_two_returns(self):
        result = [t for t in generate_tokens("\n\n")]
        self.assertEqual(1, len(result))

    def get_tokens(self, source_code):
        return [x for x, l in generate_tokens(source_code)]

    def get_tokens_and_line(self, source_code):
        return [x for x in generate_tokens(source_code)]

    def test_empty(self):
        tokens = self.get_tokens("")
        self.assertEqual(0, len(tokens))

    def test_one_digit(self):
        tokens = self.get_tokens("1")
        self.assertEqual(['1'], tokens)

    def test_operators(self):
        tokens = self.get_tokens("-;")
        self.assertEqual(['-', ';'], tokens)

    def test_operators1(self):
        tokens = self.get_tokens("-=")
        self.assertEqual(['-='], tokens)

    def test_operators2(self):
        tokens = self.get_tokens(">=")
        self.assertEqual(['>='], tokens)

    def test_more(self):
        tokens = self.get_tokens("int a{}")
        self.assertEqual(['int', "a", "{", "}"], tokens)

    def test_or(self):
        tokens = self.get_tokens("||")
        self.assertEqual(['||'], tokens)

    def test_comment(self):
        tokens = self.get_tokens("/***\n**/")
        self.assertEqual([], tokens)
        tokens = self.get_tokens("//aaa\n")
        self.assertEqual(['\n'], tokens)

    def test_commentedComment(self):
        tokens = self.get_tokens(" /*/*/")
        self.assertEqual([], tokens)

    def test_string(self):
        tokens = self.get_tokens(r'""')
        self.assertEqual(['""'], tokens)
        tokens = self.get_tokens(r'"x\"xx")')
        self.assertEqual(['"x\\"xx"', ')'], tokens)

    def test_define(self):
        tokens = self.get_tokens('''#define xx()\
                                      abc
                                    int''')
        self.assertEqual(['\n', 'int'], tokens)
       
    def test_line_number(self):
        tokens = self.get_tokens_and_line(r'abc')
        self.assertEqual(('abc', 1), tokens[0])

    def test_line_number2(self):
        tokens = self.get_tokens_and_line('abc\ndef')
        self.assertTrue(('def', 2) in tokens)

    def test_with_mutiple_line_string(self):
        tokens = self.get_tokens_and_line('"sss\nsss" t')
        self.assertTrue(('t', 2) in tokens)

    def test_with_cpp_comments(self):
        tokens = self.get_tokens_and_line('//abc\n t')
        self.assertTrue(('t', 2) in tokens)

    def test_with_line_continuer_comments(self):
        tokens = self.get_tokens_and_line('#define a \\\nb\n t')
        self.assertTrue(('t', 3) in tokens)

    def test_define2(self):
        tokens = self.get_tokens_and_line(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
''')
        self.assertTrue(('t', 3) in tokens)

    def test_with_c_comments(self):
        tokens = self.get_tokens_and_line('/*abc\n*/ t')
        self.assertTrue(('t', 2) in tokens)

