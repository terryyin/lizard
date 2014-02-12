import unittest
from lizard import generate_tokens


def get_tokens(source_code):
    return [x for x, l in generate_tokens(source_code)]

def get_tokens_and_line(source_code):
    return [x for x in generate_tokens(source_code)]

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

    def test_empty(self):
        tokens = get_tokens("")
        self.assertEqual(0, len(tokens))

    def test_one_digit(self):
        tokens = get_tokens("1")
        self.assertEqual(['1'], tokens)

    def test_operators(self):
        tokens = get_tokens("-;")
        self.assertEqual(['-', ';'], tokens)

    def test_operators1(self):
        tokens = get_tokens("-=")
        self.assertEqual(['-='], tokens)

    def test_operators2(self):
        tokens = get_tokens(">=")
        self.assertEqual(['>='], tokens)

    def test_more(self):
        tokens = get_tokens("int a{}")
        self.assertEqual(['int', "a", "{", "}"], tokens)

    def test_or(self):
        tokens = get_tokens("||")
        self.assertEqual(['||'], tokens)

    def test_string(self):
        tokens = get_tokens(r'""')
        self.assertEqual(['""'], tokens)
        tokens = get_tokens(r'"x\"xx")')
        self.assertEqual(['"x\\"xx"', ')'], tokens)

    def test_line_number(self):
        tokens = get_tokens_and_line(r'abc')
        self.assertEqual(('abc', 1), tokens[0])

    def test_line_number2(self):
        tokens = get_tokens_and_line('abc\ndef')
        self.assertTrue(('def', 2) in tokens)

    def test_with_mutiple_line_string(self):
        tokens = get_tokens_and_line('"sss\nsss" t')
        self.assertTrue(('t', 2) in tokens)


class Test_generate_tonken_for_marcos(unittest.TestCase):

    def test_define(self):
        define =  '''#define xx()\
                       abc'''
        tokens = get_tokens(define+'''
                    int''')
        self.assertEqual([define, '\n', 'int'], tokens)

    def test_if(self):
        tokens = get_tokens('''#if abc\n''')
        self.assertEqual(['#if abc', '\n'], tokens)

    def test_ifdef(self):
        tokens = get_tokens('''#ifdef abc\n''')
        self.assertEqual(['#ifdef abc', '\n'], tokens)

    def test_with_line_continuer_define(self):
        tokens = get_tokens_and_line('#define a \\\nb\n t')
        self.assertTrue(('t', 3) in tokens)

    def test_define2(self):
        tokens = get_tokens_and_line(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
                    ''')
        self.assertTrue(('t', 3) in tokens)


class Test_generate_tonken_for_comments(unittest.TestCase):

    def test_c_style_comment(self):
        tokens = get_tokens("/***\n**/")
        self.assertEqual(["/***\n**/"], tokens)

    def test_cpp_style_comment(self):
        tokens = get_tokens("//aaa\n")
        self.assertEqual(['//aaa', '\n'], tokens)

    def test_cpp_style_comment_with_multiple_lines(self):
        tokens = get_tokens("//a\\\nb")
        self.assertEqual(['//a\\\nb'], tokens)

    def test_commentedComment(self):
        tokens = get_tokens(" /*/*/")
        self.assertEqual(["/*/*/"], tokens)

    def test_with_cpp_comments(self):
        tokens = get_tokens_and_line('//abc\n t')
        self.assertTrue(('t', 2) in tokens)

    def test_with_c_comments(self):
        tokens = get_tokens_and_line('/*abc\n*/ t')
        self.assertTrue(('t', 2) in tokens)

