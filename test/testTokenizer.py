import unittest
from lizard import analyze_file
from lizard_languages.clike import CLikeReader
from lizard_languages.code_reader import CodeReader
def generate_tokens(source):
    return [t for t in CodeReader.generate_tokens(source)]


class Test_generate_token(unittest.TestCase):

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
        self.check_tokens(">>", '>', '>')
        self.check_tokens(">>=", '>>=')
        self.check_tokens("<<=", '<<=')

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

    def test_with_multiple_line_string(self):
        tokens = generate_tokens('"sss\nsss" t')
        self.assertTrue('t' in tokens)


class Test_generate_token_for_macros(unittest.TestCase):

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

class Test_generate_token_for_comments(unittest.TestCase):

    def test_c_style_comment(self):
        tokens = generate_tokens("/***\n**/")
        self.assertEqual(["/***\n**/"], tokens)

    def test_cpp_style_comment(self):
        tokens = generate_tokens("//aaa\n")
        self.assertEqual(['//aaa', '\n'], tokens)

    def test_cpp_style_comment_with_multiple_lines(self):
        tokens = list(CLikeReader.generate_tokens("//a\\\nb"))
        self.assertEqual(['//a\\\nb'], tokens)

    def test_c_reader_addition_comes_before_its_line_comment_rule(self):
        tokens = list(CLikeReader.generate_tokens("//a\\\nb", r"|//a"))
        self.assertEqual(['//a', '\\\n', 'b'], tokens)

    def test_line_comment_ends_at_its_line_after_a_backslash(self):
        tokens = generate_tokens("//a\\\nb")
        self.assertEqual(['//a\\', '\n', 'b'], tokens)

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


class Test_line_comment_ending_with_a_backslash(unittest.TestCase):
    """Only C, C++ and Objective-C continue a // comment after a backslash."""

    SOURCES = {
        'a.java': """
class A {
  int f(int a) {
    // path is C:\\
    if (a > 1) { return 2; }
    return 1;
  }
}
""",
        'a.cs': """
class A {
  int F(int a) {
    // path is C:\\
    if (a > 1) { return 2; }
    return 1;
  }
}
""",
        'a.dart': """
int f(int a) {
  // path is C:\\
  if (a > 1) { return 2; }
  return 1;
}
""",
        'a.go': """
func f(a int) int {
    // path is C:\\
    if a > 1 { return 2 }
    return 1
}
""",
        'a.kt': """
fun f(a: Int): Int {
    // path is C:\\
    if (a > 1) { return 2 }
    return 1
}
""",
        'a.rs': """
fn f(a: i32) -> i32 {
    // path is C:\\
    if a > 1 { return 2; }
    return 1;
}
""",
        'a.scala': """
object A {
  def f(a: Int): Int = {
    // path is C:\\
    if (a > 1) { return 2 }
    1
  }
}
""",
        'a.sol': """
contract A {
  function f(uint a) public returns (uint) {
    // path is C:\\
    if (a > 1) { return 2; }
    return 1;
  }
}
""",
        'a.swift': """
func f(a: Int) -> Int {
    // path is C:\\
    if a > 1 { return 2 }
    return 1
}
""",
        'a.zig': """
fn f(a: i32) i32 {
    // path is C:\\
    if (a > 1) { return 2; }
    return 1;
}
""",
    }

    def test_comment_ends_at_its_line(self):
        for filename, code in self.SOURCES.items():
            with self.subTest(filename=filename):
                function, = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(2, function.cyclomatic_complexity)

    def test_c_comment_continues_on_the_next_line(self):
        code = """
int f(int a) {
    // path is C:\\
    if (a > 1) { return 2; }
    return 1;
}
"""
        for filename in ('a.c', 'a.cpp', 'a.m'):
            with self.subTest(filename=filename):
                function, = analyze_file.analyze_source_code(
                    filename, code).function_list
                self.assertEqual(1, function.cyclomatic_complexity)
