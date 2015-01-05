import unittest
from test.mock import Mock, patch
from .testHelpers import get_cpp_fileinfo, get_cpp_function_list

class Test_Token_Count(unittest.TestCase):

    def test_non_function_tokens_are_counted(self):
        fileinfo = get_cpp_fileinfo("int i, j;")
        self.assertEqual(5, fileinfo.token_count)

    def test_include_is_counted_as_2(self):
        fileinfo = get_cpp_fileinfo("#include \"abc.h\"")
        self.assertEqual(2, fileinfo.token_count)

    def test_include_with_lg_and_gg_is_counted_as_2(self):
        fileinfo = get_cpp_fileinfo("#include <abc.h>")
        self.assertEqual(2, fileinfo.token_count)

    def test_one_function_with_no_token(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(5, result[0].token_count)

    def test_one_function_with_one_token(self):
        result = get_cpp_function_list("int fun(){;}")
        self.assertEqual(6, result[0].token_count)

    def test_one_function_with_content(self):
        result = get_cpp_function_list("int fun(){if(a){xx;}}")
        self.assertEqual(13, result[0].token_count)

    def test_one_function_with_comments_only(self):
        result = get_cpp_function_list("int fun(){/**/}")
        self.assertEqual(5, result[0].token_count)

class TestNLOC(unittest.TestCase):

    def test_one_function_with_content(self):
        result = get_cpp_function_list("int fun(){if(a){xx;}}")
        self.assertEqual(1, result[0].nloc)

    def test_nloc_of_empty_function(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(1, result[0].nloc)

    def test_nloc(self):
        result = get_cpp_function_list("int fun(){\n\n\n}")
        self.assertEqual(2, result[0].nloc)

    def test_nloc_with_new_line_in_comment(self):
        result = get_cpp_function_list("int fun(){/*\n*/}")
        self.assertEqual(2, result[0].nloc)

    def test_nloc_with_comment_between_new_lines(self):
        result = get_cpp_function_list("int fun(){\n/*\n*/\n}")
        self.assertEqual(2, result[0].nloc)

    def test_nloc2(self):
        result = get_cpp_function_list("int fun(){aa();\n\n\n\nbb();\n\n\n}")
        self.assertEqual(3, result[0].nloc)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(8, result[0].end_line)

    def check_file_nloc(self, expect, source):
        fileinfo = get_cpp_fileinfo(source)
        self.assertEqual(expect, fileinfo.nloc)

    def test_last_line_without_return_should_be_counted_in_fileinfo(self):
        self.check_file_nloc(1, ";\n")
        self.check_file_nloc(2, ";\n\n;\n")
        self.check_file_nloc(2, ";\n;")
        self.check_file_nloc(1, "fun(){}")
        self.check_file_nloc(1, "fun(){};\n")


class TestLOC(unittest.TestCase):

    def test_having_empty_line(self):
        result = get_cpp_function_list("\nint fun(){}")
        self.assertEqual(2, result[0].start_line)

    def test_newline_in_macro(self):
        result = get_cpp_function_list("#define a\\\nb\nint fun(){}")
        self.assertEqual(3, result[0].start_line)

    def test_having_empty_line_that_has_spaces(self):
        result = get_cpp_function_list("  \nint fun(){}")
        self.assertEqual(2, result[0].start_line)

    def test_having_multiple_line_comments(self):
        result = get_cpp_function_list('''int fun(){
        /*2
          3
          4*/
                }''')
        self.assertEqual(5, result[0].end_line)

class TestFileNLOC(unittest.TestCase):

    def test_empty_file_should_has_0_nloc(self):
        fileinfo = get_cpp_fileinfo("")
        self.assertEqual(0, fileinfo.nloc)

    def test_one_line_file_should_has_1_nloc(self):
        fileinfo = get_cpp_fileinfo("a")
        self.assertEqual(1, fileinfo.nloc)

    def test_one_line_file_with_newline_at_the_end_should_has_1_nloc(self):
        fileinfo = get_cpp_fileinfo("a\n")
        self.assertEqual(1, fileinfo.nloc)

    def test_two_one_line_file_should_has_2_nloc(self):
        fileinfo = get_cpp_fileinfo("a\nb")
        self.assertEqual(2, fileinfo.nloc)

    def test_comment_should_not_count(self):
        fileinfo = get_cpp_fileinfo("//abc")
        self.assertEqual(0, fileinfo.nloc)
