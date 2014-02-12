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
        self.assertEqual(2, result[0].cyclomatic_complexity)
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
