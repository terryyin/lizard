import unittest
from test.mock import Mock, patch
from .testHelpers import get_cpp_fileinfo, get_cpp_function_list

class Test_C_Token_Count(unittest.TestCase):

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
