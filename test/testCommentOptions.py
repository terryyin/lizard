import unittest
from mock import Mock, patch
from .testHelpers import get_cpp_function_list

class TestCommentOptions(unittest.TestCase):

    def test_function_with_comment_option_should_be_forgiven(self):
        function_list = get_cpp_function_list("void foo(){/* #lizard forgives*/}")
        self.assertEqual(0, len(function_list))

    def test_function_with_comment_option_before_it_should_be_forgiven(self):
        function_list = get_cpp_function_list("/* #lizard forgives*/void foo(){}")
        self.assertEqual(0, len(function_list))

    def test_function_after_comment_option_should_not_be_forgiven(self):
        function_list = get_cpp_function_list("/* #lizard forgives*/void foo(){}void bar(){}")
        self.assertEqual(1, len(function_list))

    def test_generated_code_should_be_ignored(self):
        function_list = get_cpp_function_list("/* GENERATED CODE */void foo(){}")
        self.assertEqual(0, len(function_list))

