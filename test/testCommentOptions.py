import unittest
from test.mock import Mock, patch
from .testHelpers import get_cpp_function_list

class TestCommentOptions(unittest.TestCase):

    def test_function_with_coment_option_should_be_forgiven(self):
        function_list = get_cpp_function_list("void foo(){/* #lizard forgives*/}")
        self.assertEqual(0, len(function_list))

    def test_function_with_coment_option_before_it_should_be_forgiven(self):
        function_list = get_cpp_function_list("/* #lizard forgives*/void foo(){}")
        self.assertEqual(0, len(function_list))

    def test_function_after_coment_option_should_not_be_forgiven(self):
        function_list = get_cpp_function_list("/* #lizard forgives*/void foo(){}void bar(){}")
        self.assertEqual(1, len(function_list))

