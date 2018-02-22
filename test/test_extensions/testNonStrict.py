import unittest
from ..testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardnonstrict import LizardExtension as NonStrict

class TestNonStrictExtension(unittest.TestCase):

    def test_normal_function_without_boolean_operators(self):
        result = get_cpp_function_list_with_extension('''
        int fun(){if (a==b) c;}
        ''', NonStrict())
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_normal_function_with_boolean_operators(self):
        result = get_cpp_function_list_with_extension('''
        int fun(){if (a && b) c;}
        ''', NonStrict())
        self.assertEqual(2, result[0].cyclomatic_complexity)
