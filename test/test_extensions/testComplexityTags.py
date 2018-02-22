import unittest
from ..testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags

class TestNonStrictExtension(unittest.TestCase):

    def test_empty_function(self):
        result = get_cpp_function_list_with_extension('''
        int fun(){}
        ''', ComplexTags())
        self.assertEqual([], result[0].complex_tags)

    def test_function_with_one_condition(self):
        result = get_cpp_function_list_with_extension('''
        int fun(){if (a==b) c;}
        ''', ComplexTags())
        self.assertEqual([['if', 2]], result[0].complex_tags)

    def test_multiple_lines(self):
        result = get_cpp_function_list_with_extension('''
        int fun(){
            if (a==b)
            c;
            }
        ''', ComplexTags())
        self.assertEqual([['if', 3]], result[0].complex_tags)
