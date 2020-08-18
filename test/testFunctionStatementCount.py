import unittest
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardstatementcount import LizardExtension as StatementCounter


class TestFunctionStatementCount(unittest.TestCase):

    def test_empty_function_should_count_as_0(self):
        result = get_cpp_function_list_with_extension("int fun(){}", StatementCounter())
        self.assertEqual(0, result[0].statement_count)

    def test_function_with_return_count_as_1(self):
        result = get_cpp_function_list_with_extension("int fun(){return 0;}", StatementCounter())
        self.assertEqual(1, result[0].statement_count)

    def test_function_with_if_else_count_as_3(self):
        result = get_cpp_function_list_with_extension(
            "int fun(){if (0) { return 0; } else { return 1;}}", StatementCounter())
        self.assertEqual(3, result[0].statement_count)

    def test_function_with_for_count_as_6(self):
        result = get_cpp_function_list_with_extension(
            'int fun(){int i; for(i=0;i<2;i++) { print("%d",i); } return 1;}',
            StatementCounter())
        self.assertEqual(6, result[0].statement_count)

    def test_function_with_while_count_as_5(self):
        result = get_cpp_function_list_with_extension(
            'int fun(){int i=0; while(i<2) { print("%d",i); i++; } return 1;}',
            StatementCounter())
        self.assertEqual(5, result[0].statement_count)
