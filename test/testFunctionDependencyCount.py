import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizarddependencycount import LizardExtension as DependencyCounter

class TestFunctionDependencyCount(unittest.TestCase):

    def test_no_return(self):
        result = get_cpp_function_list_with_extnesion("int fun(){}", DependencyCounter())
        self.assertEqual(0, result[0].dependency_count)

    def test_import_dependency(self):
        result = get_cpp_function_list_with_extnesion("import library; int fun(){library.callMethod()}", DependencyCounter())
        self.assertEqual(1, result[0].dependency_count)