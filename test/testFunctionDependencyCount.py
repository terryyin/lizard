import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizarddependencycount import LizardExtension as DependencyCounter


class TestFunctionDependencyCount(unittest.TestCase):

    def test_no_return(self):
        result = get_cpp_function_list_with_extnesion(
            "int fun(){}",
            DependencyCounter())
        self.assertEqual(0, result[0].dependency_count)

    def test_import_dependency(self):
        result = get_cpp_function_list_with_extnesion(
            "import library; int fun(){library.callMethod()}",
            DependencyCounter())
        self.assertEqual(1, result[0].dependency_count)

    def test_python_import_as(self):
        result = get_cpp_function_list_with_extnesion(
            "import python as py; int fun(){py.callMethod() py.version = 99}",
            DependencyCounter())
        self.assertEqual(2, result[0].dependency_count)
        result = get_cpp_function_list_with_extnesion(
            "import candy int fun(){candy.callMethod() py.version = 99}",
            DependencyCounter())
        self.assertEqual(1, result[0].dependency_count)
        result = get_cpp_function_list_with_extnesion(
            "import kok as www import tree, java as monster, coffee import java public board { private void function() { java += 1; java.tree = green; www.yay(0); teacher.lecture(0); }",
            DependencyCounter())
        self.assertEqual(3, result[0].dependency_count)
