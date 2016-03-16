import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardmccabe import LizardExtension as McCabe

class TestFunctionExitCount(unittest.TestCase):

    def test_normal_case(self):
        result = get_cpp_function_list_with_extnesion("""int fun(){
                    switch(x) {
                        case 1: break;
                        case 2: break;
                        case 3: break;
                    };
                }""", McCabe())
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_fall_through(self):
        result = get_cpp_function_list_with_extnesion("""int fun(){
                    switch(x) {
                        case 1:
                        case 2: break;
                        case 3: break;
                    };
                }""", McCabe())
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_more(self):
        result = get_cpp_function_list_with_extnesion("""int fun(){
                    switch(x) {
                        case 1:
                        case 2:
                        case 3: break;
                    };
                }""", McCabe())
        self.assertEqual(2, result[0].cyclomatic_complexity)

