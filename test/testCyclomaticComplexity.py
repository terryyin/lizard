import unittest
from .testHelpers import get_cpp_fileinfo, get_cpp_function_list

class TestCyclomaticComplexity(unittest.TestCase):

    def test_one_function_with_no_condition(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_with_one_condition(self):
        result = get_cpp_function_list("int fun(){if(a){xx;}}")
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_one_function_with_question_mark(self):
        result = get_cpp_function_list("int fun(){return (a)?b:c;}")
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_one_function_with_forever_loop(self):
        result = get_cpp_function_list("int fun(){for(;;){dosomething();}}")
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_one_function_with_and(self):
        result = get_cpp_function_list("int fun(){if(a&&b){xx;}}")
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_one_function_with_else_if(self):
        result = get_cpp_function_list("int fun(){if(a)b;else if (c) d;}")
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_sharp_if_and_sharp_elif_counts_in_cc_number(self):
        result = get_cpp_function_list('''
                int main(){
                #ifdef A
                #elif (defined E)
                #endif
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

