import unittest
from .testHelpers import get_cpp_function_list

class TestCppCyclomaticComplexity(unittest.TestCase):

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

    def test_one_function_with_r_value_ref_in_parameter(self):
        result = get_cpp_function_list("int make(Args&&... args){}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_with_r_value_ref_in_body(self):
        result = get_cpp_function_list("int f() {Args&& a=b;}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_with_r_value_ref_in_body(self):
        result = get_cpp_function_list("int f() {Args&& a=b;}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_with_non_r_value_ref_in_body(self):
        result = get_cpp_function_list("int f() {a && b==c;}")
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_two_function_with_non_r_value_ref_in_body(self):
        result = get_cpp_function_list("""
        x c() {
          if (a && b) {
          }
        }
        x a() {
          inputs = c;
        }
        """)
        self.assertEqual(1, result[1].cyclomatic_complexity)

    def test_one_function_with_typedef(self):
        result = get_cpp_function_list("int f() {typedef int&& rref;}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_with_statement_no_curly_brackets(self):
        result = get_cpp_function_list("""
        x c() {
        if (a > -1 && b>= 0 )
              if(a != 0)
                    a = b;
        }
        """)
        self.assertEqual(4, result[0].cyclomatic_complexity)

