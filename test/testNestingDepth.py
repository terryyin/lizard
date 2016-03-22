import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_cpp_with_nestdepth(source):
    return get_cpp_function_list_with_extnesion(source, NestDepth())


class TestCppNestingDepth(unittest.TestCase):

    def test_one_function_with_no_nd_condition(self):
        result = get_cpp_with_nestdepth("int fun(){}")
        self.assertEqual(0, result[0].max_nesting_depth)

    def test_one_function_with_one_nd_condition(self):
        result = get_cpp_with_nestdepth("int fun(){if(a){xx;}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_question_mark(self):
        result = get_cpp_with_nestdepth("int fun(){return (a)?b:c;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_forever_loop(self):
        result = get_cpp_with_nestdepth("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_and(self):
        result = get_cpp_with_nestdepth("int fun(){if(a&&b){xx;}}")
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_if(self):
        result = get_cpp_with_nestdepth("int fun(){if(a)b;else if (c) d;}")
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_one_function_nd_with_non_r_value_ref_in_body(self):
        result = get_cpp_with_nestdepth("int f() {a && b==c;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_two_function_nd_with_non_r_value_ref_in_body(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a && b) {
          }
        }
        x a() {
          inputs = c;
        }
        """)
        self.assertEqual(0, result[1].max_nesting_depth)

    def test_one_function_nd_with_nested_loop_statement_plus_curly_brackets(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a && b) {
            if( a != 0 ){
                a = b;
            }
          }
        }
        x a() {
          if (a && b){
            if( a != 0 )
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_one_function_nd_with_nested_loop_statement_minus_curly_brackets(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a && b) {
            if( a != 0 ){
                a = b;
            }
          }
        }
        x a() {
          if (a && b){
            if( a != 0 )
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_one_function_nd_with_nested_loop_statement_mixed_curly_brackets(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a && b) {
            if(a != 0){
                a = b;
            }
          }
          if (a && b){
            if(a != 0)
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)
