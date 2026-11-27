import unittest
from lizard import FileAnalyzer, get_extensions
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_cpp_with_nestdepth(source):
    return get_cpp_function_list_with_extension(source, NestDepth())


def get_js_nesting_depth(code, filename="a.js", function_index=0):
    result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
        filename, code).function_list
    return result[function_index].max_nesting_depth


class TestCppNestingDepth(unittest.TestCase):

    def test_one_function_with_no_nd_condition(self):
        result = get_cpp_with_nestdepth("int fun(){}")
        self.assertEqual(0, result[0].max_nesting_depth)

    def test_one_function_with_one_nd_condition(self):
        result = get_cpp_with_nestdepth("int fun(){if(a){xx;}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_with_one_nd_condition_no_curly_bracket(self):
        result = get_cpp_with_nestdepth("int fun(){if(a) xx;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_else(self):
        result = get_cpp_with_nestdepth("int fun(){if(a)xx;else b;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_and_curly_bracket(self):
        result = get_cpp_with_nestdepth("int fun(){if(a){xx;}else b;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_question_mark(self):
        result = get_cpp_with_nestdepth("int fun(){return (a)?b:c;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_default_parameter_short_name_is_not_a_structure(self):
        functions = get_cpp_with_nestdepth("""
        int resolvePort(int supplied, int def) {
            return supplied > 0 ? supplied : def;
        }
        """)
        self.assertEqual(1, functions[0].max_nesting_depth)

    def test_one_function_nd_with_forever_loop(self):
        result = get_cpp_with_nestdepth("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_if(self):
        result = get_cpp_with_nestdepth("int fun(){if(a)b;else if (c) d;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_if_chain(self):
        """Issue #418: else if chains should not increase nesting depth"""
        result = get_cpp_with_nestdepth(
            "int fun(){if(a){}else if(b){}else if(c){}}")
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
          if (a ? b : c) {
            if( a != 0 ){
                a = b;
            }
          }
        }
        x a() {
          if (a ? b : c){
            if( a != 0 )
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_one_function_nd_with_nested_loop_statement_minus_curly_brackets(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a ? b : c) {
            if( a != 0 ){
                a = b;
            }
          }
        }
        x a() {
          if (a ? b : c){
            if( a != 0 )
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_one_function_nd_with_nested_loop_statement_mixed_curly_brackets(self):
        result = get_cpp_with_nestdepth("""
        x c() {
          if (a ? b : c) {
            if(a != 0){
                a = b;
            }
          }
          if (a ? b : c){
            if(a != 0)
                a = b;
          }
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_one_function_nd_ignoring_explicit_forever_loop(self):
        result = get_cpp_with_nestdepth("""
        x a() {
          for(;;) {  // <-- explicit forever loop
            if(a != 0){
                a = b;
            }
          }
        }
        x b() {
          while(1) {
            if(a != 0){
                a = b;
            }
          }
        }
        x c() {
          while(true) {
            if(a != 0){
                a = b;
            }
          }
        }
        """)
        self.assertEqual(2, result[0].max_nesting_depth)
        self.assertEqual(2, result[1].max_nesting_depth)
        self.assertEqual(2, result[2].max_nesting_depth)

    def test_one_function_nd_with_addl_statement_in_if(self):
        result = get_cpp_with_nestdepth("""
        int fun() {
            if(a) {
                b;
                if (c) {
                    d;
                }
            }
        }""")
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_sibling_braceless_ifs_do_not_nest(self):
        """Issue #573: consecutive braceless ifs are siblings, not nested"""
        result = get_cpp_with_nestdepth("""
        int braceless(int a, int b, int c, int d) {
          if (a) f(1);
          if (b) f(2);
          if (c) f(3);
          if (d) f(4);
          return 0;
        }
        """)
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_sibling_braceless_ifs_inside_a_loop(self):
        result = get_cpp_with_nestdepth("""
        void f(int b) {
          for (;;) { if (b) g(); if (b) g(); if (b) g(); }
        }
        """)
        self.assertEqual(2, result[0].max_nesting_depth)
