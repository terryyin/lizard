import unittest
from .testNestingDepth import get_cpp_with_nestdepth


class TestLogicalOperatorNestingDepth(unittest.TestCase):

    def test_one_function_nd_with_and(self):
        result = get_cpp_with_nestdepth("int fun(){if(a&&b){xx;}}")
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_one_function_nd_with_non_r_value_ref_in_body(self):
        result = get_cpp_with_nestdepth("int f() {a && b==c;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_nesting_depth_with_multiple_or_operators_bug_426(self):
        """Test for issue #426: multiple || operators should not accumulate nesting depth incorrectly"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           // BlockA
           if (a) {           // ND=1
              if (a || b) {   // ND=2 (if + first ||)
                 bar = 1;     // ND=2
              }               // ND=1
           }                  // ND=0

           // BlockB
           if (a || b || c || d || e) {   // ND=2 (if + first ||, not 5!)
              foo = 1;                    // ND=2
           }                              // ND=0

           // BlockC
           if (a || b || c || d || e) { // ND=2 (if + first ||, not 3+5=8!)
              foo = 1;                  // ND=2
              bar = 1;                  // ND=2
              hoo = 2;                  // ND=2
           }                            // ND=0
        }
        """)
        self.assertEqual(3, result[0].max_nesting_depth)

    def test_nesting_depth_with_multiple_and_operators(self):
        """Test that multiple && operators don't accumulate nesting depth incorrectly"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           if (a && b && c && d && e) {   // ND=2 (if + first &&, not 5!)
              foo = 1;                    // ND=2
           }                              // ND=0
        }
        """)
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_nesting_depth_with_mixed_and_or_operators(self):
        """Test that mixed && and || operators don't accumulate nesting depth incorrectly"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           if (a && b || c && d || e) {   // ND=2 (if + first &&, not 5!)
              foo = 1;                    // ND=2
           }                              // ND=0
        }
        """)
        self.assertEqual(2, result[0].max_nesting_depth)
