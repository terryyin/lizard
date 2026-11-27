import contextlib
import io
import os
import unittest
from tempfile import TemporaryDirectory
import lizard
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth
from .testNestingDepth import get_cpp_with_nestdepth

LOOP_WITH_AND = """
void plain() { for(;;){ if (a) { x(); } } }
void logical() { for(;;){ if (a && b) { x(); } } }
"""


class TestLogicalOperatorNestingDepth(unittest.TestCase):

    def test_one_function_nd_with_and(self):
        result = get_cpp_with_nestdepth("int fun(){if(a&&b){xx;}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_non_r_value_ref_in_body(self):
        result = get_cpp_with_nestdepth("int f() {a && b==c;}")
        self.assertEqual(0, result[0].max_nesting_depth)

    def test_nesting_depth_with_multiple_or_operators_bug_426(self):
        """Issue #426: || operators add no nesting depth"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           // BlockA
           if (a) {           // ND=1
              if (a || b) {   // ND=2
                 bar = 1;     // ND=2
              }               // ND=1
           }                  // ND=0

           // BlockB
           if (a || b || c || d || e) {   // ND=1
              foo = 1;                    // ND=1
           }                              // ND=0

           // BlockC
           if (a || b || c || d || e) { // ND=1
              foo = 1;                  // ND=1
              bar = 1;                  // ND=1
              hoo = 2;                  // ND=1
           }                            // ND=0
        }
        """)
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_nesting_depth_with_multiple_and_operators(self):
        """Multiple && operators add no nesting depth"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           if (a && b && c && d && e) {   // ND=1
              foo = 1;                    // ND=1
           }                              // ND=0
        }
        """)
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_nesting_depth_with_mixed_and_or_operators(self):
        """Mixed && and || operators add no nesting depth"""
        result = get_cpp_with_nestdepth("""
        void foo(void) {
           if (a && b || c && d || e) {   // ND=1
              foo = 1;                    // ND=1
           }                              // ND=0
        }
        """)
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_logical_operator_in_a_condition_adds_no_depth_issue_693(self):
        result = get_cpp_with_nestdepth(LOOP_WITH_AND)
        self.assertEqual([('plain', 2), ('logical', 2)],
                         [(f.name, f.max_nesting_depth) for f in result])

    def test_logical_operator_outside_a_condition_adds_no_depth(self):
        result = get_cpp_with_nestdepth("void f() { int c = a && b; }")
        self.assertEqual(0, result[0].max_nesting_depth)

    def test_mixed_operators_in_a_loop_add_no_depth(self):
        result = get_cpp_with_nestdepth("""
        void plain() { while (x) { if (a) { y(); } } }
        void mixed() { while (x) { if (a || b && c) { y(); } } }
        """)
        self.assertEqual([2, 2], [f.max_nesting_depth for f in result])

    def test_groovy_logical_operator_adds_no_depth(self):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.groovy", """
def plain() {
    for (x in y) { if (a) { f() } }
}
def logical() {
    for (x in y) { if (a && b) { f() } }
}
""").function_list
        self.assertEqual([('plain', 2), ('logical', 2)],
                         [(f.name, f.max_nesting_depth) for f in result])

    def test_nested_structures_are_unchanged_on_the_command_line(self):
        with TemporaryDirectory() as directory:
            path = os.path.join(directory, "t.c")
            with open(path, "w") as source:
                source.write(LOOP_WITH_AND)
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                lizard.main(["lizard", "-END", "-ENS", path])
        rows = [line.split() for line in output.getvalue().splitlines()
                if "@" + path in line]
        self.assertEqual([("2", "2", "plain"), ("2", "2", "logical")],
                         [(row[5], row[6], row[7].split("@")[0])
                          for row in rows])
