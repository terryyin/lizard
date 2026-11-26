import unittest

import pytest

from lizard import FileAnalyzer, get_extensions
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardns import LizardExtension as NestedStructure


def process_cpp(source):
    return get_cpp_function_list_with_extension(source, NestedStructure())


@pytest.mark.parametrize('branch', (
    'if ($ready) { return "Ready"; } elseif ($pending) { BODY }',
    'if ($ready) return "Ready"; elseif ($pending) BODY',
    'if ($ready): echo "Ready"; elseif ($pending): BODY endif;',
    'if ($ready) { return "Ready"; } ELSEIF ($pending) { BODY }',
))
def test_elseif_body_contributes_its_level(branch):
    source = '''<?php
    function shippingLabel($ready, $pending, $backordered) {
        BRANCH
        return "Queued";
    }
    '''.replace('BRANCH', branch.replace('BODY', 'if ($backordered) return "Backordered";'))

    assert php_nested_structures(source) == {'shippingLabel': 2}


def test_elseif_excludes_finished_loops_and_file_guard():
    source = '''<?php
    if (!function_exists('shippingLabel')):
        function shippingLabel($ready, $pending, $orders) {
            if ($ready):
                foreach ($orders as $order) {
                    echo $order;
                }
            elseif ($pending):
                if ($orders) echo 'Pending';
            endif;
        }
    endif;
    '''

    assert php_nested_structures(source) == {'shippingLabel': 2}


def php_nested_structures(source):
    analyzer = FileAnalyzer(get_extensions(['ns']))
    functions = analyzer.analyze_source_code('shipping.php', source).function_list
    return {function.name: function.max_nested_structures for function in functions}


class TestCppNestedStructures(unittest.TestCase):

    def test_no_structures(self):
        result = process_cpp("int fun(){}")
        self.assertEqual(0, result[0].max_nested_structures)

    def test_if_structure(self):
        result = process_cpp("int fun(){if(a){xx;}}")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_ternary_operator(self):
        """The ternary operator is not a structure."""
        result = process_cpp("int fun(){return (a)?b:c;}")
        self.assertEqual(0, result[0].max_nested_structures)

    def test_forever_loop(self):
        result = process_cpp("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_terminator_in_parentheses(self):
        result = process_cpp("int fun(){for(int a;;){ if (a) return b;}}")
        self.assertEqual(2, result[0].max_nested_structures)

    def test_and_condition_in_if_structure(self):
        result = process_cpp("int fun(){if(a&&b){xx;}}")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_else_if(self):
        """Issue #574: an `else if` chain does not add nesting"""
        result = process_cpp("""
        x c() {
          if (a && b)
            baz();
          else if (c)
            foo();
        }
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_else_if_chain_with_blocks(self):
        result = process_cpp("""
        int chain(int x) {
          if (x == 1) { f(); } else if (x == 2) { g(); } else { h(); }
          return 0;
        }
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_structure_nested_in_else_if_branch(self):
        result = process_cpp("""
        int chain(int x) {
          if (x == 1) { f(); } else if (x == 2) { if (y) g(); }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_non_r_value_ref_in_body(self):
        result = process_cpp("""
        x c() {
          if (a && b) {
            baz();
          } else {
            foo();
          }
        }
        x a() {
          c = a && b;
        }
        """)
        self.assertEqual(1, result[0].max_nested_structures)
        self.assertEqual(0, result[1].max_nested_structures)

    def test_nested_if_structures(self):
        result = process_cpp("""
        x a() {
          if (a && b)
            if (a != 0)
               a = b;
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_nested_if_structures_in_2_functions(self):
        result = process_cpp("""
        x c() {
          if (a && b) {
            if(a != 0) {
                a = b;
            }
          }
        }
        x a() {
          if (a && b)
            if (a != 0)
               a = b;
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)
        self.assertEqual(2, result[1].max_nested_structures)

    def test_nested_loop_mixed_brackets(self):
        result = process_cpp("""
        x c() {
          if (a && b) {
            if (a != 0)
              a = b;
          }
        }
        x a() {
          if (a && b)
            if (a != 0) {
               a = b;
            }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)
        self.assertEqual(2, result[1].max_nested_structures)

    def test_equal_metric_structures(self):
        result = process_cpp("""
        x c() {
          if (a && b) {
            if(a != 0){
                a = b;
            } else b = a;
          }
          if (a && b){
            if(a != 0)
                a = b;
            else
                b = a;
          }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_while(self):
        result = process_cpp("""
        x c() {
          while (a && b) {
            if(a != 0){
                a = b;
            }
            while (s) continue;
          }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_do(self):
        result = process_cpp("""
        x c() {
          do {
            if(a != 0){
                a = b;
            } else {
                b = a;
            }
          } while (a && b);
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_try_catch(self):
        result = process_cpp("""
        x c() {
          try {
            if (a != 0) {
                foo();
            } else {
                bar();
            }
          } catch (auto& err) {}
        }
        x a() {
          try {
            foo();
          } catch (auto& err) {
            if (a != 0) {
                bar();
            } else {
                baz();
            }
          }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)
        self.assertEqual(2, result[1].max_nested_structures)

    def test_braceless_nested_if_try_structures(self):
        result = process_cpp("""
        x c() {
          if (a)
            try {
              throw 42;
            } catch(...) {
              if (b) return 42;
            }
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_non_block_if(self):
        result = process_cpp("""
        x c() {
          if (a)
            if(b) {
              {}
              if (c) { if (e) f; }
            }
        }
        """)
        self.assertEqual(4, result[0].max_nested_structures)

    def test_braceless_nested_if_and_for(self):
        result = process_cpp("""
        x c() {
          if (a)
              for (;;)
                if(b) c;
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_braceless_nested_for_try_structures(self):
        result = process_cpp("""
        x c() {
          for (;;)
            try {
              throw 42;
            } catch(...) {
              if (b) return 42;
            }
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_switch_case(self):
        """Switch-Case is one control structure."""
        result = process_cpp("""
        x c() {
          switch (a) {
            case 1:
              if (b && c) break;
            case 0:
            default:
              return;
          }
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)


class TestNestedStructuresAcrossFiles(unittest.TestCase):
    """The CLI reads all files with one extension object, and a file can
    end anywhere: inside a block, or in the condition of a structure."""

    PLAIN_JS = """
    function plainJs(x) {
      if (x) {
        return 1;
      }
      return 2;
    }
    """

    PLAIN_C = """
    int plain(int x) {
      if (x) {
        return 1;
      }
      return 2;
    }
    """

    def test_file_after_a_file_that_ends_inside_a_block(self):
        self.assertEqual([1], self.nested_structures_after(
            ("cut.js", "if (a) {\n  if (b) {\n"), ("c.js", self.PLAIN_JS)))

    def test_file_after_a_file_that_ends_in_a_condition(self):
        self.assertEqual([1], self.nested_structures_after(
            ("cut.c", "int f(int a) {\n  if (g(a"), ("c.c", self.PLAIN_C)))

    def test_file_in_another_language(self):
        self.assertEqual([1], self.nested_structures_after(
            ("cut.c", "int f(int a) {\n  if (a) {\n    while (b) {\n"),
            ("c.js", self.PLAIN_JS)))

    @staticmethod
    def nested_structures_after(first, second):
        analyzer = FileAnalyzer(get_extensions([NestedStructure()]))
        analyzer.analyze_source_code(*first)
        functions = analyzer.analyze_source_code(*second).function_list
        return [f.max_nested_structures for f in functions]
