import unittest

from .testHelpers import get_cpp_function_list_with_extnesion, \
    get_python_function_list_with_extnesion
from lizard_ext.lizardns import LizardExtension as NestedStructure


def process_cpp(source):
    return get_cpp_function_list_with_extnesion(source, NestedStructure())


def process_python(source):
    return get_python_function_list_with_extnesion(source, NestedStructure())


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
        result = process_cpp("""
        x c() {
          if (a && b)
            baz();
          else if (c)
            foo();
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

    def test_scope(self):
        result = process_cpp("""
        x c() {
          {{{{{{for (a : c) {
            {{{{if(a != 0) {{{{
                a = b;
            }}}}}}}}
          }}}}}}}
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_gotcha_if_else(self):
        """The last 'else' is associated with the innermost 'if'."""
        result = process_cpp("""
        x c() {
          if (a)
            if (b)
              call();
          else  // Deceiving indentation!
              for (auto c : d)  // #3 control structure is here!
                call(c);
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_non_structure_braces(self):
        """Extra braces for initializer lists may confuse the nesting level."""
        result = process_cpp("""
        x c() {
          if (a) return {{}};  // Initializer list with a braceless structure.
          if (b)
            if (c)
              if (d) return 42;
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def test_braceless_consecutive_if_structures(self):
        """Braceless structures one after another."""
        result = process_cpp("""
        x c() {
          if (a)
            if (b)
                foobar();
          if (c)
            if (d)
                baz();
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_braceless_consecutive_for_if_structures(self):
        """Braceless structures one after another."""
        result = process_cpp("""
        x c() {
          for (;;)
            for (;;)
                foobar();
          if (c)
            if (d)
                baz();
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_braceless_consecutive_if_structures_with_return(self):
        """Braceless structures one after another."""
        result = process_cpp("""
        x c() {
          if (a)
            if (b)
                return true;
          if (c)
            if (d)
                return false;
        }
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_braceless_nested_if_else_structures(self):
        result = process_cpp("""
        x c() {
          if (a)
            if (b) {
              return b;
            } else {
              if (b) return 42;
            }
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    def xtest_braceless_nested_if_else_if_structures(self):
        result = process_cpp("""
        x c() {
          if (a)
            if (b) {
              return b;
            } else if (c) {
              if (b) return 42;
            }
        }
        """)
        self.assertEqual(3, result[0].max_nested_structures)

    @unittest.skip("Unspecified. Not Implemented. Convoluted.")
    def test_struct_inside_declaration(self):
        """Extra complexity class/struct should be ignored."""
        result = process_cpp("""
        x c() {
          for (struct {int s; int foo() { while(c) return s; }} a{42};;) break;
        }
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    @unittest.skip("Unspecified. Not Implemented. Convoluted.")
    def test_struct_inside_definition(self):
        """Extra complexity class/struct should be ignored."""
        result = process_cpp("""
        x c() {
          for (;;) {
            struct {int s; int foo() { while(c) return s; }} a{42};
          }
        }
        """)
        self.assertEqual(1, result[0].max_nested_structures)


class X: #TestPythonNestedStructures(unittest.TestCase):

    def test_no_structures(self):
        result = process_python("def fun():\n pass")
        self.assertEqual(0, result[0].max_nested_structures)

    def test_if_structure(self):
        result = process_python("def fun():\n if a:\n  return")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_for_structure(self):
        result = process_python("def fun():\n for a in b:\n  foo()")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_condition_in_if_structure(self):
        result = process_python("def fun():\n if a and b:\n  return")
        self.assertEqual(1, result[0].max_nested_structures)

    def test_elif(self):
        result = process_python("""
        def c():
          if a:
            baz()
          elif c:
            foo()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_nested_if_structures(self):
        result = process_python("""
        def c():
          if a:
            if b:
              baz()
          else:
            foo()
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_equal_metric_structures(self):
        result = process_python("""
        def c():
          if a:
            if b:
              baz()
          else:
            foo()

          for a in b:
            if c:
              bar()
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_while(self):
        result = process_python("""
        def c():
          while a:
            baz()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_try_catch(self):
        result = process_python("""
        def c():
          try:
            f.open()
          catch Exception as err:
            print(err)
          finally:
            f.close()
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_two_functions(self):
        result = process_python("""
        def c():
          try:
            if a:
              foo()
          catch Exception as err:
            print(err)

        def d():
          for a in b:
            for x in y:
              if i:
                return j
        """)
        self.assertEqual(2, result[0].max_nested_structures)
        self.assertEqual(3, result[1].max_nested_structures)

    def test_nested_functions(self):
        result = process_python("""
        def c():
            def d():
                for a in b:
                    for x in y:
                        if i:
                            return j
            try:
                if a:
                    foo()
            catch Exception as err:
                print(err)

        """)
        self.assertEqual(3, result[0].max_nested_structures)
        self.assertEqual(2, result[1].max_nested_structures)

    def test_with_structure(self):
        result = process_python("""
        def c():
            with open(f) as input_file:
                foo(f)
        """)
        self.assertEqual(1, result[0].max_nested_structures)

    def test_for_else(self):
        result = process_python("""
        def c():
            for i in range(10):
                break
            else:
                for j in range(i):
                    print(j)
        """)
        self.assertEqual(2, result[0].max_nested_structures)

    def test_while_else(self):
        result = process_python("""
        def c(i):
            while i < 10:
                break
            else:
                for j in range(i):
                    print(j)
        """)
        self.assertEqual(2, result[0].max_nested_structures)
