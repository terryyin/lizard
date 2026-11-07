import unittest

from .testNestedStructures import process_cpp


class TestCppNestedStructureScopes(unittest.TestCase):

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

    def test_raw_string_literal_with_braces(self):
        """Raw string literals containing braces should not cause IndexError."""
        result = process_cpp(r'''
        int main() {
            const char* json = R"({
  "name": "Ada Lovelace",
  "id": 101,
  "languages": ["C++", "Python", "Assembly"],
  "active": true,
  "profile": {
    "bio": "Mathematician & pioneer",
    "links": {
      "website": "https://example.com/ada",
      "github": "https://github.com/ada"
    }
  }
})";
            for (int i = 1; i <= 3; ++i) {
                for (int j = 1; j <= 3; ++j) {
                    return i + j;
                }
            }
            return 0;
        }
        ''')
        self.assertEqual(2, result[0].max_nested_structures)

    def test_raw_string_literal_with_delimiter(self):
        """Raw string literals with delimiters should be handled correctly."""
        result = process_cpp(r'''
        int fun() {
            const char* s = R"delim(some {content} with )delim";
            if (true) {
                return 1;
            }
            return 0;
        }
        ''')
        self.assertEqual(1, result[0].max_nested_structures)

    def test_raw_string_literal_simple(self):
        """Simple raw string literals should not interfere with complexity counting."""
        result = process_cpp(r'''
        int fun() {
            const char* s = R"(hello world)";
            if (a) {
                if (b) {
                    return c;
                }
            }
            return 0;
        }
        ''')
        self.assertEqual(2, result[0].max_nested_structures)
