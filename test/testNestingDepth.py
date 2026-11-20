import unittest
from lizard import FileAnalyzer, get_extensions
from .testHelpers import get_cpp_function_list_with_extension
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_cpp_with_nestdepth(source):
    return get_cpp_function_list_with_extension(source, NestDepth())


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

    def test_one_function_nd_with_forever_loop(self):
        result = get_cpp_with_nestdepth("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_and(self):
        result = get_cpp_with_nestdepth("int fun(){if(a&&b){xx;}}")
        self.assertEqual(2, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_if(self):
        result = get_cpp_with_nestdepth("int fun(){if(a)b;else if (c) d;}")
        self.assertEqual(1, result[0].max_nesting_depth)

    def test_one_function_nd_with_else_if_chain(self):
        """Issue #418: else if chains should not increase nesting depth"""
        result = get_cpp_with_nestdepth(
            "int fun(){if(a){}else if(b){}else if(c){}}")
        self.assertEqual(1, result[0].max_nesting_depth)

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


class TestJavaScriptNestingDepth(unittest.TestCase):

    def test_sibling_braceless_ifs_do_not_nest(self):
        """Issue #573: consecutive braceless ifs are siblings, not nested"""
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", """
        function braceless(a, b, c, d) {
          if (a) console.log(1);
          if (b) console.log(2);
          if (c) console.log(3);
          if (d) console.log(4);
          return 0;
        }
        """).function_list
        self.assertEqual(1, result[0].max_nesting_depth)


class TestJavaScriptNestingDepthWithoutSemicolons(unittest.TestCase):
    """A statement that ends at a line break closes its levels,
    as a statement that ends with ';' does."""

    def test_sibling_braceless_ifs_do_not_nest(self):
        self.assertEqual(1, self.nesting_depth("""
        function role(user) {
          if (user.banned) return 'banned'
          if (user.admin) return 'admin'
          if (user.staff) return 'staff'
          return 'user'
        }
        """))

    def test_braceless_body_on_the_next_line(self):
        self.assertEqual(1, self.nesting_depth("""
        function role(user) {
          if (user.banned)
            return 'banned'
          if (user.admin)
            return 'admin'
          if (user.staff)
            return 'staff'
          return 'user'
        }
        """))

    def test_nested_braceless_structures_still_nest(self):
        self.assertEqual(3, self.nesting_depth("""
        function notify(user, devices) {
          if (user.active)
            for (const device of devices)
              if (device.online) device.push(user)
          if (user.email) send(user.email)
        }
        """))

    def test_expression_continues_on_the_next_line(self):
        self.assertEqual(2, self.nesting_depth("""
        function price(order, member) {
          if (order.total) discount = member
            ? 10
            : 0
          if (order.coupon) return order.coupon
          return discount
        }
        """))

    def test_statements_with_logical_operators(self):
        self.assertEqual(1, self.nesting_depth("""
        function settings(user, team) {
          const theme = user.theme || team.theme
          const locale = user.locale || team.locale
          const admin = user.admin && team.active
          return { theme, locale, admin }
        }
        """))

    def test_braceless_if_at_the_end_of_a_block(self):
        self.assertEqual(2, self.nesting_depth("""
        function count(users) {
          let active = 0
          for (const user of users) { if (user.active) active++ }
          for (const user of users) { if (user.banned) active-- }
          if (active) return active
          return 0
        }
        """))

    def test_do_while_loop(self):
        self.assertEqual(1, self.nesting_depth("""
        function drain(queue, log) {
          do {
            queue.pop()
          } while (queue.length)
          if (log) log.flush()
          if (queue.closed) return 1
          return 0
        }
        """))

    def test_braces_on_their_own_line(self):
        self.assertEqual(2, self.nesting_depth("""
        function notify(user, devices)
        {
          if (user.active)
          {
            for (const device of devices)
            {
              device.push(user)
            }
          }
          try
          {
            send(user.email)
          }
          catch (error)
          {
            report(error)
          }
        }
        """))

    def nesting_depth(self, code):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", code).function_list
        return result[0].max_nesting_depth
