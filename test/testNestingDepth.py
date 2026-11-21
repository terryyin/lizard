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

    def test_braceless_ifs_after_a_nested_function(self):
        self.assertEqual(1, self.nesting_depth("""
        function render(items, empty) {
          function label(item) { return item.name; }
          if (empty) return '';
          if (!items.length) return '';
          return items.map(label).join(', ');
        }
        """, "render"))

    def test_braceless_ifs_after_an_arrow_function(self):
        self.assertEqual(1, self.nesting_depth("""
        function total(orders, strict) {
          const amount = (order) => { return order.amount; };
          if (strict) check(orders);
          if (!orders.length) return 0;
          return orders.map(amount).reduce(add);
        }
        """, "total"))

    def test_braceless_ifs_after_an_expression_in_parentheses(self):
        self.assertEqual(1, self.nesting_depth("""
        function limit(value, max) {
          const over = (value > max);
          if (over) warn(value);
          if (value < 0) return 0;
          return over ? max : value;
        }
        """, "limit"))

    def test_braceless_ifs_after_an_arrow_function_argument(self):
        self.assertEqual(1, self.nesting_depth("""
        function names(users, strict) {
          const ids = users.map(user => user.id);
          if (strict) check(ids);
          if (!ids.length) return [];
          return ids;
        }
        """, "names"))

    def test_braceless_ifs_after_an_arrow_function_argument_with_an_object(self):
        self.assertEqual(1, self.nesting_depth("""
        function rows(users, strict) {
          const entries = users.map(user => ({ id: user.id }));
          if (strict) check(entries);
          if (!entries.length) return [];
          return entries;
        }
        """, "rows"))

    def test_logical_operators_in_parentheses_after_a_member(self):
        self.assertEqual(1, self.nesting_depth("""
        function ready(config, user, flag) {
          const enabled = (config.on && user && flag);
          return enabled;
        }
        """, "ready"))

    def test_arrow_body_with_an_expression_in_parentheses(self):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", """
        function bind(model, ready) {
          const set = value => model.value = (value) && ready(value);
          return set;
        }
        """).function_list
        self.assertEqual([('set', 1), ('bind', 0)],
                         [(f.name, f.max_nesting_depth) for f in result])

    def nesting_depth(self, code, name):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", code).function_list
        return next(f.max_nesting_depth for f in result if f.name == name)


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

    def test_catch_without_a_binding_before_its_block(self):
        self.assertEqual(2, self.nesting_depth("""
        function load(file) {
          try {
            open(file);
          } catch
          {
            if (file.optional) skip();
          }
        }
        """))

    def test_line_break_before_the_await_of_a_for_loop(self):
        self.assertEqual(3, self.nesting_depth("""
        async function send(batch, rows) {
          if (batch.open) for
            await (const row of rows) if (row.ready) batch.add(row);
        }
        """))

    def test_line_break_inside_a_declaration(self):
        self.assertEqual(2, self.nesting_depth("""
        function price(order, member) {
          if (order.paid) var
            discount = member && order.coupon;
          return discount;
        }
        """))

    def test_line_break_after_case(self):
        self.assertEqual(2, self.nesting_depth("""
        function notify(user) {
          switch (user.role) {
            case
              'admin': if (user.active) alert(user);
          }
        }
        """))

    def test_while_loop_as_the_body_of_a_do_loop(self):
        self.assertEqual(2, self.nesting_depth("""
        function drain(queue, log) {
          do while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_return_ends_its_statement_at_a_line_break(self):
        self.assertEqual(1, self.nesting_depth("""
        function refresh(cache, force) {
          if (!force) return
          (cache.pending || cache.loaded).clear()
        }
        """))

    def test_break_ends_its_statement_at_a_line_break(self):
        self.assertEqual(2, self.nesting_depth("""
        function stop(jobs, done) {
          while (jobs.length) {
            if (done) break
            [jobs.first || jobs.last].forEach(run)
          }
        }
        """))

    def test_property_with_the_name_of_an_operator(self):
        self.assertEqual(1, self.nesting_depth("""
        function scope(token, list) {
          if (token) return token.in
          if (list) return list.of
          return null
        }
        """))

    def test_typescript_non_null_assertion_at_a_line_end(self):
        self.assertEqual(1, self.nesting_depth("""
        function owner(first: Item | null, second: Item | null) {
          if (first) return first.owner!
          if (second) return second.owner!
          return null
        }
        """, "a.ts"))

    def test_line_break_after_a_prefix_operator(self):
        self.assertEqual(2, self.nesting_depth("""
        function hidden(item, user) {
          if (item.draft) return !
            user.admin && item.locked;
          return false;
        }
        """))

    def test_label_before_the_body_of_a_do_loop(self):
        self.assertEqual(2, self.nesting_depth("""
        function drain(queue, log) {
          do retry: while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_yield_ends_its_statement_at_a_line_break(self):
        self.assertEqual(1, self.nesting_depth("""
        function* pages(book, cover) {
          if (cover) yield
          if (book.open) yield book.page
        }
        """))

    def test_typescript_non_null_assertion_after_a_property(self):
        self.assertEqual(1, self.nesting_depth("""
        function scope(first: Token | null, second: Token | null) {
          if (second) return second.of!!
          if (first) return first.in!
          return null
        }
        """, "a.ts"))

    def test_object_literals_stay_in_their_statement(self):
        self.assertEqual(2, self.nesting_depth("""
        function flags(user, post) {
          return [{ ok: user.admin && user.active }, { ok: post.open && post.shared }];
        }
        """))

    def test_object_literal_at_the_end_of_a_statement(self):
        self.assertEqual(2, self.nesting_depth("""
        function allowed(user, post) {
          const flags = { admin: user.admin && user.active }
          if (post.open) if (flags.admin) return true
        }
        """))

    def test_typescript_const_assertion_at_a_line_end(self):
        self.assertEqual(1, self.nesting_depth("""
        function side(left: Side | null, right: Side | null) {
          if (left) return [left, 'left'] as const
          if (right) return [right, 'right'] as const
          return null
        }
        """, "a.ts"))

    def test_typescript_void_return_type(self):
        self.assertEqual(1, self.nesting_depth("""
        function close(file: File | null, force: boolean): void {
          if (!file) return
          if (force) file.destroy()
        }
        """, "a.ts"))

    def test_typescript_comparison_after_an_assertion(self):
        self.assertEqual(2, self.nesting_depth("""
        function above(value: unknown, limit: number, strict: boolean) {
          if (strict) return value as number >
            limit && strict;
          return false;
        }
        """, "a.ts"))

    def test_tsx_generic_return_type(self):
        self.assertEqual(1, self.nesting_depth("""
        async function refresh(cached: boolean, offline: boolean): Promise<void> {
          if (cached) return
          if (offline) return
        }
        """, "a.tsx"))

    def test_call_at_the_end_of_a_block(self):
        self.assertEqual(1, self.nesting_depth("""
        function resolve(insert, expected) {
          if (typeof insert === "function") { insert = insert() }
          if (typeof expected === "function") { expected = expected() }
        }
        """))

    def test_ternary_after_an_expression_in_parentheses(self):
        for condition in ("(current > start)", "current > start"):
            with self.subTest(condition=condition):
                self.assertEqual(3, self.nesting_depth("""
                function fold(next, start, current, width) {
                  while (next) {
                    if (next - start > width) {
                      end = %s ? current : next
                    }
                  }
                }
                """ % condition))

    def test_call_in_a_case_block(self):
        self.assertEqual(1, self.nesting_depth("""
        function dispatch(action) {
          switch (action.type) {
            case 'open': { show() }
            case 'close': { hide() }
          }
        }
        """))

    def test_typescript_return_type_that_ends_in_an_operator_word(self):
        self.assertEqual(1, self.nesting_depth("""
        function factory(source: Source, single: boolean): typeof Array.of {
          if (single) return source.one
          if (source.many) return source.all
        }
        """, "a.ts"))

    def test_typescript_return_type_that_ends_in_a_value_word(self):
        self.assertEqual(1, self.nesting_depth("""
        function theme(dark: boolean, saved: Theme | null): typeof themes.default {
          if (saved) return saved
          if (dark) return themes.dark
        }
        """, "a.ts"))

    def test_object_in_an_optional_computed_member_stays_an_object(self):
        self.assertEqual(2, self.nesting_depth("""
        function label(user) {
          return user?.[{ id: user && 1 }, user && 'name']
        }
        """))

    def nesting_depth(self, code, filename="a.js"):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            filename, code).function_list
        return result[0].max_nesting_depth
