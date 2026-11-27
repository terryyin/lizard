import unittest
from .testNestingDepth import get_js_nesting_depth


class TestJavaScriptNestingDepthWithoutSemicolons(unittest.TestCase):
    """A statement that ends at a line break closes its levels,
    as a statement that ends with ';' does."""

    def test_sibling_braceless_ifs_do_not_nest(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function role(user) {
          if (user.banned) return 'banned'
          if (user.admin) return 'admin'
          if (user.staff) return 'staff'
          return 'user'
        }
        """))

    def test_braceless_body_on_the_next_line(self):
        self.assertEqual(1, get_js_nesting_depth("""
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
        self.assertEqual(3, get_js_nesting_depth("""
        function notify(user, devices) {
          if (user.active)
            for (const device of devices)
              if (device.online) device.push(user)
          if (user.email) send(user.email)
        }
        """))

    def test_expression_continues_on_the_next_line(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function price(order, member) {
          if (order.total) discount = member
            ? 10
            : 0
          if (order.coupon) return order.coupon
          return discount
        }
        """))

    def test_statements_with_conditional_expressions(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function settings(user, team) {
          const theme = user.theme ? user.theme : team.theme
          const locale = user.locale ? user.locale : team.locale
          const admin = user.admin ? team.active : false
          return { theme, locale, admin }
        }
        """))

    def test_braceless_if_at_the_end_of_a_block(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function count(users) {
          let active = 0
          for (const user of users) { if (user.active) active++ }
          for (const user of users) { if (user.banned) active-- }
          if (active) return active
          return 0
        }
        """))

    def test_do_while_loop(self):
        self.assertEqual(1, get_js_nesting_depth("""
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
        self.assertEqual(2, get_js_nesting_depth("""
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
        self.assertEqual(2, get_js_nesting_depth("""
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
        self.assertEqual(3, get_js_nesting_depth("""
        async function send(batch, rows) {
          if (batch.open) for
            await (const row of rows) if (row.ready) batch.add(row);
        }
        """))

    def test_line_break_inside_a_declaration(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function price(order, member) {
          if (order.paid) var
            discount = member ? order.coupon : 0;
          return discount;
        }
        """))

    def test_line_break_after_case(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function notify(user) {
          switch (user.role) {
            case
              'admin': if (user.active) alert(user);
          }
        }
        """))

    def test_while_loop_as_the_body_of_a_do_loop(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function drain(queue, log) {
          do while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_return_ends_its_statement_at_a_line_break(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function refresh(cache, force) {
          if (!force) return
          (cache.pending || cache.loaded).clear()
        }
        """))

    def test_break_ends_its_statement_at_a_line_break(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function stop(jobs, done) {
          while (jobs.length) {
            if (done) break
            [jobs.first || jobs.last].forEach(run)
          }
        }
        """))

    def test_label_before_the_body_of_a_do_loop(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function drain(queue, log) {
          do retry: while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_yield_ends_its_statement_at_a_line_break(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function* pages(book, cover) {
          if (cover) yield
          if (book.open) yield book.page
        }
        """))

    def test_call_at_the_end_of_a_block(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function resolve(insert, expected) {
          if (typeof insert === "function") { insert = insert() }
          if (typeof expected === "function") { expected = expected() }
        }
        """))

    def test_call_in_a_case_block(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function dispatch(action) {
          switch (action.type) {
            case 'open': { show() }
            case 'close': { hide() }
          }
        }
        """))
