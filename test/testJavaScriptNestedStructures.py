import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardns import LizardExtension as NestedStructure


class TestJavaScriptNestedStructures(unittest.TestCase):

    REGEX = """
    function withRegex(text) {
      if (/#/.test(text)) {
        return 1;
      }
      return 0;
    }
    """

    NESTED = """
    function nested(list) {
      for (const x of list) {
        if (x) {
          while (x) {
            x--;
          }
        }
      }
    }
    """

    def setUp(self):
        self.analyzer = FileAnalyzer(get_extensions([NestedStructure()]))

    def nested_structures(self, filename, code):
        functions = self.analyzer.analyze_source_code(filename, code).function_list
        return [(f.name, f.max_nested_structures) for f in functions]

    def test_function_with_a_regular_expression(self):
        self.assertEqual(
            [('withRegex', 1), ('nested', 3)],
            self.nested_structures("a.js", self.REGEX + self.NESTED))

    def test_file_after_a_file_with_a_regular_expression(self):
        self.nested_structures("a.js", self.REGEX)
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.js", self.NESTED))
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.ts", self.NESTED))

    def test_return_regex_then_nested_on_same_analyzer(self):
        first = (
            'function a(s) { return /#/.test(s) || s === 1; }\n'
            'function nested(list) {\n'
            '  for (const x of list) { if (x) { while (x) { x--; } } }\n'
            '}\n'
        )
        self.assertEqual(
            [('a', 0), ('nested', 3)],
            self.nested_structures("a.js", first))
        self.assertEqual(
            [('nested', 3)], self.nested_structures("b.jsx", self.NESTED))

    def test_member_named_like_a_structure_is_not_a_structure(self):
        code = (
            'function load(p) {\n'
            '  return p.then(() => 1).catch(() => null);\n'
            '}\n'
            'function retry(p) {\n'
            '  p?.catch((e) => { if (e) { return 1; } });\n'
            '  try { p.finally(); } catch (e) { return 2; }\n'
            '}\n'
        )
        self.assertEqual(
            [('(anonymous)', 0), ('(anonymous)', 0), ('load', 0),
             ('(anonymous)', 1), ('retry', 1)],
            self.nested_structures("a.js", code))

    def test_else_if_chain_is_one_structure(self):
        """Issue #574: an `else if` chain does not add nesting"""
        code = (
            'function chain(a) {\n'
            '  if (a === 1) { return 1; } else if (a === 2) { return 2; }'
            ' else if (a === 3) { return 3; } else { return 4; }\n'
            '}\n'
        )
        self.assertEqual(
            [('chain', 1)], self.nested_structures("a.js", code))


class TestJavaScriptNestedStructuresWithoutSemicolons(unittest.TestCase):
    """A statement that ends at a line break closes its structures,
    as a statement that ends with ';' does."""

    def test_sibling_braceless_ifs_do_not_nest(self):
        self.assertEqual(1, self.nested_structures("""
        function role(user) {
          if (user.banned) return 'banned'
          if (user.admin) return 'admin'
          if (user.staff) return 'staff'
          return 'user'
        }
        """))

    def test_braceless_body_on_the_next_line(self):
        self.assertEqual(1, self.nested_structures("""
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
        self.assertEqual(3, self.nested_structures("""
        function notify(user, devices) {
          if (user.active)
            for (const device of devices)
              if (device.online) device.push(user)
          if (user.email) send(user.email)
        }
        """))

    def test_braceless_else_on_the_next_line(self):
        self.assertEqual(1, self.nested_structures("""
        function notify(user) {
          if (user.email) send(user.email)
          else log(user)
          if (user.phone)
            call(user.phone)
          else
            log(user)
          if (user.admin) audit(user)
        }
        """))

    def test_braceless_else_if_chain(self):
        self.assertEqual(1, self.nested_structures("""
        function role(user) {
          if (user.banned) return 'banned'
          else if (user.admin) return 'admin'
          else if (user.staff) return 'staff'
          else return 'user'
        }
        """))

    def test_do_while_loop(self):
        self.assertEqual(1, self.nested_structures("""
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
        self.assertEqual(2, self.nested_structures("""
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
        self.assertEqual(2, self.nested_structures("""
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
        self.assertEqual(3, self.nested_structures("""
        async function send(batch, rows) {
          if (batch.open) for
            await (const row of rows) if (row.ready) batch.add(row);
        }
        """))

    def test_while_loop_as_the_body_of_a_do_loop(self):
        self.assertEqual(3, self.nested_structures("""
        function drain(queue, log) {
          do while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_property_with_the_name_of_an_operator(self):
        self.assertEqual(1, self.nested_structures("""
        function scope(token, list) {
          if (token) return token.in
          if (list) return list.of
          return null
        }
        """))

    def test_typescript_non_null_assertion_at_a_line_end(self):
        self.assertEqual(1, self.nested_structures("""
        function owner(first: Item | null, second: Item | null) {
          if (first) return first.owner!
          if (second) return second.owner!
          return null
        }
        """, "a.ts"))

    def test_semicolon_on_the_line_after_return(self):
        self.assertEqual(2, self.nested_structures("""
        function next(queue, retry) {
          if (queue.empty) return
          ; else if (retry) if (queue.failed) queue.restart();
        }
        """))

    def test_label_before_the_body_of_a_do_loop(self):
        self.assertEqual(3, self.nested_structures("""
        function drain(queue, log) {
          do retry: while (queue.busy) if (log) log.wait(); while (queue.length);
        }
        """))

    def test_yield_ends_its_statement_at_a_line_break(self):
        self.assertEqual(1, self.nested_structures("""
        function* pages(book, cover) {
          if (cover) yield
          if (book.open) yield book.page
        }
        """))

    def test_typescript_non_null_assertion_after_a_property(self):
        self.assertEqual(1, self.nested_structures("""
        function scope(first: Token | null, second: Token | null) {
          if (second) return second.of!!
          if (first) return first.in!
          return null
        }
        """, "a.ts"))

    def test_typescript_const_assertion_at_a_line_end(self):
        self.assertEqual(1, self.nested_structures("""
        function side(left: Side | null, right: Side | null) {
          if (left) return [left, 'left'] as const
          if (right) return [right, 'right'] as const
          return null
        }
        """, "a.ts"))

    def test_typescript_void_return_type(self):
        self.assertEqual(1, self.nested_structures("""
        function close(file: File | null, force: boolean): void {
          if (!file) return
          if (force) file.destroy()
        }
        """, "a.ts"))

    def test_tsx_generic_return_type(self):
        self.assertEqual(1, self.nested_structures("""
        async function refresh(cached: boolean, offline: boolean): Promise<void> {
          if (cached) return
          if (offline) return
        }
        """, "a.tsx"))

    def test_typescript_return_type_that_ends_in_a_keyword_property(self):
        self.assertEqual(1, self.nested_structures("""
        function theme(dark: boolean, saved: Theme | null): typeof themes.default {
          if (saved) return saved
          if (dark) return themes.dark
        }
        """, "a.ts"))

    def nested_structures(self, code, filename="a.js"):
        analyzer = FileAnalyzer(get_extensions([NestedStructure()]))
        functions = analyzer.analyze_source_code(filename, code).function_list
        return functions[0].max_nested_structures
