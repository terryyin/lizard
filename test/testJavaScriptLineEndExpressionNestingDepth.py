import unittest
from .testNestingDepth import get_js_nesting_depth


class TestJavaScriptNestingDepthAtExpressionEnds(unittest.TestCase):
    """An expression that ends at a line break ends its statement
    only when its last token can end an expression."""

    def test_property_with_the_name_of_an_operator(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function scope(token, list) {
          if (token) return token.in
          if (list) return list.of
          return null
        }
        """))

    def test_typescript_non_null_assertion_at_a_line_end(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function owner(first: Item | null, second: Item | null) {
          if (first) return first.owner!
          if (second) return second.owner!
          return null
        }
        """, "a.ts"))

    def test_line_break_after_a_prefix_operator(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function hidden(item, user) {
          if (item.draft) return !
            user.admin ? item.locked : false;
          return false;
        }
        """))

    def test_typescript_non_null_assertion_after_a_property(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function scope(first: Token | null, second: Token | null) {
          if (second) return second.of!!
          if (first) return first.in!
          return null
        }
        """, "a.ts"))

    def test_object_literals_stay_in_their_statement(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function flags(user, post) {
          return [{ ok: user.admin ? user.active : false }, { ok: post.open ? post.shared : false }];
        }
        """))

    def test_object_literal_at_the_end_of_a_statement(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function allowed(user, post) {
          const flags = { admin: user.admin && user.active }
          if (post.open) if (flags.admin) return true
        }
        """))

    def test_function_expression_at_the_end_of_a_statement(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function configure(options) {
          const resolve = options.resolve || function () { return true }
          if (options.debug) log(resolve)
        }
        """, function_index=-1))

    def test_member_access_continues_a_function_expression(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function configure(options) {
          const resolve = options.resolve ? function () { return this.ready }
            .bind(options) : options.fallback ? options.defaultResolve : null
          if (options.debug) log(resolve)
        }
        """, function_index=-1))

    def test_typescript_const_assertion_at_a_line_end(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function side(left: Side | null, right: Side | null) {
          if (left) return [left, 'left'] as const
          if (right) return [right, 'right'] as const
          return null
        }
        """, "a.ts"))

    def test_typescript_void_return_type(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function close(file: File | null, force: boolean): void {
          if (!file) return
          if (force) file.destroy()
        }
        """, "a.ts"))

    def test_typescript_comparison_after_an_assertion(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function above(value: unknown, limit: number, strict: boolean) {
          if (strict) return value as number >
            limit ? strict : false;
          return false;
        }
        """, "a.ts"))

    def test_tsx_generic_return_type(self):
        self.assertEqual(1, get_js_nesting_depth("""
        async function refresh(cached: boolean, offline: boolean): Promise<void> {
          if (cached) return
          if (offline) return
        }
        """, "a.tsx"))

    def test_ternary_after_an_expression_in_parentheses(self):
        for condition in ("(current > start)", "current > start"):
            with self.subTest(condition=condition):
                self.assertEqual(3, get_js_nesting_depth("""
                function fold(next, start, current, width) {
                  while (next) {
                    if (next - start > width) {
                      end = %s ? current : next
                    }
                  }
                }
                """ % condition))

    def test_typescript_return_type_that_ends_in_an_operator_word(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function factory(source: Source, single: boolean): typeof Array.of {
          if (single) return source.one
          if (source.many) return source.all
        }
        """, "a.ts"))

    def test_typescript_return_type_that_ends_in_a_value_word(self):
        self.assertEqual(1, get_js_nesting_depth("""
        function theme(dark: boolean, saved: Theme | null): typeof themes.default {
          if (saved) return saved
          if (dark) return themes.dark
        }
        """, "a.ts"))

    def test_object_in_an_optional_computed_member_stays_an_object(self):
        self.assertEqual(2, get_js_nesting_depth("""
        function label(user) {
          return user?.[{ id: user ? 1 : 0 }, user ? 'name' : 'id']
        }
        """))
