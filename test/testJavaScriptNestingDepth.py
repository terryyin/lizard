import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth


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

    def test_keyword_after_a_dot_names_a_member(self):
        code = """
        function on(event, callback) {
          if (event === 'error') {
            this.$initialConnection.catch(() => {});
          }
          return EventEmitter.prototype.on.call(this, event, callback);
        }
        function drain(cursor, list, options) {
          if (cursor) {
            list?.finally(done).catch(fail);
            options.if = options.for + options.while + options.case;
            Promise.try(options.try);
          }
        }
        """
        for filename, source in (('a.js', code), ('a.ts', code),
                                 ('a.jsx', code), ('a.tsx', code),
                                 ('a.vue', '<script>' + code + '</script>')):
            with self.subTest(filename=filename):
                functions = FileAnalyzer(get_extensions(
                    [NestDepth()])).analyze_source_code(
                        filename, source).function_list
                self.assertEqual(
                    [('(anonymous)', 0), ('on', 1), ('drain', 1)],
                    [(f.name, f.max_nesting_depth) for f in functions])

    def test_keyword_after_a_dot_and_a_comment_names_a_member(self):
        code = """
        function load(p) {
          if (p) p./* retry */catch(fail);
        }
        function pick(o) {
          if (o.else) if (o.ready) run(o);
        }
        """
        functions = FileAnalyzer(get_extensions(
            [NestDepth()])).analyze_source_code("a.js", code).function_list
        self.assertEqual([('load', 1), ('pick', 2)],
                         [(f.name, f.max_nesting_depth) for f in functions])

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
        self.assertEqual(0, self.nesting_depth("""
        function ready(config, user, flag) {
          const enabled = (config.on && user && flag);
          return enabled;
        }
        """, "ready"))

    def test_arrow_body_with_an_expression_in_parentheses(self):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", """
        function bind(model, ready) {
          const set = value => model.value = (value) ? ready(value) : null;
          return set;
        }
        """).function_list
        self.assertEqual([('set', 1), ('bind', 0)],
                         [(f.name, f.max_nesting_depth) for f in result])

    def nesting_depth(self, code, name):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            "a.js", code).function_list
        return next(f.max_nesting_depth for f in result if f.name == name)
