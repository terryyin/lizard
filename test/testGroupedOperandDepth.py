import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth
from .testNestingDepth import get_cpp_with_nestdepth


class TestCppNestingDepthGroups(unittest.TestCase):

    def test_grouped_cpp_comma_uses_the_owning_expression(self):
        functions = get_cpp_with_nestdepth("""
        #include <utility>
        template<class Left, class Right> bool allow(bool value) { return value; }

        bool accept(bool enabled, bool cached, bool ready, bool fresh) {
            return (enabled && cached && allow<int, int>(ready && fresh));
        }
        bool acceptPair(bool enabled, bool cached, bool ready, bool fresh) {
            return (enabled && cached && allow<std::pair<int, int>, bool>(ready && fresh));
        }
        bool checkLimits(bool enabled, bool cached, bool ready, bool fresh,
                         int count, int threshold, int limit) {
            return (enabled && cached && count < threshold, limit > (ready && fresh));
        }
        struct State { bool ready; };
        bool operator>(int* pending, State state) {
            delete pending;
            return state.ready;
        }
        bool checkAllocation(bool enabled, bool cached, bool ready, bool fresh,
                             int count, int threshold) {
            return (enabled && cached && count < threshold,
                    new int > (State{ready && fresh}));
        }
        """)
        self.assertEqual([('allow', 0), ('accept', 2), ('acceptPair', 2), ('checkLimits', 1),
                          ('operator >', 0), ('checkAllocation', 1)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_grouped_comma_separates_logical_expressions(self):
        functions = get_cpp_with_nestdepth("""
        void report(bool flag);
        bool allowed(bool a, bool b, bool c, bool d, bool enabled) {
            if (enabled) return (report(a && b), c && d);
            return false;
        }
        """)
        self.assertEqual(2, functions[0].max_nesting_depth)

    def test_parentheses_keep_operators_in_the_same_standalone_chain(self):
        functions = get_cpp_with_nestdepth("""
        bool check(bool);
        bool isNameStart(char c) {
            return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
        }
        bool isReady(bool ready, bool cached, bool stale, bool allowed) {
            return ready && ((cached || stale) && allowed);
        }
        bool hasAccess(bool ready, bool cached, bool stale) {
            return ready && check(cached || stale);
        }
        """)
        self.assertEqual([('isNameStart', 1), ('isReady', 1), ('hasAccess', 2)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_standalone_logical_chain_has_one_level_with_or_without_parentheses(self):
        functions = get_cpp_with_nestdepth("""
        bool enabled(bool present, bool active, bool ready) {
            return present && active && ready;
        }
        bool enabledGrouped(bool present, bool active, bool ready) {
            return (present && active && ready);
        }
        """)
        self.assertEqual([1, 1], [f.max_nesting_depth for f in functions])


class TestJavaScriptNestingDepthWithoutSemicolonsGroups(unittest.TestCase):

    def test_grouped_comma_separates_logical_expressions(self):
        self.assertEqual(1, self.nesting_depth("""
        function allowed(report, a, b, c, d) {
            return (report(a && b), c && d)
        }
        """))

    def test_standalone_logical_chain_ends_at_the_next_statement(self):
        self.assertEqual(1, self.nesting_depth("""
        function check(user, team, config) {
          const login = user.active && team.active && config.login
          const admin = (login && user.admin && config.admin)
          return { login, admin }
        }
        """))

    def nesting_depth(self, code, filename="a.js"):
        result = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            filename, code).function_list
        return result[0].max_nesting_depth
