import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth
from .testNestingDepth import get_cpp_with_nestdepth


class TestCppNestingDepthCondition(unittest.TestCase):

    def test_condition_lambda_keeps_its_declarator_until_the_body(self):
        functions = get_cpp_with_nestdepth("""
        #include <concepts>
        #include <type_traits>
        #include <utility>
        bool equalPlan(bool online, bool ready, bool cached, bool fresh) {
            if (online && ready == [=] { return cached && fresh; }()) return true;
            return false;
        }
        bool choosePlan(bool online, bool cached, bool fresh) {
            if (online && [=]() -> std::pair<bool, bool> {
                return {cached && fresh, false};
            }().first) return true;
            return false;
        }
        bool constrainedPlan(bool online, bool cached, bool fresh) {
            if (online && [=]<class T>(T ready) requires requires { bool(ready); } {
                return bool(ready) && cached && fresh;
            }(true)) return true;
            return false;
        }
        bool signedPlan(bool online, bool cached, bool fresh) {
            if (online && [=]<class T> requires std::integral<T>
                (T value) requires std::is_signed_v<T> {
                const bool eligible = cached && fresh;
                if (value > 0) return eligible;
                return false;
            }(1)) return true;
            return false;
        }
        """)
        self.assertEqual([('equalPlan', 3), ('choosePlan', 3), ('constrainedPlan', 3),
                          ('signedPlan', 3)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_lambda_constraint_accepts_alternative_operator_spellings(self):
        for operator in ('and', 'or'):
            with self.subTest(operator=operator):
                source = """
                #include <concepts>
                bool eligible(bool online, bool cached, bool fresh) {
                    if (online && [=]<class T>(T value)
                        requires std::integral<T> OP requires { bool(value); } {
                        const bool available = cached && fresh;
                        if (value > 0) return available;
                        return false;
                    }(1)) return true;
                    return false;
                }
                """.replace('OP', operator)
                functions = get_cpp_with_nestdepth(source)
                self.assertEqual([('eligible', 3)],
                                 [(f.name, f.max_nesting_depth) for f in functions])

    def test_cpp_condition_lambda_starts_a_separate_statement_scope(self):
        functions = get_cpp_with_nestdepth("""
        template<class Check> bool verify(Check check) { return check(); }
        bool allowed(bool ready, bool cached, bool fresh) {
            if (ready && [&] {
                bool valid = cached && fresh;
                return valid;
            }()) return true;
            return false;
        }
        bool allowedLater(bool ready, bool cached, bool fresh) {
            if (verify([&] {
                bool valid = cached && fresh;
                if (valid) return true;
                return false;
            }) && ready) return true;
            return false;
        }
        bool permitted(bool ready, bool warm, bool safe, bool cached) {
            return verify([&] {
                if (ready) {
                    if (warm && safe) { return true; }
                }
                if (cached) {
                    if (safe) { return true; }
                }
                return false;
            });
        }
        """)
        self.assertEqual([('verify', 0), ('allowed', 3), ('allowedLater', 2),
                          ('permitted', 3)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_initializer_braces_keep_the_logical_condition(self):
        functions = get_cpp_with_nestdepth("""
        struct Toggle { bool active; };
        bool allowed(bool enabled, bool cached, bool verified) {
            if (Toggle{enabled && cached && verified}.active) return true;
            return false;
        }
        bool permit(bool enabled, bool cached, bool verified) {
            if (Toggle toggle{enabled && cached && verified}; toggle.active) return true;
            return false;
        }
        """)
        self.assertEqual([2, 2], [f.max_nesting_depth for f in functions])

    def test_cpp_expression_block_keeps_condition_levels(self):
        functions = get_cpp_with_nestdepth("""
        bool allowed(bool enabled, bool cached, bool verified) {
            if (enabled && [](bool ready) { return ready; }(cached)) {
                if (verified) return true;
            }
            return false;
        }
        """)
        self.assertEqual(3, functions[0].max_nesting_depth)


class TestJavaNestingDepth(unittest.TestCase):

    def test_java_condition_lambda_starts_a_separate_statement_scope(self):
        functions = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            'Permissions.java', """
        import java.util.function.BooleanSupplier;
        class Permissions {
            static boolean verify(BooleanSupplier check) { return check.getAsBoolean(); }
            static void log(boolean value) {}
            static boolean allowed(boolean ready, boolean cached, boolean fresh) {
                if (ready && verify(() -> {
                    log(cached && fresh);
                    return true;
                })) return true;
                return false;
            }
            static boolean allowedLater(boolean ready, boolean cached, boolean fresh) {
                if (verify(() -> {
                    boolean valid = cached && fresh;
                    if (valid) return true;
                    return false;
                }) && ready) return true;
                return false;
            }
            static boolean permitted(boolean ready, boolean warm, boolean safe, boolean cached) {
                return verify(() -> {
                    if (ready) {
                        if (warm && safe) { return true; }
                    }
                    if (cached) {
                        if (safe) { return true; }
                    }
                    return false;
                });
            }
        }
        """).function_list
        self.assertEqual([('Permissions::verify', 0), ('Permissions::log', 0),
                          ('Permissions::allowed', 3), ('Permissions::allowedLater', 2),
                          ('Permissions::permitted', 3)],
                         [(f.name, f.max_nesting_depth) for f in functions])

    def test_array_initializer_braces_keep_the_logical_condition(self):
        functions = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            'Permissions.java', """
        class Permissions {
            static boolean allowed(boolean enabled, boolean cached, boolean verified) {
                if (new boolean[]{enabled && cached && verified}[0]) return true;
                return false;
            }
        }
        """).function_list
        self.assertEqual(2, functions[0].max_nesting_depth)

    def test_java_expression_block_keeps_condition_levels(self):
        functions = FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
            'Permissions.java', """
        class Permissions {
            static boolean allowed(boolean enabled, boolean verified,
                                   java.util.function.Predicate<String> check) {
                if (enabled && java.util.stream.Stream.of("admin").anyMatch(role -> {
                    return check.test(role);
                })) {
                    if (verified) return true;
                }
                return false;
            }
        }
        """).function_list
        self.assertEqual(3, functions[0].max_nesting_depth)
