import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_method_calls(unittest.TestCase):
    def test_no_false_positive_method_calls(self):
        # Ensure method calls are not detected as functions
        code = '''
        class Widget {
            updateUI() {
                document.getElementById('count').textContent = this.state.clicks;
                const formatted = new Date().toISOString();
                this.helperMethod();
            }

            helperMethod() {
                return 42;
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]

        # Should only detect actual methods
        expected_methods = ['updateUI', 'helperMethod']
        self.assertEqual(sorted(expected_methods), sorted(found_methods),
                        f"Should only detect actual methods, not method calls. Found: {found_methods}")


class Test_JavaScript_no_false_positives(unittest.TestCase):
    """Tests that various non-function patterns don't produce FPs."""

    def test_method_chaining_no_fp(self):
        """Method chaining should not produce FPs"""
        code = '''
        function buildQuery(table) {
            return db.select("*")
                .from(table)
                .where("active", true)
                .orderBy("name");
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["buildQuery"], [f.name for f in functions])

    def test_object_destructuring_no_fp(self):
        """Destructuring assignment should not produce FPs"""
        code = '''
        function getConfig() {
            const { host, port } = config;
            return { host, port };
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["getConfig"], [f.name for f in functions])

    def test_template_literal_no_fp(self):
        """Template literal interpolation should not produce FPs"""
        code = '''
        function greet(name) {
            return `Hello, ${name}! Welcome.`;
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["greet"], [f.name for f in functions])
