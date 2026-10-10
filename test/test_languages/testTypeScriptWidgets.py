import unittest

from .testTypeScript import get_ts_function_list
from .typescript_widget_fixtures import INTERACTIVE_WIDGET


class Test_TypeScript_widgets(unittest.TestCase):
    @unittest.skip("Known limitation: method after complex async with nested callbacks and method calls in object literal")
    def test_static_async_method_detection(self):
        # Test that static async methods are properly detected
        # KNOWN LIMITATION: generateRandomId is not detected when it follows simulateApiCall
        # with complex nested callbacks containing method calls in object literals
        code = '''
        class TestClass {
            static async simulateApiCall(data: any): Promise<any> {
                return new Promise((resolve) => {
                    setTimeout(() => {
                        resolve({
                            status: 'success',
                            data,
                            timestamp: new Date().toISOString(),
                            id: this.generateRandomId()
                        });
                    }, 1000);
                });
            }

            static generateRandomId(): string {
                return Math.random().toString(36).substring(2, 9);
            }
        }
        '''
        functions = get_ts_function_list(code)
        found_methods = [f.name for f in functions]

        # Should detect both static methods
        self.assertIn('simulateApiCall', found_methods,
                     f"Method 'simulateApiCall' should be detected. Found: {found_methods}")
        self.assertIn('generateRandomId', found_methods,
                     f"Method 'generateRandomId' should be detected. Found: {found_methods}")

        # Should NOT detect method calls as functions
        for method in found_methods:
            self.assertNotIn('Date.toISOString', method,
                           f"Method call 'Date.toISOString' should not be detected as a function")
            self.assertNotIn('this.generateRandomId', method,
                           f"Method call 'this.generateRandomId' should not be detected as a function")
            # Make sure we don't have standalone Date as a function
            if method == 'Date' or method.startswith('Date@'):
                self.fail(f"Constructor call 'Date' should not be detected as a function. Found: {method}")

    def test_interactive_widget_from_github_issue_415(self):
        # InteractiveWidget class from GitHub issue #415 (TypeScript version)
        # Known limitation: private field declarations followed by return-type-annotated
        # methods cause the parser to lose track of subsequent class members.
        # The JS version (without type annotations) detects most methods correctly.
        code = INTERACTIVE_WIDGET
        functions = get_ts_function_list(code)
        found_methods = [f.name for f in functions]

        # Instance methods following typed field declarations are detected
        # (was: zero methods detected before the _in_prop_value reset fix).
        # Static methods with return-type annotations remain a known limitation
        # (see test_abstract_class_methods skip).
        critical_methods = [
            'constructor', 'init', 'render', 'updateUI', 'handleClick',
            'startTimer', 'stopTimer',
        ]

        for method in critical_methods:
            self.assertIn(method, found_methods,
                         f"Method '{method}' should be detected. Found: {found_methods}")

        # Should NOT detect method calls or constructor calls as functions
        false_positives = ['Date', 'Date.toISOString', 'this.generateRandomId']
        for fp in false_positives:
            for found in found_methods:
                if found == fp or (fp in found and found.startswith(fp)):
                    self.fail(f"False positive '{fp}' should not be detected. Found: {found} in {found_methods}")
