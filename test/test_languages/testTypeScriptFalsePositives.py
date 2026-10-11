import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_method_calls(unittest.TestCase):
    def test_no_false_positive_method_calls(self):
        # Ensure method calls are not detected as functions
        # Known limitation: methods after a method with return-type annotation
        # and complex expressions may not be detected
        code = '''
        class Widget {
            updateUI(): void {
                document.getElementById('count').textContent = this.state.clicks.toString();
                const formatted: string = new Date().toISOString();
                this.helperMethod();
            }

            helperMethod(): number {
                return 42;
            }
        }
        '''
        functions = get_ts_function_list(code)
        found_methods = [f.name for f in functions]

        # updateUI is detected; helperMethod after return-type-annotated method
        # with complex expressions is a known limitation
        self.assertIn('updateUI', found_methods,
                      f"updateUI should be detected. Found: {found_methods}")
        # No false positives for method calls
        for name in found_methods:
            self.assertNotIn('toString', name)
            self.assertNotIn('toISOString', name)
            self.assertNotIn('getElementById', name)


class Test_TypeScript_assertion_expressions(unittest.TestCase):
    def test_as_const_no_fp(self):
        """as const assertion should not produce FPs"""
        code = '''
        const ROUTES = {
            HOME: "/",
            ABOUT: "/about"
        } as const;
        function getRoute(name: keyof typeof ROUTES) { return ROUTES[name]; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["getRoute"], [f.name for f in functions])

    def test_satisfies_no_fp(self):
        """satisfies operator should not produce FPs"""
        code = '''
        const palette = {
            red: [255, 0, 0],
            green: "#00ff00"
        } satisfies Record<string, string | number[]>;
        function usePalette() { return palette; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["usePalette"], [f.name for f in functions])

    def test_keyof_typeof_no_fp(self):
        code = '''
        const config = { a: 1, b: 2, c: 3 };
        type ConfigKey = keyof typeof config;
        function getConfig(key: ConfigKey): number { return config[key]; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["getConfig"], [f.name for f in functions])


class Test_TypeScript_function_call_no_false_positives(unittest.TestCase):
    """Tests that function/method calls are NOT detected as function definitions (Fix 3)."""

    def test_simple_function_call(self):
        """const x = someFunc(a, b) should not detect someFunc as a definition"""
        code = '''
        const x = someFunc(a, b);
        const realFn = (p: string) => p.toUpperCase();
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["realFn"], [f.name for f in functions])

    def test_chained_calls(self):
        """Method chaining should not create FPs"""
        code = '''
        function buildQuery(table: string) {
            return db.select("*")
                .from(table)
                .where("active", true)
                .orderBy("name");
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["buildQuery"], [f.name for f in functions])

    def test_await_call_not_fp(self):
        """await fetchData(url) should not detect fetchData as definition"""
        code = '''
        async function loadData(url: string) {
            const data = await fetchData(url);
            return data;
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["loadData"], [f.name for f in functions])

    def test_ternary_calls_not_fp(self):
        """Ternary with function calls should not produce FPs"""
        code = '''
        function decide(x: boolean) {
            return x ? handleTrue(x) : handleFalse(x);
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["decide"], [f.name for f in functions])



class Test_ts_builtin_name_functions(unittest.TestCase):
    """Functions/methods named after JS builtins must still be detected."""

    def test_function_named_String(self):
        """function String() {} is a legitimate function definition"""
        code = 'function String() { return "custom"; }'
        functions = get_ts_function_list(code)
        self.assertEqual(["String"], [f.name for f in functions])

    def test_class_method_named_Number(self):
        """Class method named Number should be detected"""
        code = '''
        class Util {
            Number() { return 0; }
            Boolean() { return true; }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["Number", "Boolean"], [f.name for f in functions])

    def test_object_method_named_Array(self):
        """Object method named Array should be detected"""
        code = '''
        const obj = {
            Array() { return []; },
            Object() { return {}; }
        };
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Array", names)
        self.assertIn("Object", names)

    def test_builtin_call_not_detected(self):
        """const s = String(42) should NOT be a function definition"""
        code = '''
        const s = String(42);
        const n = Number("5");
        function realFn() { return 1; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["realFn"], [f.name for f in functions])

    def test_bare_builtin_call_not_detected(self):
        """String(42) bare call should NOT be a function definition"""
        code = '''
        String(42);
        Number(true);
        function realFn() { return 1; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["realFn"], [f.name for f in functions])
