import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_functions(unittest.TestCase):
    def test_simple_function(self):
        functions = get_ts_function_list("""
            function warnUser(): void {
                console.log("This is my warning message");
            }
        """)
        self.assertEqual(["warnUser"], [f.name for f in functions])

    def test_simple_function_with_no_return_type(self):
        functions = get_ts_function_list("""
            function warnUser() {
                console.log("This is my warning message");
            }
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual("warnUser", functions[0].name)

    def test_function_with_default(self):
        functions = get_ts_function_list("""
        function x(config: X): {color: string; area: number} {
            if (config.color) {
                newSquare.color = config.color;
            }
        }
        """)
        self.assertEqual(["x"], [f.name for f in functions])
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_multiple_functions(self):
        code = '''
            function helper1() {
                return 1;
            }
            export default {
                methods: {
                    method1() {
                        return helper1();
                    },
                    method2() {
                        if (true) {
                            return 2;
                        }
                        return 3;
                    }
                }
            }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["helper1", "method1", "method2"], [f.name for f in functions])
        self.assertEqual(2, functions[2].cyclomatic_complexity)

    def test_anonymous_arrow_function_with_type_annotation(self):
        code = 'fun((x: number) => x * 2)'
        functions = get_ts_function_list(code)
        expected_methods = [ '(anonymous)' ]
        self.assertEqual(sorted(expected_methods), sorted([f.name for f in functions]))

    def test_arrow_function_with_type_annotation(self):
        code = '''
        fun((x: number) => x * 2);
        const jsfun = (x) => x * 2;
        const tsfun = (x: number) => x * 2;
'''
        functions = get_ts_function_list(code)
        expected_methods = [
            '(anonymous)',
            'jsfun',
            'tsfun',
        ]
        self.assertEqual(sorted(expected_methods), sorted([f.name for f in functions]))

    def test_plain_type_annotation(self):
        code = '''
          const MyComponent: React.FC = () => {
            return "hello";
          }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual("MyComponent", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)


class Test_TypeScript_abandoned_arrow_forgive(unittest.TestCase):
    """Tests that abandoned arrow function attempts are cleaned up (Fix 5)."""

    def test_call_then_real_arrow(self):
        """Function call followed by real arrow should detect only the real arrow"""
        code = '''
        const x = someFunc(a, b);
        const y = otherFunc(c);
        const realFn = (p: string) => p.toUpperCase();
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["realFn"], [f.name for f in functions])

    def test_multiple_calls_then_function(self):
        """Multiple calls should not corrupt the function stack"""
        code = '''
        queryClient.invalidateQueries(["key"]);
        const data = fetchData(url);
        function processResult(data: any) {
            return data.items;
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["processResult"], [f.name for f in functions])



class Test_TypeScript_generic_arrow_functions(unittest.TestCase):
    """Tests that generic arrow functions are correctly detected."""

    def test_generic_arrow_extends(self):
        """const fn = <T extends Foo>(x: T) => x should detect fn"""
        code = "const fn = <T extends Foo>(x: T) => x;"
        functions = get_ts_function_list(code)
        self.assertEqual(["fn"], [f.name for f in functions])

    def test_generic_arrow_simple(self):
        """const identity = <T>(x: T): T => x should detect identity"""
        code = "const identity = <T>(x: T): T => x;"
        functions = get_ts_function_list(code)
        self.assertEqual(["identity"], [f.name for f in functions])

    def test_generic_function_declaration(self):
        """function first<T>(list: T[]) {} keeps its parameters after <T>"""
        code = '''
            export function first<T>(list: T[]) {
                return list[0];
            }
            function pair<T, U extends object>(a: T, b: U): T {
                return a;
            }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([("first", 2, 4, ["list"]), ("pair", 5, 7, ["a", "b"])],
                         [(f.name, f.start_line, f.end_line, f.parameters)
                          for f in functions])



class Test_TypeScript_export_patterns(unittest.TestCase):
    """Tests export function/const/default patterns."""

    def test_export_function(self):
        functions = get_ts_function_list("export function foo() { return 1; }")
        self.assertEqual(["foo"], [f.name for f in functions])

    def test_export_const_arrow(self):
        functions = get_ts_function_list("export const bar = () => 2;")
        self.assertEqual(["bar"], [f.name for f in functions])

    def test_export_default_function(self):
        functions = get_ts_function_list("export default function baz() { return 3; }")
        self.assertEqual(["baz"], [f.name for f in functions])

    def test_multiple_exports(self):
        code = '''
        export function foo() { return 1; }
        export const bar = () => 2;
        export default function baz() { return 3; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["foo", "bar", "baz"], [f.name for f in functions])



class Test_TypeScript_async_patterns(unittest.TestCase):
    """Tests async function/arrow patterns."""

    def test_async_arrow_with_types(self):
        code = '''
        const fetchData = async (url: string): Promise<Response> => {
            const res = await fetch(url);
            if (!res.ok) throw new Error("fail");
            return res;
        };
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["fetchData"], [f.name for f in functions])
        self.assertGreater(functions[0].cyclomatic_complexity, 1)

    def test_async_function_declaration(self):
        code = '''
        async function loadUser(id: string): Promise<User> {
            const user = await db.findOne(id);
            if (!user) throw new Error("not found");
            return user;
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["loadUser"], [f.name for f in functions])



class Test_TypeScript_function_scopes(unittest.TestCase):
    def test_namespace_functions(self):
        code = '''
        namespace Utils {
            export function helper() { return 1; }
            export const calc = (x: number) => x * 2;
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("helper", names)

    def test_iife(self):
        """Immediately invoked function expressions should be detected"""
        code = '''
        (function() { console.log("init"); })();
        (() => { console.log("init2"); })();
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(2, len(functions))
        for f in functions:
            self.assertEqual("(anonymous)", f.name)

    def test_try_catch_async(self):
        code = '''
        async function safeFetch(url: string) {
            try {
                const res = await fetch(url);
                return await res.json();
            } catch (err) {
                console.error(err);
                return null;
            }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["safeFetch"], [f.name for f in functions])


class Test_ts_expression_bodied_arrows(unittest.TestCase):

    def spans(self, code):
        return [(f.name, f.start_line, f.end_line)
                for f in get_ts_function_list(code)]

    def test_arrow_starts_at_its_arrow_when_the_body_is_on_the_next_line(self):
        code = (
            "function h(xs) {\n"
            "  return xs.map((x) =>\n"
            "    x + 1);\n"
            "}\n"
        )
        self.assertEqual([('(anonymous)', 2, 3), ('h', 1, 4)], self.spans(code))

    def test_parenthesized_function_expression_is_not_a_parameter_list(self):
        code = (
            "const m = (function () {\n"
            "  function inner() { return 1; }\n"
            "  return inner;\n"
            "})();\n"
            "function after() {}\n"
        )
        # The IIFE keeps the binding name `m`, matching other grouped
        # function expressions; `after` must still be a separate function.
        self.assertEqual(['inner', 'm', 'after'],
                         [name for name, _, _ in self.spans(code)])

    def test_expression_body_keeps_internal_commas_and_ternaries(self):
        code = "const f = () => [1, x ? 2 : 3];\n"
        functions = get_ts_function_list(code)
        self.assertEqual([('f', 1, 1)],
                         [(f.name, f.start_line, f.end_line) for f in functions])
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_return_type_arrow_does_not_end_the_function_early(self):
        code = (
            "function f(): () => [number, number] {\n"
            "  return () => [1, 2];\n"
            "}\n"
        )
        self.assertEqual([('(anonymous)', 2, 2), ('f', 1, 3)], self.spans(code))

    def test_optional_index_keeps_nested_callback(self):
        code = (
            "class C {\n"
            "  value = callbacks?.[items.findIndex(item => item.ok)];\n"
            "  method() {}\n"
            "}\n"
        )
        self.assertEqual(['(anonymous)', 'method'],
                         [name for name, _, _ in self.spans(code)])
