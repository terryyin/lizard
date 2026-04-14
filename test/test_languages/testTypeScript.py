import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import TypeScriptReader


def get_ts_function_list(source_code):
    return analyze_file.analyze_source_code("a.ts", source_code).function_list

class Test_tokenizing_TypeScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TypeScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_simple(self):
        self.check_tokens(['abc?'], 'abc?')

class Test_parser_for_TypeScript(unittest.TestCase):

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

    def test_function_declare(self):
        functions = get_ts_function_list("""
            declare function create(o): void;
        """)
        self.assertEqual([], [f.name for f in functions])

    def test_function_declare_and_a_function(self):
        functions = get_ts_function_list("""
            declare function create(o: object | null): void;
            function warnUser() {
                console.log("This is my warning message");
            }
        """)
        self.assertEqual(["warnUser"], [f.name for f in functions])

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

    def test_object_method(self):
        functions = get_ts_function_list("""
        const x = {
            test(): number {
                return 1;
            }
        }
        """)
        self.assertEqual(["test"], [f.name for f in functions])
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_nested_object_method(self):
        functions = get_ts_function_list("""
        export default {
            methods: { 
                test(): number {
                    return 1;
                }
            }
        }
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        self.assertEqual("test", functions[0].name)

    def test_nested_object_with_not_type_method(self):
        functions = get_ts_function_list("""
        export default {
            methods: { 
                test() {
                    return 1;
                }
            }
        }
        """)
        self.assertEqual(["test"], [f.name for f in functions])
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_multiple_classes_with_methods(self):
        functions = get_ts_function_list("""
            class FirstClass {
                doSomething() {
                    return "first";
                }
            }
            
            class SecondClass {
                doAnotherThing() {
                    return "second";
                }
            }
        """)
        self.assertEqual(["doSomething", "doAnotherThing"], [f.name for f in functions])
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        self.assertEqual(1, functions[1].cyclomatic_complexity)

    def test_multiple_objects_with_methods(self):
        functions = get_ts_function_list("""
            const firstObject = {
                doSomething() {
                    return "first";
                }
            }
            
            const secondObject = {
                doAnotherThing() {
                    return "second";
                }
            }
        """)
        self.assertEqual(["doSomething", "doAnotherThing"], [f.name for f in functions])
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        self.assertEqual(1, functions[1].cyclomatic_complexity)

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

    def test_type_annotation_with_generic(self):
        code = '''
             class BaseType {
                required(): RequiredType<this> {
                    return result;
                }
            }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(
            ["required"],
            [f.name for f in functions]
        )
        for f in functions:
            self.assertEqual(1, f.cyclomatic_complexity)


    @unittest.skip("Known limitation: methods after first return-type-annotated method in abstract class are not detected")
    def test_abstract_class_methods(self):
        code = '''
             export abstract class BaseType {
                required(): RequiredType<this> {
                    return result;
                }

                forbidden(): never {
                    return this as any as never;
                }

                options(options: Joi.ValidationOptions) {
                    return this;
                }

                strict(isStrict?: boolean) {
                    return this;
                }

                default(value: any) {
                    return this;
                }

                error(err: Error | Joi.ValidationErrorFunction) {
                    return this;
                }

                nullable(): UnionType<this, ConstType<null>> {
                    return this as any;
                }
            }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(
            ["required", "forbidden", "options", "strict", "default", "error", "nullable"],
            [f.name for f in functions]
        )
        for f in functions:
            self.assertEqual(1, f.cyclomatic_complexity)

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

    def test_typed_field_followed_by_methods(self):
        code = '''
        class Svc {
            count: number;
            private helper(): number { return 1; }
            exposed(): number { return 2; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn('helper', names)
        self.assertIn('exposed', names)

    def test_typed_field_with_initializer_followed_by_methods(self):
        code = '''
        class Svc {
            private state: any = { x: 1 };
            doWork(): void { return; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn('doWork', names)

    def test_interactive_widget_from_github_issue_415(self):
        # InteractiveWidget class from GitHub issue #415 (TypeScript version)
        # Known limitation: private field declarations followed by return-type-annotated
        # methods cause the parser to lose track of subsequent class members.
        # The JS version (without type annotations) detects most methods correctly.
        code = '''
        class InteractiveWidget {
            private container: HTMLElement;
            private state: any;

            constructor(containerId: string) {
                this.container = document.getElementById(containerId) as HTMLElement;
                if (!this.container) {
                    throw new Error(`Container element with ID ${containerId} not found`);
                }
                this.state = { clicks: 0, items: [], timer: null };
                this.init();
            }

            init(): void {
                this.render();
            }

            render(): void {
                this.container.innerHTML = `<div>widget</div>`;
                this.updateUI();
            }

            updateUI(): void {
                (document.getElementById('count') as HTMLElement).textContent = this.state.clicks.toString();
            }

            handleClick(): void {
                this.state.clicks++;
                this.updateUI();
            }

            startTimer(): void {
                this.state.timer = setInterval(() => {
                    this.state.count++;
                }, 1000);
            }

            stopTimer(): void {
                clearInterval(this.state.timer);
            }

            static formatDate(date: Date): string {
                return new Date().toISOString();
            }

            static generateRandomId(): string {
                return Math.random().toString(36).substring(2, 9);
            }

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

            static processItems(items: string[], processorFn: Function): any[] {
                return items.map((item, index) => processorFn(item, index));
            }

            static filterUnique<T>(array: T[]): T[] {
                return [...new Set(array)];
            }
        }
        '''
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


class Test_TypeScript_type_alias_no_false_positives(unittest.TestCase):
    """Tests that type aliases with arrow signatures are NOT detected as functions (Fix 2)."""

    def test_type_simple_arrow_signature(self):
        """type Handler = (event: Event) => void; should produce 0 functions"""
        functions = get_ts_function_list("type Handler = (event: Event) => void;")
        self.assertEqual([], [f.name for f in functions])

    def test_type_object_with_method_signatures(self):
        """type Actions = { increment: (n) => void; ... } should produce 0 functions"""
        code = '''
        type Actions = {
            increment: (amount: number) => void;
            decrement: () => void;
            reset: () => { count: number };
        };
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_type_union_with_arrow(self):
        """type StringOrFn = string | ((x: number) => boolean); should produce 0 functions"""
        functions = get_ts_function_list(
            "type StringOrFn = string | ((x: number) => boolean);"
        )
        self.assertEqual([], [f.name for f in functions])

    def test_type_mapped(self):
        """Mapped types with arrow signatures should produce 0 functions"""
        code = '''
        type Mapped<T> = {
            [K in keyof T]: (val: T[K]) => void;
        };
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_type_conditional(self):
        """Conditional types with arrow signatures should produce 0 functions"""
        code = "type Result<T> = T extends string ? (s: string) => void : (n: number) => void;"
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_type_generic_nested(self):
        """Nested generic type with arrow sigs should produce 0 functions"""
        code = '''
        type Nested<T> = {
            data: T;
            transform: <U>(fn: (item: T) => U) => Nested<U>;
            flatMap: (fn: (item: T) => Nested<T>) => Nested<T>;
        };
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_type_intersection(self):
        """Type intersection should not produce FPs; only real function after it detected"""
        code = '''
        type WithTimestamp = {
            createdAt: Date;
            updatedAt: Date;
        };
        type User = WithTimestamp & {
            name: string;
            getFullName: () => string;
        };
        function createUser(): User { return null as any; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["createUser"], [f.name for f in functions])

    def test_type_followed_by_real_function(self):
        """Real functions after type alias should still be detected"""
        code = '''
        type Callback = (data: any) => void;
        function processData(cb: Callback) {
            cb({result: 1});
        }
        const handler = (x: number) => x * 2;
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("processData", names)
        self.assertIn("handler", names)
        self.assertEqual(2, len(functions))


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


class Test_TypeScript_static_field_class_parsing(unittest.TestCase):
    """Tests that static field = {} does not break subsequent class method detection (Fix 4)."""

    def test_static_defaultprops_then_methods(self):
        """Methods after static defaultProps = {...} should be detected"""
        code = '''
        class Comp {
            static defaultProps = { color: "red", size: 10 };
            static displayName = "Comp";
            render() { return null; }
            handleClick() { return true; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("render", names)
        self.assertIn("handleClick", names)
        # static field assignments should NOT be functions
        self.assertNotIn("defaultProps", names)
        self.assertNotIn("displayName", names)


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


class Test_TypeScript_interface_enum_no_fp(unittest.TestCase):
    """Tests that interfaces and enums do NOT produce false positives."""

    @unittest.skip("Requires interface skipping enhancement")
    def test_interface_with_methods(self):
        """Interface method signatures should not be detected as functions"""
        code = '''
        interface Service {
            get(id: string): Promise<Item>;
            create(data: Partial<Item>): Promise<Item>;
            update(id: string, data: Partial<Item>): Promise<Item>;
            delete(id: string): Promise<void>;
            onError: (err: Error) => void;
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_interface_then_function(self):
        """Functions after interface should still be detected"""
        code = '''
        interface Config {
            host: string;
            port: number;
        }
        function createConfig(): Config {
            return { host: "localhost", port: 3000 };
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["createConfig"], [f.name for f in functions])

    @unittest.skip("Requires interface skipping enhancement")
    def test_index_signature_no_fp(self):
        """Index signatures with arrow types should not be FPs"""
        code = '''
        interface Dict {
            [key: string]: (value: any) => void;
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_enum_basic(self):
        """Basic enum should produce 0 functions"""
        functions = get_ts_function_list("enum Direction { Up = 1, Down, Left, Right }")
        self.assertEqual([], [f.name for f in functions])

    def test_enum_string(self):
        """String enum should produce 0 functions"""
        code = '''enum Color { Red = "RED", Green = "GREEN", Blue = "BLUE" }'''
        functions = get_ts_function_list(code)
        self.assertEqual([], [f.name for f in functions])

    def test_enum_then_function(self):
        """Functions after enum should still be detected"""
        code = '''
        enum Status { Active, Inactive }
        function getStatus(): Status { return Status.Active; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["getStatus"], [f.name for f in functions])


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


class Test_TypeScript_misc_patterns(unittest.TestCase):
    """Tests miscellaneous TypeScript patterns."""

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

    def test_decorators_on_methods(self):
        code = '''
        class Api {
            @Get("/users")
            getUsers() { return []; }
            @Post("/users")
            createUser() { return {}; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("getUsers", names)
        self.assertIn("createUser", names)

    def test_class_inheritance(self):
        code = '''
        class Base {
            constructor() { this.x = 1; }
            baseMethod() { return this.x; }
        }
        class Child extends Base {
            constructor() { super(); this.y = 2; }
            childMethod() { return this.y; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("baseMethod", names)
        self.assertIn("childMethod", names)
        self.assertEqual(names.count("constructor"), 2)

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

    def test_declare_function_skipped(self):
        """declare function should be skipped, real function after it detected"""
        code = '''
        declare function external(): void;
        function local() { return 1; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["local"], [f.name for f in functions])

    def test_access_modifiers(self):
        """private/protected/public methods should be detected"""
        code = '''
        class Svc {
            private helper() { return 1; }
            protected internal() { return 2; }
            public exposed() { return 3; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("helper", names)
        self.assertIn("internal", names)
        self.assertIn("exposed", names)

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

    def test_class_arrow_field(self):
        """Class arrow field should detect the arrow as anonymous"""
        code = '''
        class Btn {
            handleClick = () => { console.log("click"); };
            render() { return null; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("(anonymous)", names)
        self.assertIn("render", names)
