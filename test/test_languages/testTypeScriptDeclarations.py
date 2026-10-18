import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_declared_functions(unittest.TestCase):
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


class Test_TypeScript_declaration_boundaries(unittest.TestCase):
    def test_declare_function_skipped(self):
        """declare function should be skipped, real function after it detected"""
        code = '''
        declare function external(): void;
        function local() { return 1; }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["local"], [f.name for f in functions])


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



class Test_TypeScript_interface_enum_no_fp(unittest.TestCase):
    """Tests that interfaces and enums do NOT produce false positives."""

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
