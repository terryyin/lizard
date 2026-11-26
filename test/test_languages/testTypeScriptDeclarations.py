import unittest

from lizard import FileAnalyzer, analyze_file, get_extensions
from .testTypeScript import get_ts_function_list


class Test_TypeScript_declared_functions(unittest.TestCase):
    def test_ambient_and_overload_signatures_for_new_methods(self):
        code = '''
declare class ExistingRegistry {
  new(key: string): object;
}

class Registry {
  static new(key: string): Registry;
  static new(key: string, fresh: boolean): Registry;
  static new(key: string, fresh = false): Registry {
    return key || fresh ? new Registry() : new Registry();
  }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual([('new', 9, 4)],
                         [(f.name, f.start_line, f.cyclomatic_complexity) for f in functions])

    def test_generic_field_type_before_a_method_named_new(self):
        code = '''
class Registry {
  cache!: Map<string, number>
  new(ready: boolean) { return ready && this.cache }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual([('new', 2)], [(f.name, f.cyclomatic_complexity) for f in functions])

    def test_type_checked_field_value_before_a_method_named_new(self):
        for operator in ('as', 'satisfies'):
            with self.subTest(operator=operator):
                code = '''
class Registry {
  options = { enabled: true } OPERATOR Record<string, boolean>
  new(key: 'enabled') {
    return this.options[key] ? new Registry() : null;
  }
}
'''.replace('OPERATOR', operator)
                functions = get_ts_function_list(code)
                self.assertEqual(
                    [('new', 4, 2, ['key'])],
                    [(f.name, f.start_line, f.cyclomatic_complexity, f.parameters)
                     for f in functions])

    def test_split_abstract_constructor_assertion_stays_type_only(self):
        code = '''class Rect {
  constructor(readonly width: number) {}
}
abstract class Shape {
  factory = Rect as abstract
    new (width: number) => Rect
  area() { return new Rect(2).width; }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual([('constructor', 2, ['width']), ('area', 7, [])], [
            (f.name, f.start_line, f.parameters) for f in functions
        ])

    def test_abstract_constructor_return_type_stays_with_field_arrow(self):
        code = '''
abstract class Item {}
class Registry {
  factory = (): abstract
    new () => Item => Item;
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual([('factory', 12)], [(f.name, f.token_count) for f in functions])

    def test_abstract_constructor_return_type_stays_with_method(self):
        code = '''
abstract class Item {}
class Registry {
  build(): abstract
    new () => Item { return Item; }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual([('build', 14)], [(f.name, f.token_count) for f in functions])

    def test_new_method_after_a_qualified_type_ending_in_abstract(self):
        code = '''
const defaults = { abstract: new Map<string, string>() };
class Registry {
  cache: typeof defaults.abstract
  new(key: string) { return this.cache.get(key); }
  constructor() { this.cache = defaults.abstract; }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual(
            [('new', 1, 17, ['key']), ('constructor', 1, 12, [])],
            [(f.name, f.cyclomatic_complexity, f.token_count, f.full_parameters)
             for f in functions])

    def test_non_null_field_value_before_a_method_named_new(self):
        code = '''
class Registry {
  cache = new Map<string, string>()!
  new(key: string) { return this.cache.get(key); }
}

class Cache {
  data = new Map<string, string>();
  current = this.data!
  new(key: string) { return this.current.get(key); }
}
'''
        functions = get_ts_function_list(code)
        self.assertEqual(
            [('new', 4, 1, ['key']), ('new', 10, 1, ['key'])],
            [(f.name, f.start_line, f.cyclomatic_complexity, f.full_parameters)
             for f in functions])

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


class Test_TypeScript_template_literal_types_in_declarations(unittest.TestCase):
    """A template literal type's '${' closes with '}' inside skipped
    interface and object-type bodies; later code keeps its functions."""

    def test_functions_after_template_literal_types_in_declarations(self):
        bodies = {
            'optional field': 'interface O { kind?: `x-${string}`; }',
            'required field': 'interface O { kind: `x-${string}`; }',
            'generic argument':
                'interface O { kind?: Extract<string, `x-${string}`>; }',
            'nested generics':
                'interface O { tag: Map<string, Array<`t-${string}`>>; }',
            'nested interpolation': 'interface O { n: `a-${`b-${string}`}`; }',
            'extends clause':
                'interface O extends Array<`e-${string}`> { n: number; }',
            'object type alias': 'type O = { label: `s-${string}` };',
        }
        for case, declaration in bodies.items():
            with self.subTest(case=case):
                functions = get_ts_function_list(
                    declaration + "\n"
                    "function choose(x: boolean) { if (x) return 1; return 0; }")
                self.assertEqual([('choose', 2)],
                                 [(f.name, f.cyclomatic_complexity)
                                  for f in functions])

    def test_ccn_and_cognitive_match_code_without_the_types(self):
        types = (
            "type Prefix = `x-${string}`;\n"
            "interface Options {\n"
            "  kind?: Extract<string, `x-${string}`>;\n"
            "  name: `id-${number}`;\n"
            "  tag: Map<string, Array<`t-${Uppercase<`k${string}`>}`>>;\n"
            "  nested: `a-${`b-${string}`}`;\n"
            "}\n"
            "type Shape = { label: `s-${string}`; size?: number };\n"
            "interface Tagged extends Array<`e-${string}`> { first: Prefix; }\n"
        )
        code = (
            "function choose(x: boolean, o: Options): string {\n"
            "  if (x) return `${o.name ? 'a' : 'b'}`;\n"
            "  return o.kind ?? '';\n"
            "}\n"
            "class Box {\n"
            "  run(s: Shape, t: Tagged): number {\n"
            "    return s.size && t.length ? 1 : 0;\n"
            "  }\n"
            "}\n"
        )
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'outside']))
        for source in (types + code, code):
            with self.subTest(with_types=source != code):
                functions = analyzer.analyze_source_code(
                    'a.ts', source).function_list
                self.assertEqual(
                    [('choose', 4, 3), ('run', 3, 2), ('*global*', 1, 0)],
                    [(f.name, f.cyclomatic_complexity,
                      f.cognitive_complexity) for f in functions])


class Test_TypeScript_bodyless_signatures(unittest.TestCase):
    def test_overloads_report_only_the_implementation(self):
        code = """function last<T>(array: readonly T[]): T | undefined;
function last<T>(array: readonly T[], fallback: T): T;
function last<T>(array: readonly T[], fallback?: T): T | undefined {
    return array.length ? array[array.length - 1] : fallback;
}
"""
        for suffix in ('ts', 'tsx'):
            with self.subTest(suffix=suffix):
                functions = analyze_file.analyze_source_code('example.' + suffix, code).function_list
                self.assertEqual([('last', 3, 5, 2, 2)], [
                    (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
                    for f in functions])

    def test_semicolonless_method_overloads_keep_only_body_parameters(self):
        code = """class Registry {
  get(key: string): string | undefined
  get(key: string, defaultValue: string): string
  get(key: string, defaultValue?: string): string | undefined
  {
    return this.entries.get(key) ?? defaultValue
  }

  constructor(private readonly entries: Map<string, string>) {}
}
"""
        method, constructor = get_ts_function_list(code)
        self.assertEqual(('get', 4, 7, ['key', 'defaultValue']),
                         (method.name, method.start_line, method.end_line, method.parameters))
        self.assertEqual('constructor', constructor.name)

    def test_semicolonless_overload_before_computed_method(self):
        code = """const read = Symbol();
class Registry {
  [read](key: string): string
  [read](key: string, fallback?: string): string {
    return key ? key : fallback ?? "";
  }
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('read', 4, 6, 2, 3)], [
            (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
            for f in functions])

    def test_semicolonless_overload_before_keyword_method(self):
        code = """class Filter {
  %s(value: string): boolean
  %s(value: string, expected?: string): boolean {
    return expected ? value === expected : Boolean(value);
  }
}
"""
        for name in ('is', 'as', 'in', 'extends'):
            with self.subTest(name=name):
                functions = get_ts_function_list(code % (name, name))
                self.assertEqual([(name, 3, 5, 2, 2)], [
                    (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
                    for f in functions])

    def test_assertion_method_can_wrap_before_is(self):
        code = """class Validator {
  check(%(value)s: unknown): asserts %(value)s
    is string {
      if (typeof %(value)s !== "string") throw new TypeError();
    }
}
"""
        for name in ('value', r'\u0076alue'):
            with self.subTest(name=name):
                functions = get_ts_function_list(code % {'value': name})
                self.assertEqual([('check', 2, 5, 2)], [
                    (f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions])

    def test_overload_return_type_can_wrap_before_infer_constraint(self):
        code = """class Registry {
  read(value: string): string extends infer U
    extends string ? U : never
  read(value: unknown, fallback?: string): string extends infer U
    extends string ? U : never {
    return typeof value === "string" ? value : fallback ?? "";
  }
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('read', 4, 7, 2, 4)], [
            (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
            for f in functions])

    def test_type_query_can_end_with_a_keyword_property(self):
        code = """const predicates = { %(key)s: (value: unknown) => typeof value === "string" };

function getPredicate(): typeof predicates.%(key)s {
  return predicates.%(key)s;
}
"""
        for key in ('is', 'in', 'instanceof'):
            with self.subTest(key=key):
                functions = get_ts_function_list(code % {'key': key})
                self.assertEqual([(key, 1, 1), ('getPredicate', 3, 5)],
                                 [(f.name, f.start_line, f.end_line) for f in functions])

    def test_assertion_method_accepts_as_as_the_parameter(self):
        code = """class Validator {
  check(as: unknown): asserts as {
    if (!as) throw new TypeError();
  }
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('check', 2, 4, 2)],
                         [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions])

    def test_overload_before_an_escaped_method_name(self):
        code = r"""class Registry {
  \u0072ead(key: string): string
  \u0072ead(key: string, fallback?: string): string {
    return key ? key : fallback ?? "";
  }
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([(3, 5, 2, 3)], [
            (f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity) for f in functions])

    def test_overload_before_a_unicode_method_name(self):
        code = """class Registry {
  ゛get(value: unknown): boolean
  ゛get(value: unknown, message?: string): boolean {
    return message ? Boolean(value) : false;
  }
}
const keep = () => true;
"""
        for suffix in ('ts', 'tsx'):
            with self.subTest(suffix=suffix):
                functions = analyze_file.analyze_source_code('example.' + suffix, code).function_list
                self.assertEqual([(3, 5, 2, 2), (7, 7, 0, 1)], [
                    (f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
                    for f in functions])

    def test_predicate_can_return_a_type_named_is(self):
        code = """type is = string;
function check(value: unknown): value is is {
  return typeof value === "string";
}
const is = "ready";
function getState(): typeof is {
  return is;
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('check', 2, 4), ('getState', 6, 8)],
                         [(f.name, f.start_line, f.end_line) for f in functions])

    def test_type_predicate_can_name_a_parameter_with_a_contextual_keyword(self):
        code = """function matches(%(key)s: unknown): %(key)s is { value: string } {
  return %(key)s !== null && typeof %(key)s === "object" && "value" in %(key)s;
}
class Validator {
  check(%(key)s: unknown): %(key)s is { value: string };
  check(%(key)s: unknown, label?: string): %(key)s is { value: string } {
    return %(key)s !== null && typeof %(key)s === "object" && "value" in %(key)s;
  }
}
"""
        for key in ('asserts', 'keyof', 'infer', 'readonly', 'unique'):
            with self.subTest(key=key):
                functions = get_ts_function_list(code % {'key': key})
                self.assertEqual([('matches', 1, 3, 1, 3), ('check', 6, 8, 2, 3)], [
                    (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
                    for f in functions])

    def test_return_predicates_in_runtime_arrows_and_object_methods(self):
        code = """const matches = (%(key)s: unknown): %(key)s is { value: string } => {
  return %(key)s !== null && typeof %(key)s === "object" && "value" in %(key)s;
};
const validator = {
  check(%(key)s: unknown): %(key)s is { value: string } {
    return %(key)s !== null && typeof %(key)s === "object" && "value" in %(key)s;
  }
};
"""
        for key in ('asserts', 'keyof', 'infer', 'readonly', 'unique'):
            for filename in ('guards.ts', 'guards.tsx'):
                with self.subTest(key=key, filename=filename):
                    functions = analyze_file.analyze_source_code(filename, code % {'key': key}).function_list
                    self.assertEqual([('matches', 1, 3, 1, 3), ('check', 5, 7, 1, 3)], [
                        (f.name, f.start_line, f.end_line, f.parameter_count, f.cyclomatic_complexity)
                        for f in functions])

    def test_bodyless_declaration_at_end_of_file_is_not_a_function(self):
        code = """export function decode(input: string): Uint8Array
"""
        result = analyze_file.analyze_source_code('example.d.ts', code)
        self.assertEqual([], result.function_list)

    def test_overload_return_type_does_not_add_a_decision_to_the_parent(self):
        code = """function converter(value: string | number) {
  function read<T extends string | number>(input: T): T extends string ? string : number;
  function read(input: string | number): string | number {
    return input;
  }
  return read(value);
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('read', 1), ('converter', 1)],
                         [(f.name, f.cyclomatic_complexity) for f in functions])

    def test_typed_arrow_returns_an_array_in_a_namespace(self):
        code = """namespace Completion {
  export const names = (plus: readonly string[], options?: { noLib?: boolean }): readonly string[] =>
    [
      ...plus,
      ...options?.noLib ? [] : ['name'],
    ].sort();
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('names', 2, 6)], [(f.name, f.start_line, f.end_line) for f in functions])

    def test_computed_method_after_bodyless_signature(self):
        code = """abstract class Cursor {
  return?(): void;
  [Symbol.iterator]() {
    return this;
  }
}
"""
        functions = get_ts_function_list(code)
        self.assertEqual([('symbol.iterator', 3, 5)], [
            (f.name, f.start_line, f.end_line) for f in functions])
