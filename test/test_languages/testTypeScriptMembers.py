import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_members(unittest.TestCase):
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


class Test_TypeScript_class_members(unittest.TestCase):
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


class Test_ts_abstract_method_detection(unittest.TestCase):
    """Abstract methods should not be detected as functions."""

    def test_abstract_method_skipped(self):
        """abstract doWork(): void should not be detected"""
        code = '''
        abstract class Base {
            abstract doWork(): void;
            concrete() { return 1; }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["concrete"], [f.name for f in functions])

    def test_abstract_with_params_skipped(self):
        """abstract method with params should not be detected"""
        code = '''
        abstract class Service {
            abstract fetch(url: string, options: object): Promise<Response>;
            abstract parse(data: string): object;
            process() { return this.fetch("").then(r => this.parse(r)); }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("process", names)
        self.assertNotIn("fetch", names)
        self.assertNotIn("parse", names)

    def test_abstract_class_not_abstract_function(self):
        """abstract class declaration should not affect non-abstract methods"""
        code = '''
        abstract class Widget {
            render() { return null; }
            update() { this.render(); }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["render", "update"], [f.name for f in functions])
