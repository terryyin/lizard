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

