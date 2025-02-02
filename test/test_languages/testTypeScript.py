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
        self.assertEqual(1, len(functions))
        self.assertEqual("warnUser", functions[0].name)

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
            declare function create(o: object | null): void;
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
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_object_method(self):
        functions = get_ts_function_list("""
        const x = {
            test(): number {
                return 1;
            }
        }
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        self.assertEqual("test", functions[0].name)

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
        self.assertEqual(["test"], [f.name for f in functions])
        self.assertEqual(1, functions[0].cyclomatic_complexity)

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

    def test_simple_object_property(self):
        functions = get_ts_function_list("""
        const obj = {
            prop: 42,
            method() {
                return 1;
            }
        }
        """)
        self.assertEqual(["method"], [f.name for f in functions])


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


