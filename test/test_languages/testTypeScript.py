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

