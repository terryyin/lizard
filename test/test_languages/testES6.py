import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import JavaScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_parser_for_JavaScript_ES6(unittest.TestCase):

    def test_simple_function(self):
        functions = get_js_function_list("x=>x")
        self.assertEqual("(anonymous)", functions[0].name)

    def test_two_functions(self):
        functions = get_js_function_list("""
            x=>x
            x=>x
        """)
        self.assertEqual(2, len(functions))

    def test_two_functions_with_semicolon(self):
        functions = get_js_function_list("""x=>x; x=>x;""")
        self.assertEqual(2, len(functions))

    def test_function_with_block(self):
        functions = get_js_function_list("""
            x=>{return 0;}
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual("(anonymous)", functions[0].name)

    def test_complexity(self):
        functions = get_js_function_list("""
            x=>a && b
        """)
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_nested(self):
        functions = get_js_function_list("""
            function a(){x=>a;}
        """)
        self.assertEqual(2, len(functions))
        self.assertEqual('a', functions[1].name)

    def test_nested2(self):
        functions = get_js_function_list("""
            function a(){m.map(x=>a) && b}
        """)
        self.assertEqual('(anonymous)', functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)
        self.assertEqual(2, functions[1].cyclomatic_complexity)

    def test_nested3(self):
        functions = get_js_function_list("""
            function a(){x=>a}
        """)
        self.assertEqual(2, len(functions))
        self.assertEqual('a', functions[1].name)

    def test_nested_complexity(self):
        functions = get_js_function_list("""
            x=>{
                a&&b;
                b&&c;
                }
        """)
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_arraw_function_name(self):
        functions = get_js_function_list("""
            const x=a=>1
        """)
        self.assertEqual('x', functions[0].name)

    def xtest_arraw_function_with_multiple_param(self):
        functions = get_js_function_list("""
            const x=(a, b=3)=>1
        """)
        self.assertEqual('x', functions[0].name)

