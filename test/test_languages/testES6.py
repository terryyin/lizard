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



