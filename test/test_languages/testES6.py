import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import JavaScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list

class Test_tokenizing_ES6(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(JavaScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['`abc`'], '`abc`')

    def test_dollar_var(self):
        self.check_tokens(["`abc\ndef`"], """`abc\ndef`""")

    def xtest_tokenizing_string_with_formatter(self):
        self.check_tokens(['""', '${', '1', '}', '"a"' ], r'''"${1}a"''')

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

    def test_arraw_function_with_multiple_param(self):
        functions = get_js_function_list("""
            const x=(a, b=3, {...x, y})=>1
        """)
        self.assertEqual('x', functions[0].name)

    def test_arrow_function_return_object(self):
        functions = get_js_function_list("""
            pairs = evens.map(v => ({ even: v, odd: v + 1 }))
        """)
        self.assertEqual(1, len(functions))

    def test_class(self):
        functions = get_js_function_list("""
            class A {
            f(){}
            g:(x,y)=>x+1
            h:function(){}
            get i(){return 1;}
            }
        """)
        self.assertEqual(4, len(functions))
        self.assertEqual('f', functions[0].name)
        self.assertEqual('g', functions[1].name)
        self.assertEqual('h', functions[2].name)
        self.assertEqual('i', functions[3].name)

    def test_generator_function(self):
        functions = get_js_function_list("""
            function* range() {yield 1}
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual('range', functions[0].name)

    def test_generator_function_assign_to_name(self):
        functions = get_js_function_list("""
            range = function* () {yield 1}
        """)
        self.assertEqual(1, len(functions))
        self.assertEqual('range', functions[0].name)

    def test_statement_block(self):
        functions = get_js_function_list("""
            function a(e) {
                if (id == 'current') {
                    a() {}
                } else {
                    a() {}
                }
                do{ a() {} }while(x);
                switch(x){ a() {} }
                for(x){ a() {} }
                for await (x){ a() {} }
                while(x){ a() {} }
                try{
                    a(){}
                } catch (x) {
                    a() {}
                } final {
                    a() {}
                }
            }

        """)
        self.assertEqual(1, len(functions))
        self.assertEqual('a', functions[0].name)

    # TBD: Method Properties
