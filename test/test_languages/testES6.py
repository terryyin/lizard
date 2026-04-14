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
        self.check_tokens(["`", "`abc\ndef`", "`"], """`abc\ndef`""")

    def test_tokenizing_string_with_formatter(self):
        self.check_tokens(['"${1}a"'], r'"${1}a"')

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
            m(){}
            }
        """)
        self.assertEqual(['f', 'm'], [f.name for f in functions])

    def test_class_with_prop_as_function(self):
        functions = get_js_function_list("""
            class A {
            f(){}
            m(){}
            g:(x,y)=>x+1
            h:function(){}
            get i(){return 1;}
            }
        """)
        self.assertEqual(['f', 'm', 'g', 'h', 'get i'], [f.name for f in functions])

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


class Test_ES6_destructuring_params(unittest.TestCase):
    """Tests arrow functions with destructuring parameters."""

    def test_object_destructuring(self):
        functions = get_js_function_list("const process = ({name, age}) => name + age;")
        self.assertEqual(["process"], [f.name for f in functions])

    def test_array_destructuring(self):
        functions = get_js_function_list("const first = ([head, ...tail]) => head;")
        self.assertEqual(["first"], [f.name for f in functions])

    def test_nested_destructuring(self):
        code = '''
        const extract = ({user: {name, address: {city}}}) => {
            return name + " from " + city;
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["extract"], [f.name for f in functions])

    def test_default_values_in_destructuring(self):
        code = '''
        const configure = ({host = "localhost", port = 3000, debug = false}) => {
            return {host, port, debug};
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["configure"], [f.name for f in functions])


class Test_ES6_default_params(unittest.TestCase):
    """Tests arrow functions with default parameters."""

    def test_default_params(self):
        code = 'const greet = (name = "world", greeting = "Hello") => greeting + " " + name;'
        functions = get_js_function_list(code)
        self.assertEqual(["greet"], [f.name for f in functions])

    def test_default_param_function(self):
        code = '''
        function createEl(tag = "div", content = "") {
            const el = document.createElement(tag);
            el.textContent = content;
            return el;
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["createEl"], [f.name for f in functions])


class Test_ES6_class_field_arrows(unittest.TestCase):
    """Tests class field arrow functions."""

    def test_class_arrow_fields(self):
        """Arrow functions as class fields should use the field name"""
        code = '''
        class Btn {
            handleClick = () => { this.setState({clicked: true}); };
            handleHover = (e) => { console.log(e); };
            render() { return null; }
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(
            ["handleClick", "handleHover", "render"],
            [f.name for f in functions])

    def test_class_field_then_method(self):
        """Regular class method after arrow field should be detected"""
        code = '''
        class Timer {
            tick = () => { this.count++; };
            reset() { this.count = 0; }
            getCount() { return this.count; }
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(
            ["tick", "reset", "getCount"],
            [f.name for f in functions])

    def test_field_arrow_with_params(self):
        """Field arrow with parameters should use the field name"""
        code = '''
        class EventBus {
            emit = (event, data) => { this.listeners[event](data); };
            on = (event, cb) => { this.listeners[event] = cb; };
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(
            ["emit", "on"],
            [f.name for f in functions])

    def test_field_arrow_block_body(self):
        """Field arrow with block body and complexity"""
        code = '''
        class Validator {
            validate = (value) => {
                if (!value) return false;
                if (value.length < 3) return false;
                return true;
            };
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["validate"], [f.name for f in functions])
        self.assertGreater(functions[0].cyclomatic_complexity, 1)


class Test_ES6_optional_chaining_no_fp(unittest.TestCase):
    """Tests that optional chaining does not produce false positives."""

    def test_optional_chain_call(self):
        """obj?.method() should not produce FP for method"""
        code = '''
        function safe(obj) {
            return obj?.method();
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["safe"], [f.name for f in functions])

    def test_nullish_coalescing(self):
        """Nullish coalescing should not produce FP"""
        code = '''
        function getVal(cfg) {
            const val = cfg?.setting ?? "default";
            return val;
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["getVal"], [f.name for f in functions])


class Test_ES6_private_class_fields(unittest.TestCase):
    """Tests private class field methods."""

    def test_private_field_methods(self):
        code = '''
        class Counter {
            #count = 0;
            increment() { this.#count++; }
            getCount() { return this.#count; }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("increment", names)
        self.assertIn("getCount", names)


class Test_ES6_computed_property_methods(unittest.TestCase):
    """Tests computed property name methods."""

    def test_computed_key_methods(self):
        code = '''
        const key = "method";
        const obj = {
            [key]() { return 1; },
            ["static" + "Key"]() { return 2; }
        };
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("key", names)
        self.assertIn("staticKey", names)

    def test_symbol_iterator(self):
        code = '''
        class Iterable {
            [Symbol.iterator]() {
                let i = 0;
                return { next: () => ({value: i++, done: i > 10}) };
            }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("symbol.iterator", names)


class Test_ES6_for_loops_with_functions(unittest.TestCase):
    """Tests function detection inside for loops."""

    def test_for_of_body(self):
        code = '''
        function processAll(items) {
            for (const item of items) {
                if (item.active) { handle(item); }
            }
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["processAll"], [f.name for f in functions])

    def test_forEach_callback(self):
        code = '''
        function logAll(items) {
            items.forEach((item) => {
                console.log(item.name);
            });
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("logAll", names)
        self.assertIn("(anonymous)", names)


class Test_ES6_multiple_arrow_patterns(unittest.TestCase):
    """Tests various arrow function declaration patterns."""

    def test_arrow_returning_object(self):
        """Arrow returning object literal in parens"""
        code = "const make = (x) => ({value: x, label: String(x)});"
        functions = get_js_function_list(code)
        self.assertEqual(["make"], [f.name for f in functions])

    def test_chained_arrow_filter_map(self):
        """Filter + map chain with arrow callbacks"""
        code = '''
        const result = items
            .filter(x => x > 0)
            .map(x => x * 2);
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertEqual(2, anon_count)

    def test_nested_arrows(self):
        """Curried function pattern — outer arrow detected"""
        code = '''
        const add = (a) => (b) => a + b;
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("add", names)
        # Expression-body arrow: inner (b) => a + b is the body expression,
        # only the outer arrow is detected as a function
        self.assertEqual(1, len(functions))

    def test_arrow_with_block_and_return(self):
        """Arrow with block body and explicit return"""
        code = '''
        const validate = (value) => {
            if (!value) return false;
            if (value.length < 3) return false;
            return true;
        };
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["validate"], [f.name for f in functions])
        self.assertGreater(functions[0].cyclomatic_complexity, 1)
