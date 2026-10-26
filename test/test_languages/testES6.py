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


class Test_ES6_unparenthesized_arrow_params(unittest.TestCase):
    """Tests class field arrows with unparenthesized single parameter."""

    def test_async_unparenthesized_arrow(self):
        """field = async x => {} should detect field and not break subsequent methods"""
        code = '''
        class Foo {
            before() { return 1; }
            handler = async x => { return x; };
            after() { return 2; }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("before", names)
        self.assertIn("after", names)

    def test_unparenthesized_arrow_no_async(self):
        """field = x => {} should detect field and not break subsequent methods"""
        code = '''
        class Foo {
            before() { return 1; }
            transform = x => x * 2;
            after() { return 2; }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("before", names)
        self.assertIn("after", names)

    def test_async_unparenthesized_arrow_complex_body(self):
        """async field arrow with complex body should not corrupt parser state"""
        code = '''
        class Service {
            extractRules() { return {}; }
            runExternal = async recordIds => {
                if (recordIds.length > 0) {
                    await this.doSomething();
                }
                return true;
            };
            runReset(record) {
                if (typeof this.reset === "function") { this.reset(record); }
            }
            get tableClass() { return "test"; }
            deleteRecord(isDeleted, itemIndex) {
                try { this.records.splice(itemIndex, 1); } catch (err) { console.error(err); }
            }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("extractRules", names)
        self.assertIn("runReset", names)
        self.assertIn("get tableClass", names)
        self.assertIn("deleteRecord", names)
        self.assertNotIn("if", names)


class Test_ES6_dot_field_assignment(unittest.TestCase):
    """Tests that field = OBJ.PROP does not break subsequent method detection."""

    def test_dot_field_assignment(self):
        """field = A.B; should not swallow the next method"""
        code = 'class C { x = A.B; m1() {} m2() {} }'
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("m1", names)
        self.assertIn("m2", names)

    def test_chained_dot_field_assignment(self):
        """field = A.B.C; should not swallow the next method"""
        code = 'class C { x = A.B.C; m1() {} m2() {} }'
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("m1", names)
        self.assertIn("m2", names)

    def test_multiple_dot_field_assignments(self):
        """Multiple field = OBJ.PROP should not swallow methods"""
        code = '''
        class C {
            x = A.B;
            y = C.D;
            m1() { return 1; }
            m2() { return 2; }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("m1", names)
        self.assertIn("m2", names)


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


class Test_ES6_object_in_the_body_of_an_arrow_function(unittest.TestCase):

    def test_object_literals_in_an_expression_body(self):
        code = (
            "function layout(vertical) {\n"
            "  const box = f => vertical ? {x: f.a, y: f.b} : {x: f.b, y: f.a};\n"
            "  const pair = (a, b) => a && {a, b};\n"
            "  return box;\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        for filename in ("a.js", "a.ts", "a.tsx"):
            self.assertEqual(
                [('box', 2, 2, 2), ('pair', 3, 3, 2), ('layout', 1, 5, 1),
                 ('after', 6, 6, 1)],
                [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                 for f in analyze_file.analyze_source_code(
                     filename, code).function_list], filename)

    def test_brace_after_a_function_type_is_the_body(self):
        code = (
            "function filter(value: Json): (event: Obj) => boolean {\n"
            "  return () => true;\n"
            "}\n"
            "function after(): void {}\n"
        )
        self.assertEqual(
            [('(anonymous)', 2, 2), ('filter', 1, 3), ('after', 4, 4)],
            [(f.name, f.start_line, f.end_line) for f in
             analyze_file.analyze_source_code("a.ts", code).function_list])


class Test_ES6_arrow_function_without_braces_on_many_lines(unittest.TestCase):
    """An arrow function without braces ends with the last line of its
    expression; the first token of the next line is not its own."""

    def summary(self, code, filename="a.js"):
        return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
                 f.nloc)
                for f in analyze_file.analyze_source_code(
                    filename, code).function_list]

    def test_lines_and_tokens_are_the_same_with_and_without_a_semicolon(self):
        with_semicolon = get_js_function_list(
            "const inc = x => x + 1;\nlet a\n")[0]
        without = get_js_function_list("const inc = x => x + 1\nlet a\n")[0]
        self.assertEqual(
            (1, 1, with_semicolon.token_count - 1),
            (without.end_line, without.nloc, without.token_count))

    def test_ended_by_a_closing_bracket_on_the_next_line(self):
        code = (
            "run(\n"
            "  x => x + 1\n"
            ")\n"
            "const rules = {\n"
            "  find: text => text.match(/a/g)\n"
            "}\n"
            "function after() { return 1; }\n"
        )
        self.assertEqual([('(anonymous)', 2, 2, 1, 1), ('find', 5, 5, 1, 1),
                          ('after', 7, 7, 1, 1)],
                         self.summary(code))

    def test_statements_and_members_on_the_next_lines(self):
        code = (
            "const inc = x => x + 1\n"
            "const dec = x => x - 1\n"
            "if (a) { b() }\n"
            "class A {\n"
            "  handler = () => 1\n"
            "  async load() {\n"
            "    return 1;\n"
            "  }\n"
            "}\n"
        )
        for filename in ("a.js", "a.ts", "a.tsx"):
            self.assertEqual(
                [('inc', 1, 1, 1, 1), ('dec', 2, 2, 1, 1),
                 ('handler', 5, 5, 1, 1), ('load', 6, 8, 1, 3)],
                self.summary(code, filename), filename)

    def test_case_and_closing_brace_go_to_the_function_around(self):
        code = (
            "function m(key, cursor) {\n"
            "  switch (true) {\n"
            "    case key.left && key.ctrl:\n"
            "      return () => cursor.prevWord()\n"
            "    case key.right:\n"
            "      return () => cursor.nextWord()\n"
            "  }\n"
            "}\n"
            "export function register(fn) {\n"
            "  add(fn)\n"
            "  return () => remove(fn)\n"
            "}\n"
        )
        self.assertEqual(
            [('(anonymous)', 4, 4, 1, 1), ('(anonymous)', 6, 6, 1, 1),
             ('m', 1, 8, 4, 8), ('(anonymous)', 11, 11, 1, 1),
             ('register', 9, 12, 1, 4)],
            self.summary(code))

    def test_expression_that_goes_on_at_the_next_lines(self):
        code = (
            "const pick = (a, b) => a && b\n"
            "    ? a\n"
            "    : b || 0;\n"
            "const row = item => '<td>' + item.name\n"
            "  + (item.on ? 'on' : 'off') +\n"
            "  '</td>';\n"
            "const names = list => list\n"
            "  .map(x => x.name)\n"
            "  .join(', ')\n"
            "function after() { return 1; }\n"
        )
        self.assertEqual(
            [('pick', 1, 3, 4, 3), ('row', 4, 6, 2, 3),
             ('(anonymous)', 8, 8, 1, 1), ('names', 7, 9, 1, 3),
             ('after', 10, 10, 1, 1)],
            self.summary(code))
