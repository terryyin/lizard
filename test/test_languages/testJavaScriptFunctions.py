import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_functions(unittest.TestCase):
    def test_simple_function(self):
        functions = get_js_function_list("function foo(){}")
        self.assertEqual("foo", functions[0].name)

    def test_simple_function_complexity(self):
        functions = get_js_function_list("function foo(){m;if(a);}")
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_parameter_count(self):
        functions = get_js_function_list("function foo(a, b){}")
        self.assertEqual(2, functions[0].parameter_count)

    def test_function_assigning_to_a_name(self):
        functions = get_js_function_list("a = function (a, b){}")
        self.assertEqual('a', functions[0].name)

    def test_not_a_function_assigning_to_a_name(self):
        functions = get_js_function_list("abc=3; function (a, b){}")
        self.assertEqual('(anonymous)', functions[0].name)

    def test_function_without_name_assign_to_field(self):
        functions = get_js_function_list("a.b.c = function (a, b){}")
        self.assertEqual('a.b.c', functions[0].name)

    def test_function_in_a_object(self):
        functions = get_js_function_list("var App={a:function(){};}")
        self.assertEqual('a', functions[0].name)

    def test_function_in_a_function(self):
        functions = get_js_function_list("function a(){function b(){}}")
        self.assertEqual('b', functions[0].name)
        self.assertEqual('a', functions[1].name)

    # test "<>" error match in "< b) {} } function b () { return (dispatch, getState) =>"
    def test_function_in_arrow(self):
        functions = get_js_function_list(
            "function a () {f (a < b) {} } function b () { return (dispatch, getState) => {} }")
        self.assertEqual('a', functions[0].name)
        self.assertEqual('(anonymous)', functions[1].name)
        self.assertEqual('b', functions[2].name)

    # test long_name, fix "a x, y)" to "a (x, y)"
    def test_function_long_name(self):
        functions = get_js_function_list(
            "function a (x, y) {if (a < b) {} } function b () { return (dispatch, getState) => {} }")
        self.assertEqual('a ( x , y )', functions[0].long_name)
        self.assertEqual('b ( )', functions[2].long_name)

    def test_global(self):
        functions = get_js_function_list("{}")
        self.assertEqual(0, len(functions))

    def test_async_function(self):
        functions = get_js_function_list("async function foo() {}")
        self.assertEqual('foo', functions[0].name)

    def test_generator_function(self):
        functions = get_js_function_list("function* gen() {}")
        self.assertEqual('gen', functions[0].name)

    def test_async_generator_function(self):
        functions = get_js_function_list("async function* gen() {}")
        self.assertEqual('gen', functions[0].name)


class Test_JavaScript_commonjs_exports(unittest.TestCase):
    """Tests CommonJS module patterns."""

    def test_module_exports_function(self):
        code = '''
        module.exports = function handler(req, res) {
            res.send("ok");
        };
        exports.helper = function(x) { return x + 1; };
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("handler", names)
        self.assertIn("exports.helper", names)

    def test_named_exports(self):
        code = '''
        exports.add = function(a, b) { return a + b; };
        exports.subtract = function(a, b) { return a - b; };
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("exports.add", names)
        self.assertIn("exports.subtract", names)



class Test_JavaScript_factory_functions(unittest.TestCase):
    """Tests factory function patterns."""

    def test_factory_returning_object_methods(self):
        code = '''
        function createUser(name, age) {
            return {
                getName() { return name; },
                getAge() { return age; },
                greet() { return "Hi " + name; }
            };
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("createUser", names)
        self.assertIn("getName", names)
        self.assertIn("getAge", names)
        self.assertIn("greet", names)

    def test_revealing_module(self):
        """Revealing module pattern"""
        code = '''
        const myModule = (function() {
            function privateMethod() { return 42; }
            function publicMethod() { return privateMethod(); }
            return { publicMethod: publicMethod };
        })();
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("privateMethod", names)
        self.assertIn("publicMethod", names)



class Test_JavaScript_promise_and_async(unittest.TestCase):
    """Tests Promise chains and async patterns."""

    def test_promise_chain_callbacks(self):
        code = '''
        function loadData(url) {
            return fetch(url)
                .then(res => res.json())
                .then(data => data.items);
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("loadData", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 2)  # two .then callbacks

    def test_try_catch_async(self):
        code = '''
        async function safeFetch(url) {
            try {
                const res = await fetch(url);
                return await res.json();
            } catch (err) {
                console.error(err);
                return null;
            }
        }
        '''
        functions = get_js_function_list(code)
        self.assertEqual(["safeFetch"], [f.name for f in functions])

    def test_reduce_callback(self):
        code = '''
        function sum(arr) {
            return arr.reduce((acc, x) => acc + x, 0);
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("sum", names)
        self.assertIn("(anonymous)", names)



class Test_JavaScript_event_listeners(unittest.TestCase):
    """Tests DOM event listener patterns."""

    def test_addEventListener_named(self):
        """Named function in addEventListener should be detected"""
        code = '''
        function setupListeners(el) {
            el.addEventListener("click", function handleClick(e) {
                e.preventDefault();
            });
            el.addEventListener("keydown", (e) => {
                if (e.key === "Enter") submit();
            });
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("setupListeners", names)
        self.assertIn("handleClick", names)
        self.assertIn("(anonymous)", names)

    def test_setTimeout_callback(self):
        code = '''
        function delayedLog(msg) {
            setTimeout(function() {
                console.log(msg);
            }, 1000);
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("delayedLog", names)
        self.assertIn("(anonymous)", names)
