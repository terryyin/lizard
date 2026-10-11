import unittest

from .testJavaScript import get_js_function_list


class Test_JavaScript_members(unittest.TestCase):
    def test_object_method_shorthand(self):
        functions = get_js_function_list("var obj = {method() {}}")
        self.assertEqual('method', functions[0].name)

    def test_object_method_with_computed_name(self):
        functions = get_js_function_list("var obj = {['computed' + 'Name']() {}}")
        self.assertEqual('computedName', functions[0].name)

    def test_object_getter_method(self):
        functions = get_js_function_list("var obj = {get prop() {}}")
        self.assertEqual('get prop', functions[0].name)

    def test_object_setter_method(self):
        functions = get_js_function_list("var obj = {set prop(val) {}}")
        self.assertEqual('set prop', functions[0].name)

    def test_class_method_decorators(self):
        code = '''
            class Example {
                @decorator
                method() {}
            }
        '''
        functions = get_js_function_list(code)
        self.assertEqual('method', functions[0].name)

    def test_nested_object_methods(self):
        code = '''
            const obj = {
                outer: {
                    inner() {}
                }
            }
        '''
        functions = get_js_function_list(code)
        self.assertEqual('inner', functions[0].name)

    def test_simple_class_two_methods_WORKS(self):
        # This simpler case works fine
        code = '''
        class SimpleClass {
            constructor() {
                this.value = 0;
            }

            getValue() {
                return this.value;
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]
        expected_methods = ['constructor', 'getValue']
        self.assertEqual(sorted(expected_methods), sorted(found_methods))

    def test_class_with_complex_constructor_and_methods(self):
        # This reproduces the failing pattern from the original issue
        code = '''
        class TestWidget {
            constructor(id) {
                this.container = document.getElementById(id);
                this.state = {
                    clicks: 0,
                    items: []
                };
                this.init();
            }

            init() {
                this.render();
            }

            render() {
                this.container.innerHTML = '';
                this.updateUI();
            }

            updateUI() {
                document.getElementById('count').textContent = this.state.clicks;
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]
        expected_methods = ['constructor', 'init', 'render', 'updateUI']

        # Check each expected method individually to see which ones are missing
        for method in expected_methods:
            self.assertIn(method, found_methods, f"Method '{method}' should be detected. Found: {found_methods}")

    def test_class_with_template_literal_bug(self):
        # Template literals with ${} interpolation seem to break class method detection
        code = '''
        class BuggyClass {
            constructor(id) {
                throw new Error(`Element with ID ${id} not found`);
            }

            init() {
                return "should be detected";
            }

            render() {
                return "should also be detected";
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]
        expected_methods = ['constructor', 'init', 'render']

        # This should fail - only constructor will be detected, init and render will be missing
        for method in expected_methods:
            self.assertIn(method, found_methods, f"Method '{method}' should be detected. Found: {found_methods}")


class Test_JavaScript_prototype_methods(unittest.TestCase):
    """Tests prototype-based patterns."""

    def test_prototype_methods(self):
        code = '''
        function Greeter(name) { this.name = name; }
        Greeter.prototype.greet = function() { return "Hello " + this.name; };
        Greeter.prototype.farewell = function() { return "Bye " + this.name; };
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Greeter", names)
        self.assertIn("Greeter.prototype.greet", names)
        self.assertIn("Greeter.prototype.farewell", names)

    def test_prototype_arrow(self):
        """Prototype assignment with arrow function"""
        code = '''
        function Counter() { this.count = 0; }
        Counter.prototype.increment = function() { this.count++; };
        Counter.prototype.getCount = function() { return this.count; };
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("Counter", names)
        self.assertIn("Counter.prototype.increment", names)
        self.assertIn("Counter.prototype.getCount", names)



class Test_JavaScript_class_inheritance(unittest.TestCase):
    """Tests class inheritance patterns."""

    def test_extends_with_methods(self):
        code = '''
        class Animal {
            constructor(name) { this.name = name; }
            speak() { return this.name + " makes a noise"; }
        }
        class Dog extends Animal {
            constructor(name) { super(name); }
            speak() { return this.name + " barks"; }
            fetch(item) { return "fetched " + item; }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertEqual(names.count("constructor"), 2)
        self.assertEqual(names.count("speak"), 2)
        self.assertIn("fetch", names)

    def test_simple_class_methods(self):
        """Simple class with no complexity should detect all methods"""
        code = '''
        class Calculator {
            add(a, b) { return a + b; }
            subtract(a, b) { return a - b; }
            multiply(a, b) { return a * b; }
            divide(a, b) {
                if (b === 0) throw new Error("div by zero");
                return a / b;
            }
        }
        '''
        functions = get_js_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("add", names)
        self.assertIn("subtract", names)
        self.assertIn("multiply", names)
        self.assertIn("divide", names)
        div = next(f for f in functions if f.name == "divide")
        self.assertGreater(div.cyclomatic_complexity, 1)
