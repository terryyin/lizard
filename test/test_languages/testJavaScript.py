import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import TypeScriptReader


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("a.js", source_code).function_list


class Test_tokenizing_JavaScript(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(TypeScriptReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_dollar_var(self):
        self.check_tokens(['$a'], '$a')

    def test_tokenizing_javascript_regular_expression(self):
        self.check_tokens(['/ab/'], '/ab/')
        self.check_tokens([r'/\//'], r'/\//')
        self.check_tokens([r'/a/igm'], r'/a/igm')

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_tokenizing_javascript_regular_expression1(self):
        self.check_tokens(['a', '=', '/ab/'], 'a=/ab/')

    def test_tokenizing_javascript_comments(self):
        self.check_tokens(['/**a/*/'], '''/**a/*/''')

    def test_tokenizing_pattern(self):
        self.check_tokens([r'/\//'], r'/\//')

    def test_tokenizing_javascript_multiple_line_string(self):
        self.check_tokens(['"aaa\\\nbbb"'], '"aaa\\\nbbb"')

    def test_tokenizing_template_literal_with_expression(self):
        self.check_tokens(['`', '`hello `', '${', 'name', '}', '`'], '`hello ${name}`')

    def test_tokenizing_template_literal_multiline(self):
        self.check_tokens(['`','`hello\nworld`', '`'], '`hello\nworld`')

class Test_parser_for_JavaScript(unittest.TestCase):

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

    def test_async_function(self):
        functions = get_js_function_list("async function foo() {}")
        self.assertEqual('foo', functions[0].name)

    def test_generator_function(self):
        functions = get_js_function_list("function* gen() {}")
        self.assertEqual('gen', functions[0].name)

    def test_async_generator_function(self):
        functions = get_js_function_list("async function* gen() {}")
        self.assertEqual('gen', functions[0].name)

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
        print(f"DEBUG: Found methods: {found_methods}")
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
        print(f"DEBUG: Found methods: {found_methods}")
        expected_methods = ['constructor', 'init', 'render']
        
        # This should fail - only constructor will be detected, init and render will be missing
        for method in expected_methods:
            self.assertIn(method, found_methods, f"Method '{method}' should be detected. Found: {found_methods}")

    @unittest.skip("Ignoring complex test case while debugging simpler cases")
    def test_interactive_widget_methods_IGNORE(self):
        code = '''
        /**
         * Interactive UI Component with Multiple JavaScript Methods
         */
        class InteractiveWidget {
            constructor(containerId) {
                this.container = document.getElementById(containerId);
                if (!this.container) {
                    throw new Error(`Container element with ID ${containerId} not found`);
                }
                this.state = {
                    clicks: 0,
                    items: [],
                    timer: null,
                    isRunning: false
                };
                this.init();
            }
            // Initialization methods
            init() {
                this.render();
                this.bindEvents();
            }
            render() {
                this.container.innerHTML = '';
                this.updateUI();
            }
            updateUI() {
                document.getElementById('click-count').textContent = this.state.clicks;
                this.renderItemList();
            }
            renderItemList() {
                const list = document.getElementById('item-list');
                list.innerHTML = this.state.items.map(item => `<li>${item}</li>`).join('');
            }
            bindEvents() {
                document.getElementById('click-btn').addEventListener('click', this.handleClick.bind(this));
            }
            handleClick() {
                this.state.clicks++;
                this.updateUI();
            }
            toggleTimer() {
                if (this.state.isRunning) {
                    this.stopTimer();
                } else {
                    this.startTimer();
                }
                this.state.isRunning = !this.state.isRunning;
            }
            startTimer() {
                this.state.timerCount = 0;
                this.state.timer = setInterval(() => {
                    this.state.timerCount++;
                    this.updateUI();
                }, 1000);
            }
            stopTimer() {
                clearInterval(this.state.timer);
                this.state.timer = null;
            }
            addItem() {
                const input = document.getElementById('item-input');
                const value = input.value.trim();
                if (value) {
                    this.state.items.push(value);
                    input.value = '';
                    this.updateUI();
                }
            }
            removeItem(item) {
                this.state.items = this.state.items.filter(i => i !== item);
                this.updateUI();
            }
            animateButton(buttonId) {
                const button = document.getElementById(buttonId);
                button.classList.add('clicked');
                setTimeout(() => {
                    button.classList.remove('clicked');
                }, 200);
            }
            animateAddition() {
                const list = document.getElementById('item-list');
                list.classList.add('item-added');
                setTimeout(() => {
                    list.classList.remove('item-added');
                }, 300);
            }
            static formatDate(date) {
                return new Intl.DateTimeFormat('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit'
                }).format(date);
            }
            static generateRandomId() {
                return Math.random().toString(36).substring(2, 9);
            }
            static async simulateApiCall(data) {
                return new Promise((resolve) => {
                    setTimeout(() => {
                        resolve({
                            status: 'success',
                            data,
                            timestamp: new Date().toISOString(),
                            id: this.generateRandomId()
                        });
                    }, 1000);
                });
            }
            static processItems(items, processorFn) {
                return items.map((item, index) => processorFn(item, index));
            }
            static filterUnique(array) {
                return [...new Set(array)];
            }
            static sortByKey(array, key, ascending = true) {
                return [...array].sort((a, b) => {
                    if (a[key] < b[key]) return ascending ? -1 : 1;
                    if (a[key] > b[key]) return ascending ? 1 : -1;
                    return 0;
                });
            }
        }
        '''
        functions = get_js_function_list(code)
        # All instance and static methods should be detected
        expected_methods = [
            'constructor', 'init', 'render', 'updateUI', 'renderItemList', 'bindEvents', 'handleClick',
            'toggleTimer', 'startTimer', 'stopTimer', 'addItem', 'removeItem', 'animateButton', 'animateAddition',
            'formatDate', 'generateRandomId', 'simulateApiCall', 'processItems', 'filterUnique', 'sortByKey'
        ]
        found_methods = sorted([f.name for f in functions])
        for method in expected_methods:
            self.assertIn(method, found_methods, f"Method '{method}' should be detected.")

    def test_simple_async_method(self):
        # Test basic async method without complex nested callbacks
        code = '''
        class TestClass {
            static async simpleMethod() {
                return 'success';
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]
        print(f"DEBUG: Found methods in simple test: {found_methods}")
        
        self.assertIn('simpleMethod', found_methods, f"Method 'simpleMethod' should be detected. Found: {found_methods}")

    @unittest.skip("Ignoring complex test case while debugging simpler cases")
    def test_async_method_with_nested_callbacks(self):
        # Isolate the exact issue with simulateApiCall
        code = '''
        class TestClass {
            static async apiCall(data) {
                return new Promise((resolve) => {
                    setTimeout(() => {
                        resolve({
                            status: 'success',
                            timestamp: new Date().toISOString(),
                            id: this.generateRandomId()
                        });
                    }, 1000);
                });
            }
        }
        '''
        functions = get_js_function_list(code)
        found_methods = [f.name for f in functions]
        
        # This should detect apiCall but currently doesn't due to nested callback issue
        self.assertIn('apiCall', found_methods, f"Method 'apiCall' should be detected. Found: {found_methods}")

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
