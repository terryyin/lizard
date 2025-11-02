import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import VueReader


def get_vue_function_list(source_code):
    return analyze_file.analyze_source_code("a.vue", source_code).function_list


class TestVue(unittest.TestCase):

    def test_empty(self):
        functions = get_vue_function_list("")
        self.assertEqual(0, len(functions))

    def test_simple_js_function(self):
        code = '''
        <template>
            <div>Hello</div>
        </template>
        <script>
            export default {
                methods: {
                    hello() {
                        return "world"
                    }
                }
            }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(["hello"], [f.name for f in functions])

    def test_ts_function(self):
        code = '''
        <template>
            <div>Hello</div>
        </template>
        <script lang="ts">
            export default {
                methods: {
                    hello(): string {
                        return "world"
                    }
                }
            }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("hello", functions[0].name)

    def test_multiple_functions(self):
        code = '''
        <script>
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
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(["helper1", "method1", "method2"], [f.name for f in functions])
        self.assertEqual(2, functions[2].cyclomatic_complexity)


class Test_Vue_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Vue.js"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        <script>
        export default {
            methods: {
                simple(x) {
                    return x + 1;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        <script>
        export default {
            methods: {
                check(x) {
                    if (x > 0) {  // +1
                        return "positive";
                    }
                    return "non-positive";
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        <script>
        export default {
            methods: {
                nested() {
                    for (let i = 0; i < 10; i++) {           // +1
                        for (let j = 0; j < 10; j++) {       // +2 (nesting=1)
                            if (i === j) {                   // +3 (nesting=2)
                                console.log(i);
                            }
                        }
                    }
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        <script>
        export default {
            methods: {
                check(a, b, c, d, e) {
                    if (a && b && c) {  // +1 for if, +1 for && sequence
                        return 1;
                    }
                    if (d || e) {       // +1 for if, +1 for || sequence
                        return 2;
                    }
                    return 0;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_typescript_support(self):
        """TypeScript in Vue should work"""
        code = '''
        <script lang="ts">
        export default {
            methods: {
                check(x: number): string {
                    if (x > 0) {  // +1
                        return "positive";
                    }
                    return "non-positive";
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)