import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import VueReader


def get_vue_function_list(source_code):
    return analyze_file.analyze_source_code("a.vue", source_code).function_list


class Test_Vue(unittest.TestCase):

    def test_empty_vue_file(self):
        functions = get_vue_function_list("<template></template>")
        self.assertEqual(0, len(functions))

    def test_vue_file_with_empty_script(self):
        code = '''
        <template>
            <div>Hello</div>
        </template>
        <script>
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(0, len(functions))

    def test_vue_file_with_js_function(self):
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
        self.assertEqual(1, len(functions))
        self.assertEqual("hello", functions[0].name)

    def test_vue_file_with_ts_function(self):
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

    def test_vue_file_with_nested_object(self):
        code = '''
        <template>
            <div>Hello</div>
        </template>
        <script>
        export default {
            nested: { 
                test() {
                    return 1;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("test", functions[0].name)

    def test_vue_file_with_ts_nested_object(self):
        code = '''
        <template>
            <div>Hello</div>
        </template>
        <script lang="ts">
        export default {
            nested: { 
                test(): number {
                    return 1;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("test", functions[0].name) 