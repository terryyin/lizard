import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import VueReader


def get_vue_function_list(source_code):
    return analyze_file.analyze_source_code("a.vue", source_code).function_list

class Test_tokenizing_Vue(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(VueReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_js_script(self):
        code = '''
        <script>
        export default {
            methods: {
                test() {
                    return 1;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual("test", functions[0].name)

    def xtest_ts_script(self):
        code = '''
        <script lang="ts">
        export default {
            methods: {
                test(): number {
                    return 1;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual("test", functions[0].name)

    def test_complex_component(self):
        code = '''
        <template>
            <div>{{ message }}</div>
        </template>
        
        <script>
        export default {
            data() {
                return {
                    message: 'Hello'
                }
            },
            methods: {
                updateMessage(newMsg) {
                    this.message = newMsg;
                }
            }
        }
        </script>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(2, len(functions))
        self.assertEqual("data", functions[0].name)
        self.assertEqual("updateMessage", functions[1].name)

    def test_template_ignored(self):
        code = '''
        <template>
            <div @click="handleClick"></div>
        </template>
        '''
        functions = get_vue_function_list(code)
        self.assertEqual(0, len(functions)) 