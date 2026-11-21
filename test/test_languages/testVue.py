import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import VueReader


def get_vue_file_info(source_code):
    return analyze_file.analyze_source_code("a.vue", source_code)


def get_vue_function_list(source_code):
    return get_vue_file_info(source_code).function_list


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


def function_summaries(functions):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
             f.nloc, f.token_count) for f in functions]


def script_file_summaries(filename, script):
    return function_summaries(
        analyze_file.analyze_source_code(filename, script).function_list)


PICK = '''\
export default {
  methods: {
    pick(a, b) {
      if (a && b) {
        return 'both'
      }
      return 'one'
    }
  }
}
'''


class TestVueScriptBlocks(unittest.TestCase):

    def test_angle_bracket_in_script_attribute(self):
        functions = get_vue_function_list('''\
<script setup lang="ts" generic="T extends Record<string, any>">
import { computed } from "vue"
const props = defineProps<{ items: T[] }>()
function pick(a: T, b: T) {
  if (a && b) {
    return "both"
  }
  return "one"
}
</script>
''')
        self.assertEqual([('pick', 4, 9, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_apostrophe_in_template_text(self):
        functions = get_vue_function_list('''\
<template>
  <p>Don't worry, the draft is saved</p>
</template>
<script>
''' + PICK + '''\
</script>
''')
        self.assertEqual([('pick', 7, 12, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_apostrophe_in_html_comment(self):
        functions = get_vue_function_list('''\
<!-- You'll need to reload the page after a change -->
<script>
''' + PICK + '''\
</script>
''')
        self.assertEqual([('pick', 5, 10, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_comparisons_without_spaces(self):
        functions = get_vue_function_list('''\
<script>
function clamp(i, n, x) {
  if (i<n && x>0) {
    return i
  }
  return n
}
</script>
''')
        self.assertEqual([('clamp', 2, 7, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_generic_type_members_are_not_functions(self):
        script = '''\
type FieldInfo<T = any> = {
  fieldState: {
    validator: ((_value: T) => string | null) | null
  }
  fieldApi: {
    setValue(_value: T): void
    validate(): boolean
    reset(): void
  }
}
function pick(a, b) {
  if (a && b) {
    return 1
  }
  return 2
}
'''
        functions = get_vue_function_list(
            '<script lang="ts">\n' + script + '</script>\n')
        self.assertEqual(['pick'], [f.name for f in functions])

    def test_script_tag_in_template_or_comment_is_not_code(self):
        functions = get_vue_function_list('''\
<template>
  <!-- <script>function commented() {}</script> -->
  <template v-if="legacy">
    <script>function inTemplate() {}</script>
  </template>
  <p>{{ count<limit ? 'more' : 'done' }}</p>
</template>
<!-- <script>function outside() {}</script> -->
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual([('real', 10, 12)],
                         [(f.name, f.start_line, f.end_line)
                          for f in functions])

    def test_script_example_in_custom_block_is_not_code(self):
        functions = get_vue_function_list('''\
<docs>
Use it like this:

<script>
function example() {}
</script>
</docs>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual(['real'], [f.name for f in functions])

    def test_typescript_script_matches_ts_file(self):
        script = '''\
interface Props<T = string> {
  items: T[]
  format?: (item: T) => string
}
const props = defineProps<Props>()
const label = computed(() => props.items.length > 1 ? 'many' : 'one')
function select(index: number): string | undefined {
  if (index<0 || index>=props.items.length) {
    return undefined
  }
  return props.format?.(props.items[index]) ?? `${index && 'item'}`
}
'''
        functions = get_vue_function_list(
            '<script setup lang="ts">\n' + script + '</script>\n')
        self.assertEqual(
            [(n, s + 1, e + 1, c, nl, t) for n, s, e, c, nl, t
             in script_file_summaries('a.ts', script)],
            function_summaries(functions))

    def test_tsx_script_matches_tsx_file(self):
        script = '''\
export default defineComponent({
  setup(props) {
    const label = (count) => count > 1 ? 'items' : 'item'
    return () => <p>{props.count} {label(props.count)} aren't shown</p>
  },
  methods: {
    reset(a, b) {
      return `${a && b ? a : b}`
    }
  }
})
'''
        functions = get_vue_function_list(
            '<script lang="tsx">\n' + script + '</script>\n')
        self.assertEqual(
            [(n, s + 1, e + 1, c, nl, t) for n, s, e, c, nl, t
             in script_file_summaries('a.tsx', script)],
            function_summaries(functions))

    def test_conditions_in_template_literal_interpolation(self):
        functions = get_vue_function_list('''\
<script>
function label(count, max) {
  return `${count > max && max > 0 ? 'many' : 'few'} items`
}
</script>
''')
        self.assertEqual([('label', 2, 4, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_closing_tag_text_in_script_string_and_comment(self):
        functions = get_vue_function_list('''\
<script>
const closing = "</script>"
/* A block ends at the first </script> outside strings and comments. */
function pick(a, b) {
  if (a && b) {
    return 1
  }
  return 2
}
</script>
''')
        self.assertEqual([('pick', 4, 9, 3)],
                         [s[:4] for s in function_summaries(functions)])

    def test_unclosed_interpolation_in_v_pre(self):
        functions = get_vue_function_list('''\
<template>
  <div v-pre>{{ raw</div>
</template>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual([('real', 5, 7)],
                         [(f.name, f.start_line, f.end_line)
                          for f in functions])

    def test_lang_is_not_read_from_other_attributes(self):
        functions = get_vue_function_list('''\
<script data-lang="js" lang="ts">
class Store {
  items: (() => void)[] = []
  clear() {}
}
</script>
''')
        self.assertEqual(['clear'], [f.name for f in functions])

    def test_template_tag_in_interpolation_string(self):
        functions = get_vue_function_list('''\
<template>
  <p>{{ '<template>' }}</p>
</template>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual(['real'], [f.name for f in functions])

    def test_script_tag_in_interpolation_string(self):
        functions = get_vue_function_list('''\
<template>
  <p>{{ '</template><script>function ghost() {}</script>' }}</p>
</template>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual(['real'], [f.name for f in functions])

    def test_closing_tag_ends_a_line_comment(self):
        info = get_vue_file_info('''\
<script>
export const answer = 42
// end of the options block</script>
<script setup>
function reset() {
  return 1
}
</script>
''')
        script = analyze_file.analyze_source_code('a.js', '''\

export const answer = 42
// end of the options block

function reset() {
  return 1
}
''')
        self.assertEqual(function_summaries(script.function_list),
                         function_summaries(info.function_list))
        self.assertEqual(script.nloc, info.nloc)

    def test_lang_text_in_another_attribute_value(self):
        functions = get_vue_function_list('''\
<script data-help=" lang='js' " lang="ts">
class Store {
  items: (() => void)[] = []
  clear() {}
}
</script>
''')
        self.assertEqual(['clear'], [f.name for f in functions])

    def test_pug_template_is_text(self):
        functions = get_vue_function_list('''\
<template lang="pug">
p Use a <template> element for groups
</template>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual(['real'], [f.name for f in functions])

    def test_raw_text_elements_in_template(self):
        functions = get_vue_function_list('''\
<template>
  <textarea>Wrap it in <template></textarea>
  <script>const example = "<template>"</script>
</template>
<script>
function real() {
  return 1
}
</script>
''')
        self.assertEqual(['real'], [f.name for f in functions])

    def test_uppercase_lang_attribute(self):
        functions = get_vue_function_list('''\
<script LANG="ts">
class Store {
  items: (() => void)[] = []
  clear() {}
}
</script>
''')
        self.assertEqual(['clear'], [f.name for f in functions])

    def test_unclosed_template_markup_keeps_the_script(self):
        for template in (
                '<template v-pre>{{</template>\n  <p>{{ value }}</p>',
                "<textarea>{{ '</textarea><template>' }}</textarea>",
                '<svg><text><![CDATA[<template>]]></text></svg>'):
            functions = get_vue_function_list(
                '<template>\n  ' + template + '\n</template>\n'
                '<script>\nfunction real() {\n  return 1\n}\n</script>\n')
            self.assertEqual(['real'], [f.name for f in functions], template)
