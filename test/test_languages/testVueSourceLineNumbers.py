import unittest
from lizard import analyze_file

from .testVue import get_vue_function_list


def get_vue_file_info(source_code):
    return analyze_file.analyze_source_code("a.vue", source_code)


def function_metrics(functions):
    return [
        (f.name, f.nloc, f.cyclomatic_complexity, f.token_count, f.parameter_count)
        for f in functions
    ]


def function_ranges(functions):
    return [(f.name, f.start_line, f.end_line) for f in functions]


TEMPLATE_FIRST_SCRIPT = '''\
function first() {
  if (x) return 1;
  return 2;
}

function second() {
  return 3;
}
'''

TEMPLATE_FIRST = '''\
<template>
  <div>
    Hello
  </div>
</template>

<script>
''' + TEMPLATE_FIRST_SCRIPT + '''\
</script>
'''

FOUR_LINE_PREFIX_SHIFT = '''\
<template>
  <div>
    Hello
  </div>
</template>





<script>
''' + TEMPLATE_FIRST_SCRIPT + '''\
</script>
'''

SCRIPT_FIRST = '''\
<script>
''' + TEMPLATE_FIRST_SCRIPT + '''\
</script>
'''

TYPESCRIPT_TEMPLATE_FIRST = '''\
<template>
  <div>
    Hello
  </div>
</template>

<script lang="ts">
function first(): number {
  if (x) return 1;
  return 2;
}

function second(): number {
  return 3;
}
</script>
'''

MULTILINE_IGNORED_ATTRIBUTE = '''\
<template>
  <div
    class="foo
bar"
  >
  </div>
</template>

<script>
function first() {
  if (x) return 1;
  return 2;
}
</script>
'''

MULTIPLE_SCRIPTS = '''\
<script>
function first() {
  return 1;
}
</script>
<style>
.foo {
  color: red;
}
.bar {
  color: blue;
}
</style>
<template>
  <div>
    x
  </div>
</template>

<script>
function third() {
  return 3;
}
</script>
'''

NO_SCRIPT = '''\
<template>
  <div>Hello</div>
</template>
<style>
.foo { color: red; }
</style>
'''


class TestVueSourceLineNumbers(unittest.TestCase):

    def test_template_first_reports_original_function_ranges(self):
        functions = get_vue_function_list(TEMPLATE_FIRST)
        self.assertEqual(
            [("first", 8, 11), ("second", 13, 15)],
            function_ranges(functions),
        )

    def test_four_line_prefix_shifts_original_ranges(self):
        functions = get_vue_function_list(FOUR_LINE_PREFIX_SHIFT)
        self.assertEqual(
            [("first", 12, 15), ("second", 17, 19)],
            function_ranges(functions),
        )

    def test_script_first_keeps_correct_ranges_and_metrics(self):
        info = get_vue_file_info(SCRIPT_FIRST)
        functions = info.function_list
        self.assertEqual(
            [("first", 2, 5), ("second", 7, 9)],
            function_ranges(functions),
        )
        self.assertEqual(
            [("first", 4, 2, 14, 0), ("second", 3, 1, 7, 0)],
            function_metrics(functions),
        )

    def test_template_first_preserves_script_metrics(self):
        template_first = get_vue_function_list(TEMPLATE_FIRST)
        script_first = get_vue_function_list(SCRIPT_FIRST)
        self.assertEqual(function_metrics(script_first), function_metrics(template_first))
        shifted = get_vue_function_list(FOUR_LINE_PREFIX_SHIFT)
        self.assertEqual(function_metrics(script_first), function_metrics(shifted))

    def test_typescript_script_keeps_original_ranges(self):
        functions = get_vue_function_list(TYPESCRIPT_TEMPLATE_FIRST)
        self.assertEqual(
            [("first", 8, 11), ("second", 13, 15)],
            function_ranges(functions),
        )
        self.assertEqual(
            [("first", 4, 2, 16, 0), ("second", 3, 1, 9, 0)],
            function_metrics(functions),
        )

    def test_ignored_multiline_attribute_preserves_following_range(self):
        functions = get_vue_function_list(MULTILINE_IGNORED_ATTRIBUTE)
        self.assertEqual([("first", 10, 13)], function_ranges(functions))
        self.assertEqual([("first", 4, 2, 14, 0)], function_metrics(functions))

    def test_multiple_scripts_preserve_later_function_range(self):
        functions = get_vue_function_list(MULTIPLE_SCRIPTS)
        self.assertEqual(
            [("first", 2, 4), ("third", 21, 23)],
            function_ranges(functions),
        )
        self.assertEqual(
            [("first", 3, 1, 7, 0), ("third", 3, 1, 7, 0)],
            function_metrics(functions),
        )

    def test_no_script_has_no_functions(self):
        info = get_vue_file_info(NO_SCRIPT)
        self.assertEqual([], info.function_list)
        self.assertEqual(0, info.nloc)
