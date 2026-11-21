import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

import lizard
from lizard import analyze_file


def get_svelte_function_list(source_code):
    return analyze_file.analyze_source_code(
        "Counter.svelte", source_code).function_list


def function_summaries(functions):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in functions]


COUNTER_SCRIPT = '''\
<script>
  let count = 0;

  const increment = (step) => {
    if (step > 0 && count < 10) {
      count += step;
    }
  };
</script>
'''

COUNTER_MARKUP = '''
{#if count > 0}
  <ul>
    {#each items as item (item.id)}
      <li onclick={() => count++}>{item.name} isn't {count > 1 ? 'many' : 'one'}</li>
    {/each}
  </ul>
{:else}
  <p>Nothing yet: { count }</p>
{/if}

<style>
  ul { margin: 0; }
</style>
'''


class TestSvelte(unittest.TestCase):

    def test_reports_script_functions_with_original_lines(self):
        functions = get_svelte_function_list(COUNTER_SCRIPT)
        self.assertEqual([('increment', 4, 8, 3)],
                         function_summaries(functions))

    def test_markup_after_the_script_keeps_script_results(self):
        script_only = get_svelte_function_list(COUNTER_SCRIPT)
        with_markup = get_svelte_function_list(COUNTER_SCRIPT + COUNTER_MARKUP)
        self.assertEqual([('increment', 4, 8, 3)],
                         function_summaries(with_markup))
        self.assertEqual(
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.token_count) for f in script_only],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.token_count) for f in with_markup])

    def test_module_script_and_instance_script(self):
        functions = get_svelte_function_list('''\
<script module>
  export function format(value) {
    return value > 0 ? '+' + value : String(value);
  }
</script>

<script>
  const reset = () => {
    count = 0;
  };
</script>
''')
        self.assertEqual([('format', 2, 4, 2), ('reset', 8, 10, 1)],
                         function_summaries(functions))

    def test_legacy_context_module_script(self):
        functions = get_svelte_function_list('''\
<script context="module">
  export const preload = async ({ params }) => {
    if (!params.id) return null;
    return params.id;
  };
</script>
<script>
  export let id;
</script>
''')
        self.assertEqual([('preload', 2, 5, 2)],
                         function_summaries(functions))

    def test_generics_attribute_and_apostrophe_before_script(self):
        functions = get_svelte_function_list('''\
<!-- Don't move this component without its store -->
<script lang="ts" generics="T extends Record<string, any>">
  import { onMount } from "svelte";
  export let items: T[] = [];
  const pick = (a: T, b: T) => {
    if (a && b) return "both";
    return "one";
  };
</script>
''')
        self.assertEqual([('pick', 5, 8, 3)], function_summaries(functions))

    def test_script_text_in_a_brace_attribute_is_not_code(self):
        functions = get_svelte_function_list('''\
<Preview markup={"<script>function ghost() {}</script>"} />
<script>
  const reset = () => {
    count = 0;
  };
</script>
''')
        self.assertEqual([('reset', 3, 5, 1)], function_summaries(functions))

    def test_typescript_script_reads_class_field_annotations(self):
        typed = get_svelte_function_list(
            '<script lang="ts">'
            'class Store { items: (() => void)[] = []; clear() {} }'
            '</script>')
        untyped = get_svelte_function_list(
            '<script>'
            'class Store { items:(x)=>x; clear() {} }'
            '</script>')
        self.assertEqual(['clear'], [f.name for f in typed])
        self.assertEqual(['items', 'clear'], [f.name for f in untyped])

    def test_reactive_statements_stay_outside_functions(self):
        functions = get_svelte_function_list('''\
<script>
  let count = 0;
  $: doubled = count * 2;
  $: if (count > 5) {
    alert('too many');
  }
  function reset() {
    count = 0;
  }
</script>
''')
        self.assertEqual([('reset', 7, 9, 1)], function_summaries(functions))

    def test_rune_callbacks_are_anonymous_functions(self):
        functions = get_svelte_function_list('''\
<script lang="ts">
  let count = $state(0);
  const total = $derived.by(() => {
    let sum = 0;
    for (const n of numbers) sum += n;
    return sum;
  });
  $effect(() => {
    if (count > 3) console.log(count);
  });
</script>
''')
        self.assertEqual([('(anonymous)', 3, 7, 2), ('(anonymous)', 8, 10, 2)],
                         function_summaries(functions))

    def test_directory_scan_includes_svelte_components(self):
        with TemporaryDirectory() as directory:
            Path(directory, 'Counter.svelte').write_text(COUNTER_SCRIPT)
            results = list(lizard.analyze([directory]))
        self.assertEqual(
            ['increment'],
            [f.name for info in results for f in info.function_list])
