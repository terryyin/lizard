import time
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

import lizard
from lizard import FileAnalyzer, analyze_file, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_ext.lizardcpre import LizardExtension as CPreprocessor
from lizard_ext.lizarddumpcomments import LizardExtension as DumpComments
from lizard_ext.lizardnd import LizardExtension as NestingDepth
from lizard_ext.lizardns import LizardExtension as NestedStructures
from lizard_ext.lizardoutside import LizardExtension as Outside
from lizard_languages import js_style_regex_context

COMPONENT = '(component)'


def without_component(functions):
    return [f for f in functions if f.name != COMPONENT]


def get_svelte_function_list(source_code):
    """The functions of a component, without the component unit."""
    return without_component(analyze_file.analyze_source_code(
        "Counter.svelte", source_code).function_list)


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
        self.assertEqual([('increment', 4, 8, 3), ('(anonymous)', 14, 14, 1)],
                         function_summaries(with_markup))
        self.assertEqual(
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.token_count) for f in script_only],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.token_count) for f in with_markup[:1]])

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
            ['increment', COMPONENT],
            [f.name for info in results for f in info.function_list])


def analyze_with_extensions(source_code):
    return FileAnalyzer(get_extensions(
        [NestingDepth(), NestedStructures(), Cognitive()])).analyze_source_code(
            "Component.svelte", source_code)


class TestSvelteMarkup(unittest.TestCase):

    def test_snippets_and_markup_functions(self):
        functions = get_svelte_function_list('''\
<script>
  let { items, onpick } = $props();
</script>

{#snippet row(item, index)}
  {#if item.visible && !item.archived}
    <li class={index % 2 ? 'odd' : 'even'}>
      <button onclick={() => { if (item.ready) onpick(item); }}>{item.name}</button>
    </li>
  {:else if item.placeholder}
    <li>...</li>
  {/if}
{/snippet}

<ul>
  {#each items.filter((item) => item.kind === 'a' || item.pinned) as item, index (item.id)}
    {@render row(item, index)}
  {/each}
</ul>
''')
        self.assertEqual(
            [('(anonymous)', 8, 8, 2), ('row', 5, 13, 5),
             ('(anonymous)', 16, 16, 2)],
            function_summaries(functions))
        self.assertEqual(['item', 'index'], functions[1].parameters)

    def test_blocks_count_like_the_same_script_statements(self):
        functions = get_svelte_function_list('''\
{#snippet list(items, filter)}
  {#each items as item}
    {#if item.ok || filter}
      <p>{item.label ?? 'none'}</p>
    {:else if item.warn}
      <p>warn</p>
    {:else}
      <p>bad</p>
    {/if}
  {:else}
    <p>empty</p>
  {/each}
{/snippet}
{#snippet load(promise)}
  {#await promise}
    <p>...</p>
  {:then value}
    <p>{value}</p>
  {:catch error}
    <p>{error.message}</p>
  {/await}
  {#await promise then v}{v}{/await}
  {#await promise catch e}{e}{/await}
  {#key promise}<p>{promise}</p>{/key}
{/snippet}
''')
        self.assertEqual([('list', 1, 13, 6), ('load', 14, 25, 3)],
                         function_summaries(functions))

    def test_nesting_and_cognitive_complexity_of_blocks(self):
        result = analyze_with_extensions('''\
{#snippet grid(rows)}
  {#each rows as row}
    {#if row.visible}
      <p>{row.a}</p>
    {:else if row.b}
      <p>b</p>
    {/if}
  {/each}
{/snippet}
''')
        grid = result.function_list[0]
        self.assertEqual(
            ('grid', 4, 2, 2),
            (grid.name, grid.cognitive_complexity, grid.max_nesting_depth,
             grid.max_nested_structures))

    def test_top_level_markup_logic_goes_to_the_component(self):
        script = '''\
<script>
  function reset() { count = 0; }
</script>
'''
        markup = '''\
{#if count > 0 && ready}
  {#each items as item}
    <p>{item ? 'a' : 'b'}</p>
  {/each}
{/if}
'''
        script_only = analyze_file.analyze_source_code('C.svelte', script)
        with_markup = analyze_file.analyze_source_code(
            'C.svelte', script + markup)
        self.assertEqual(
            [(f.name, f.cyclomatic_complexity, f.nloc, f.token_count)
             for f in without_component(script_only.function_list)],
            [(f.name, f.cyclomatic_complexity, f.nloc, f.token_count)
             for f in without_component(with_markup.function_list)])
        self.assertEqual(
            [(COMPONENT, 1), (COMPONENT, 5)],
            [(f.name, f.cyclomatic_complexity) for f in (
                script_only.function_list[-1],
                with_markup.function_list[-1])])

    def test_only_markup_lines_with_code_count_as_code(self):
        result = analyze_file.analyze_source_code('Title.svelte', '''\
<script>
  function f() {}
</script>
<h1>Title</h1>
<p class="plain">Plain text, don't count it.</p>
<!-- {#if note} -->
{#snippet a(x)}
  <p>
    {x}
  </p>
{/snippet}
<style>
  p { color: red; }
</style>
''')
        self.assertEqual([('f', 2, 2, 1), ('a', 7, 11, 3)],
                         [(f.name, f.start_line, f.end_line, f.nloc)
                          for f in without_component(result.function_list)])
        self.assertEqual(4, result.nloc)

    def test_attribute_and_directive_expressions(self):
        functions = get_svelte_function_list('''\
{#snippet field(f, disabled)}
  <input
    class="a {f.big ? 'big' : 'small'} c"
    title={f.title || 'none'}
    {disabled}
    {...f.rest}
    on:click|preventDefault={() => f.pick()}
    onkeydown={(e) => e.key === 'Enter' && f.pick()}
    onblur={function (e) { if (e) f.leave(); }}
    bind:value
    class:active={f.on && !f.off}
    use:tooltip={f.hint ?? ''}
    style:color={f.color}
    transition:fade={{ duration: f.fast ? 100 : 200 }}
    data-x=plain
    data-y={f.y}z
  />
{/snippet}
''')
        self.assertEqual(
            [('(anonymous)', 7, 7, 1), ('(anonymous)', 8, 8, 2),
             ('(anonymous)', 9, 9, 2), ('field', 1, 18, 6)],
            function_summaries(functions))

    def test_quotes_and_angle_brackets_in_markup(self):
        functions = get_svelte_function_list('''\
<script>
  function pick(a) { return a ? 1 : 2; }
</script>
<p>Don't stop. It's "fine" {count > 1 ? 's' : ''}</p>
{#snippet s(a)}<p title='a > b'>It's {a > 1 && a < 5} > {a}</p>{/snippet}
<Sorter compare={(a, b) => a > b} />
<p>"</p>
{#snippet t(b)}{b || 0}{/snippet}
''')
        self.assertEqual(
            [('pick', 2, 2, 2), ('s', 5, 5, 2), ('(anonymous)', 6, 6, 1),
             ('t', 8, 8, 2)],
            function_summaries(functions))

    def test_expression_ends_skip_strings_templates_regexes_and_comments(self):
        functions = get_svelte_function_list('''\
{#snippet e(x)}
  <p>{x ? "}" : '{'}</p>
  <p>{`${x ? `}${x}` : '{'}` || x}</p>
  <p>{x.match(/}/) ? 1 : 0}</p>
  <p>{x /* } */ && x}</p>
  <p>{({ a: { b: x } }).a.b ?? x}</p>
  <p>{x // }
    || 1}</p>
{/snippet}
{#snippet after(y)}{#if y}{/if}{/snippet}
''')
        self.assertEqual([('e', 1, 9, 8), ('after', 10, 10, 2)],
                         function_summaries(functions))

    def test_each_heads(self):
        functions = get_svelte_function_list('''\
<script lang="ts">
  type Row = { ok: boolean };
</script>
{#snippet each(rows)}
  {#each rows as { id, label = id ?? 'x', tags: [first] = [] }, i (key(id) || i)}
    <p>{label}</p>
  {/each}
  {#each (rows as Row[]).filter((r) => r.ok) as row}
    <p>{row}</p>
  {/each}
  {#each { length: 3 }}
    <p>dot</p>
  {/each}
{/snippet}
''')
        self.assertEqual([('(anonymous)', 8, 8, 1), ('each', 4, 14, 6)],
                         function_summaries(functions))

    def test_const_tag_names_its_arrow_function(self):
        functions = get_svelte_function_list('''\
{#snippet totals(items)}
  {#each items as item}
    {@const total = () => item.a || item.b}
    <p>{total()}</p>
  {/each}
{/snippet}
''')
        self.assertEqual([('total', 3, 3, 2), ('totals', 1, 6, 2)],
                         function_summaries(functions))

    def test_special_elements_and_snippet_children(self):
        functions = get_svelte_function_list('''\
<svelte:window onkeydown={(e) => e.key === 'Escape' && close()} />
<svelte:head><title>{title || 'App'}</title><script src="x.js"></script></svelte:head>
<svelte:element this={tag} {@attach (node) => { if (node) focus(node); }}></svelte:element>
<svelte:boundary>
  <Child />
  {#snippet failed(error, reset)}
    <button onclick={reset}>{error.message ?? 'oops'}</button>
  {/snippet}
</svelte:boundary>
<Table>
  {#snippet header()}<th>Name</th>{/snippet}
  {#snippet row(item)}{#if item}<td>{item.name}</td>{/if}{/snippet}
</Table>
''')
        self.assertEqual(
            [('(anonymous)', 1, 1, 2), ('(anonymous)', 3, 3, 2),
             ('failed', 6, 8, 2), ('header', 11, 11, 1), ('row', 12, 12, 2)],
            function_summaries(functions))
        self.assertEqual(
            [1, 1, 2, 0, 1], [f.parameter_count for f in functions])

    def test_script_and_style_blocks_between_markup(self):
        functions = get_svelte_function_list('''\
<!-- {#if broken} <script> -->
{#snippet before(a)}{#if a}{/if}{/snippet}
<script module>
  export function helper(x) { return x || 0; }
</script>
<style>
  p { color: red; } .a:is(.b) { }
</style>
{#snippet between(b)}{b && 1}{/snippet}
<script>
  function inst(y) { if (y) {} }
</script>
<!-- } {/snippet} -->
{#snippet after(c)}{c ?? 1}{/snippet}
''')
        self.assertEqual(
            [('before', 2, 2, 2), ('helper', 4, 4, 2), ('between', 9, 9, 2),
             ('inst', 11, 11, 2), ('after', 14, 14, 2)],
            function_summaries(functions))

    def test_svelte_4_directives_slots_and_let(self):
        functions = get_svelte_function_list('''\
<script>
  export let items = [];
  $: total = items.length;
</script>
<List {items} let:item on:select={(e) => e.detail && select(e.detail)}>
  <slot name="row" {item} active={item === current} />
</List>
''')
        self.assertEqual([('(anonymous)', 5, 5, 2)],
                         function_summaries(functions))

    def test_invalid_expressions_and_cut_off_markup_keep_script_functions(self):
        script = '''\
<script>
  function keep(a) { if (a) {} }
</script>
'''
        for markup in (
                '{#if a}\n  {#each items as item}\n    <p>{item.name</p>\n',
                '{#if a}\n  {/each}\n{/snippet}\n{:else if}\n',
                '{#snippet s(a)}{foo(}{a && 1}{/snippet}\n',
                '<p title="{a', '{#snippet s(', '<p {broken</p>'):
            with self.subTest(markup=markup):
                after = get_svelte_function_list(script + markup)
                before = get_svelte_function_list(markup + '\n' + script)
                self.assertIn(('keep', 2, 2, 2), function_summaries(after))
                self.assertIn(('keep', 2), [
                    (f.name, f.cyclomatic_complexity) for f in before])
        # An open comment or expression holds the rest of the file, so these
        # can only end it.
        for markup in ('<div onclick={() => {', '<!-- {'):
            with self.subTest(markup=markup):
                after = get_svelte_function_list(script + markup)
                self.assertIn(('keep', 2, 2, 2), function_summaries(after))

    def test_unbalanced_expression_is_skipped(self):
        functions = get_svelte_function_list('''\
{#snippet s(a)}{foo(}{a && 1}{/snippet}
{#snippet t(b)}{b || 1}{/snippet}
''')
        self.assertEqual([('s', 1, 1, 2), ('t', 2, 2, 2)],
                         function_summaries(functions))

    def test_function_binding_reports_getter_and_setter(self):
        functions = get_svelte_function_list('''\
<input bind:value={
  () => value,
  (v) => { if (v) value = v.toLowerCase(); }
} />
''')
        self.assertEqual([('(anonymous)', 2, 2, 1), ('(anonymous)', 3, 3, 2)],
                         function_summaries(functions))

    def test_script_end_ends_its_last_statement(self):
        script = '''\
<script>
  const add = (binding) =>
    helpers.select(binding, {
      js: true,
    })
</script>
'''
        script_only = get_svelte_function_list(script)
        with_markup = get_svelte_function_list(
            script + '<Modal on:hide={() => { dispatch("change") }} />\n')
        self.assertEqual(
            [('add', 2, 5, 1, 4, 17), ('(anonymous)', 7, 7, 1, 1, 6)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.nloc, f.token_count) for f in with_markup])
        self.assertEqual(
            [(f.name, f.nloc, f.token_count) for f in script_only],
            [(f.name, f.nloc, f.token_count) for f in with_markup[:1]])

    def test_expression_that_starts_with_a_comment(self):
        functions = get_svelte_function_list('''\
{#snippet s(a)}
  <button
    on:click={// comment
    () => {
      if (a) go();
    }}
  >{/* note */ a || 1}</button>
{/snippet}
''')
        self.assertEqual([('(anonymous)', 4, 6, 2), ('s', 1, 8, 2)],
                         function_summaries(functions))

    def test_unclosed_brace_holds_the_rest_of_the_markup(self):
        functions = get_svelte_function_list('''\
{#snippet a(x)}{x}{/snippet}
<p>{oops</p>
{#snippet b(y)}{y}{/snippet}
<script>
  function keep() {}
</script>
{#snippet c(z)}{z}{/snippet}
''')
        self.assertEqual(['a', 'keep', 'c'], [f.name for f in functions])

    def test_regular_expressions_hold_braces_as_text(self):
        functions = get_svelte_function_list('''\
<button onclick={() => { return /}/.test(x) && y; }}>a</button>
<button onclick={() => /{/.test(x) || y}>b</button>
<button onclick={() => x. return / 2 && /{/.test(x)}>c</button>
<button onclick={async () => await /{/.test(x) && x}>d</button>
<button onclick={/x/.test(x) ? () => a && b : undefined}>e</button>
<button onclick={() => x. /* c */ return / 2 && /{/.test(x)}>f</button>
{#snippet s(x)}
  {#if /{/.test(x)}{x}{/if}
  {#if /* why */ /{/.test(x)}{x}{/if}
{/snippet}
{#snippet later()}{x && y}{/snippet}
''')
        self.assertEqual([('(anonymous)', 1, 1, 2), ('(anonymous)', 2, 2, 2),
                          ('(anonymous)', 3, 3, 2), ('(anonymous)', 4, 4, 2),
                          ('(anonymous)', 5, 5, 2), ('(anonymous)', 6, 6, 2),
                          ('s', 7, 10, 3), ('later', 11, 11, 2)],
                         function_summaries(functions))

    def test_regular_expressions_after_comments(self):
        functions = get_svelte_function_list('''\
<button onclick={() => /* note */ /{/.test(x) && y}>a</button>
<button onclick={() => (/* note */ /}/.test(x) && y)}>b</button>
<button onclick={() => { return /* note */ /}/.test(x) && y; }}>c</button>
<button onclick={() => x. // note
  return / 2 && y}>d</button>
{/* note */ /}/.test(x) && (() => y && z)}
{#snippet later()}{x && y}{/snippet}
''')
        self.assertEqual([('(anonymous)', 1, 1, 2), ('(anonymous)', 2, 2, 2),
                          ('(anonymous)', 3, 3, 2), ('(anonymous)', 4, 5, 2),
                          ('(anonymous)', 6, 6, 2), ('later', 7, 7, 2)],
                         function_summaries(functions))

    def test_regular_expression_after_a_line_comment(self):
        for ending in ('\n', '\r', ' ', ' '):
            with self.subTest(ending=ending):
                functions = get_svelte_function_list(
                    '<button onclick={() => { // note' + ending
                    + '/}/.test(x) && y; }}>a</button>\n'
                    '{#snippet later()}{x && y}{/snippet}\n')
                self.assertEqual(
                    [('(anonymous)', 2), ('later', 2)],
                    [(f.name, f.cyclomatic_complexity) for f in functions])

    def test_long_strings_comments_and_regexes_hold_braces(self):
        functions = get_svelte_function_list('''\
{#snippet s(x)}
  <p>{x /*
    } ` }
  */ && y}</p>
  <p>{`${x /* `}`}` + 1} y
  } */}` || z}</p>
  <p>{"}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}" || x}</p>
  <p>{x || /}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}/.test(y)}</p>
{/snippet}
{#snippet later()}{x && y}{/snippet}
''')
        self.assertEqual([('s', 1, 9, 5), ('later', 10, 10, 2)],
                         function_summaries(functions))

    def test_long_runs_of_spaces_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{#snippet f()}{x' + ' ' * 100000 + '&& y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_long_runs_of_line_breaks_after_a_division_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{#snippet f()}{x / 2' + '\n' * 64000 + '&& y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 64001, 2)], function_summaries(functions))

    def test_many_invalid_tags_on_one_line_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '<p>{a \\"}</p>' * 3000 + '\n{#snippet f()}{x && y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 2, 2, 2)], function_summaries(functions))

    def test_division_after_a_postfix_operator(self):
        functions = get_svelte_function_list(
            '{#snippet f()}' + '<p>{i++ / 2}</p>' * 100
            + '<p>{(/a}b/.test(y) && z)}</p>{/snippet}\n')
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_template_in_an_interpolation(self):
        functions = get_svelte_function_list(
            '{#snippet f()}{`${`x`}` + "}" && y}{/snippet}\n'
            '{`${`x`}`\n\n}\n<script>function after() {}</script>\n')
        self.assertEqual([('f', 1, 1, 2), ('after', 5, 5, 1)],
                         function_summaries(functions))

    def test_division_after_a_template_in_an_interpolation(self):
        functions = get_svelte_function_list(
            '{#snippet f()}' + '{`${`x` / "["}`}' * 50
            + '{"}" && y}{/snippet}\n')
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_many_divisions_in_interpolations_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{#snippet f()}' + '{`${`x` / "["}`}' * 3000
            + ']/{x && y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_regular_expression_with_a_backtick_in_an_interpolation(self):
        functions = get_svelte_function_list(
            '{#snippet f()}' + '{`${x + /`a`[/*]/.test(x)}`}' * 50
            + '{"}" && y}{/snippet}\n')
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_many_regexes_in_interpolations_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{#snippet f()}' + '{`${x + /`a`[/*]/.test(x)}`}' * 3000
            + '*/{x && y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_many_divisions_before_braces_in_strings(self):
        functions = get_svelte_function_list(
            '{#snippet f()}' + '{x / 2 && "}" && y}' * 50 + '{/snippet}\n')
        self.assertEqual([('f', 1, 1, 101)], function_summaries(functions))

    def test_many_comment_starts_that_do_not_close(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{x / /a\\/*b}/.test(x)}' * 3000
            + '{#snippet f()}{"}" && y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_comment_cut_before_its_last_character(self):
        functions = get_svelte_function_list(
            '{#snippet f()}{   "}"/*}*/&&y}{/snippet}\n')
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_line_breaks_before_markup_with_slashes_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{#snippet f()}{x / 2 + "{"' + '\n' * 64000
            + '&& y}{/snippet}<p/>}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 1, 64001, 2)], function_summaries(functions))

    def test_many_strings_that_do_not_close_on_one_line(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '{x</\\"}/}' * 3000 + '\n{#snippet f()}{\'}\'&&y}{/snippet}\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('f', 2, 2, 2)], function_summaries(functions))

    def test_regular_expression_cut_after_a_backslash(self):
        functions = get_svelte_function_list(
            '{#snippet f()}{"aaaaa}"+(/[}\\}]/.test(x))&& y}{/snippet}\n')
        self.assertEqual([('f', 1, 1, 2)], function_summaries(functions))

    def test_markup_nested_too_deep_keeps_script_functions(self):
        nested = '`${' * 500 + 'async && /x/' + '}`' * 500
        functions = get_svelte_function_list(
            '<script>function before() {}</script>\n'
            '<p>{' + nested + '}</p>\n'
            '<script>function after() {}</script>\n')
        self.assertEqual(['before', 'after'], [f.name for f in functions])

    def test_named_function_expression_keeps_its_name(self):
        functions = get_svelte_function_list(
            '<button onclick={function named() { if (x) y(); }}>a</button>\n')
        self.assertEqual([('named', 1, 1, 2)], function_summaries(functions))

    def test_number_that_ends_with_a_dot_is_no_member_access(self):
        functions = get_svelte_function_list('''\
<button onclick={() => { 1.
  return /{/.test(x); }}>a</button>
{#snippet later(a, b)}{a && b}{/snippet}
''')
        self.assertEqual(('later', 3, 3, 2), function_summaries(functions)[-1])

    def test_leading_comment_ends_at_its_own_close(self):
        functions = get_svelte_function_list('''\
<button onclick={/* first */ () => x && y}>Go</button>
{#if /* second */ /x/.test(x)}yes{/if}
{#snippet later()}{x && y}{/snippet}
''')
        self.assertEqual([('(anonymous)', 1, 1, 2), ('later', 3, 3, 2)],
                         function_summaries(functions))

    def test_leading_line_comments_end_at_the_line_end(self):
        functions = get_svelte_function_list(
            '<button onclick={// /x\u2028() => x && y}>Go</button>\n'
            '{#snippet later()}{x && y}{/snippet}\n')
        self.assertEqual([('(anonymous)', 1, 1, 2), ('later', 2, 2, 2)],
                         function_summaries(functions))

    def test_many_leading_line_comments_take_linear_time(self):
        started = time.perf_counter()
        functions = get_svelte_function_list(
            '<button onclick={\n' + '// note \n' * 26
            + '() => a && b}>Go</button>\n')
        self.assertLess(time.perf_counter() - started, 1)
        self.assertEqual([('(anonymous)', 28, 28, 2)],
                         function_summaries(functions))

    def test_whitespace_before_the_block_tag_head(self):
        functions = get_svelte_function_list('''\
{ #snippet s(a, b)}
  { #if a}{b}{ :else if b}{a}{ /if}
{ /snippet}
''')
        self.assertEqual([('s', 1, 3, 3)], function_summaries(functions))

    def test_style_ends_only_at_its_own_closing_tag(self):
        result = analyze_file.analyze_source_code('Component.svelte', '''\
<style>p { content: "</style-x>"; } q { color: red; }</style>
''')
        self.assertEqual(0, result.nloc)

    def test_const_tag_has_the_tokens_of_the_declaration(self):
        markup = get_svelte_function_list(
            '{#snippet s(obj)}{@const { a } = obj}{/snippet}\n')
        script = get_svelte_function_list(
            '<script>function s(obj) { const { a } = obj; }</script>\n')
        self.assertEqual([f.token_count for f in script],
                         [f.token_count for f in markup])

    def test_capitalized_and_self_closing_tags_hold_markup(self):
        functions = get_svelte_function_list('''\
<Style />
<button onclick={() => a && b}>a</button>
<Script>{#snippet s()}{c ? 1 : 2}{/snippet}</Script>
''')
        self.assertEqual([('(anonymous)', 2, 2, 2), ('s', 3, 3, 2)],
                         function_summaries(functions))

    def test_closing_tag_closes_the_blocks_opened_inside_it(self):
        result = analyze_with_extensions('''\
{#if a}{#snippet s()}{/if}{/snippet}
<script>
  function g() { if (x) {} }
</script>
''')
        self.assertEqual(
            [('s', 1, 1, 0), ('g', 3, 3, 1)],
            [(f.name, f.start_line, f.end_line, f.max_nested_structures)
             for f in without_component(result.function_list)])

    def test_closing_brace_on_its_own_line_adds_no_code_line(self):
        result = analyze_file.analyze_source_code('Component.svelte', '''\
<p onclick={
  () => x
}>a</p>
''')
        self.assertEqual(
            [('(anonymous)', 2, 2, 1)],
            [(f.name, f.start_line, f.end_line, f.nloc)
             for f in without_component(result.function_list)])
        self.assertEqual(1, result.nloc)

    def test_comment_after_the_code_adds_no_code_line(self):
        functions = get_svelte_function_list('''\
<button onclick={
  () => x
  // why
}>a</button>
<button onclick={
  () => y /* why */
}>b</button>
''')
        self.assertEqual(
            [('(anonymous)', 2, 2, 1), ('(anonymous)', 6, 6, 1)],
            [(f.name, f.start_line, f.end_line, f.nloc) for f in functions])

    def test_repeated_analysis_keeps_no_markup_expression_in_memory(self):
        source = '<button onclick={() => a && b}>Go</button>\n'
        get_svelte_function_list(source)
        cached = len(js_style_regex_context._MARK_CACHE)
        for _ in range(10):
            get_svelte_function_list(source)
        self.assertEqual(cached, len(js_style_regex_context._MARK_CACHE))


def analyze_svelte(source_code, extensions=()):
    return FileAnalyzer(get_extensions(list(extensions))).analyze_source_code(
        "List.svelte", source_code)


def component_of(result):
    components = [f for f in result.function_list if f.name == COMPONENT]
    return components[0] if components else None


def complexity(function):
    return (function.cyclomatic_complexity, function.max_nesting_depth,
            function.max_nested_structures, function.cognitive_complexity)


class TestSvelteComponent(unittest.TestCase):

    def test_component_holds_top_level_script_and_markup(self):
        result = analyze_svelte('''\
<script>
  export let items = []
  export let mode
  $: visible = mode === 'all' ? items : items.filter((i) => i.active)
  $: if (visible.length > 100) console.warn('long list')
</script>

{#if visible.length}
  <ul>
    {#each visible as item}
      <li class={item.done ? 'done' : ''}>{item.name}</li>
    {/each}
  </ul>
{:else if mode === 'all'}
  <p>Nothing here</p>
{/if}
''')
        self.assertEqual(
            [('(anonymous)', 4, 4, 1), (COMPONENT, 2, 16, 7)],
            function_summaries(result.function_list))
        self.assertEqual((10, 0), (component_of(result).nloc,
                                   component_of(result).parameter_count))

    def test_functions_and_snippets_keep_their_own_decision_points(self):
        result = analyze_svelte('''\
<script>
  let { items } = $props();
  const active = items.filter((i) => i.on && !i.hidden);
  function pick(item) {
    if (item.locked) return;
    selected = item;
  }
</script>

{#snippet row(item)}
  {#if item.on}<b>{item.name}</b>{:else}{item.name}{/if}
{/snippet}

<ul>
  {#each active as item}
    <li onclick={() => item.locked || pick(item)}>{@render row(item)}</li>
  {/each}
</ul>
''')
        self.assertEqual(
            [('(anonymous)', 3, 3, 2), ('pick', 4, 7, 2), ('row', 10, 12, 2),
             ('(anonymous)', 16, 16, 2), (COMPONENT, 2, 17, 2)],
            function_summaries(result.function_list))

    def test_svelte_4_reactive_statements(self):
        result = analyze_svelte('''\
<script>
  export let items = [];
  export let filter;
  $: shown = filter ? items.filter((i) => i.on) : items;
  $: if (shown.length > 10) {
    console.warn('many');
  } else if (!shown.length) {
    console.warn('none');
  }
  $: {
    for (const item of shown) total += item.n ?? 0;
  }
</script>
''')
        self.assertEqual(
            [('(anonymous)', 4, 4, 1), (COMPONENT, 2, 12, 6)],
            function_summaries(result.function_list))

    def test_rune_callbacks_are_functions_and_rune_arguments_are_not(self):
        result = analyze_svelte('''\
<script lang="ts">
  let { items, filter } = $props();
  let shown = $derived(filter === 'all' ? items : items.filter((i) => i.on));
  const total = $derived.by(() => {
    let sum = 0;
    for (const i of shown) sum += i.n || 0;
    return sum;
  });
  $effect(() => {
    if (total > 10) console.log(total);
  });
</script>
''')
        self.assertEqual(
            [('(anonymous)', 3, 3, 1), ('(anonymous)', 4, 8, 3),
             ('(anonymous)', 9, 11, 2), (COMPONENT, 2, 11, 2)],
            function_summaries(result.function_list))

    def test_prop_defaults_count_like_destructuring_defaults(self):
        script = "let { size = 'md', disabled = false, ...rest } = $props();"
        component = component_of(analyze_svelte(
            '<script>\n  ' + script + '\n</script>\n'))
        javascript = analyze_file.analyze_source_code(
            'a.js', 'function f() {\n  ' + script + '\n}\n').function_list[0]
        self.assertEqual(3, component.cyclomatic_complexity)
        self.assertEqual(javascript.cyclomatic_complexity,
                         component.cyclomatic_complexity)

    def test_top_level_statements(self):
        result = analyze_svelte('''\
<script>
  import { load } from './api';
  let rows = [];
  try {
    rows = await load();
  } catch (error) {
    rows = [];
  }
  outer: for (const row of rows) {
    for (const cell of row) {
      if (!cell) continue outer;
    }
  }
  while (rows.length > 100) rows.pop();
</script>
''')
        self.assertEqual([(COMPONENT, 2, 14, 6)],
                         function_summaries(result.function_list))

    def test_markup_between_and_after_snippets(self):
        result = analyze_svelte('''\
{#if user}<p>{user.name}</p>{/if}
{#snippet row(item)}
  {#if item.done}<s>{item.name}</s>{:else}{item.name}{/if}
{/snippet}
<Table rows={items}>
  {#snippet header()}
    {#each columns as column}<th>{column}</th>{/each}
  {/snippet}
  {#if loading}<p>...</p>{/if}
</Table>
{#each items as item}{@render row(item)}{/each}
''')
        self.assertEqual(
            [('row', 2, 4, 2), ('header', 6, 8, 2), (COMPONENT, 1, 11, 4)],
            function_summaries(result.function_list))

    def test_module_script_is_not_part_of_the_component(self):
        source = '''\
<script module>
  export const sizes = small ? [1, 2] : [3];
  if (import.meta.env.DEV) console.log('dev');
  export function label(size) { return size || 'none'; }
</script>
<script>
  let { size } = $props();
</script>
<p>{size > 1 ? 'big' : 'small'}</p>
'''
        self.assertEqual(
            [('label', 4, 4, 2), (COMPONENT, 7, 9, 2)],
            function_summaries(analyze_svelte(source).function_list))
        self.assertEqual(
            [('label', 4, 4, 2), (COMPONENT, 7, 9, 2), ('*global*', 0, 4, 3)],
            function_summaries(
                analyze_svelte(source, [Outside()]).function_list))

    def test_legacy_module_script_after_the_markup(self):
        result = analyze_svelte('''\
<script>
  export let size;
</script>
<p>{size > 1 ? 'big' : 'small'}</p>
<script context="module">
  export const sizes = small ? [1, 2] : [3];
</script>
''')
        self.assertEqual([(COMPONENT, 2, 4, 2)],
                         function_summaries(result.function_list))

    def test_files_with_and_without_component_code(self):
        cases = (
            ('', []),
            ('<h1>Home</h1>\n<style>h1 { color: red; }</style>\n', []),
            ('<script module>\n  export const a = b || c;\n</script>\n', []),
            ("<h1>{title ?? 'Home'}</h1>\n", [(COMPONENT, 1, 1, 2)]),
            ('<script>\n  let a = b || c;\n</script>\n',
             [(COMPONENT, 2, 2, 2)]),
        )
        for source, expected in cases:
            with self.subTest(source=source):
                self.assertEqual(expected, function_summaries(
                    analyze_svelte(source).function_list))

    def test_lines_and_tokens_are_those_of_the_component_code(self):
        result = analyze_svelte('''\
<script>
  let x = 1;
</script>

<p>{x ? 'a' : 'b'}</p>
<style>p { color: red; }</style>
''')
        component = component_of(result)
        self.assertEqual((2, 5, 2, 11), (
            component.start_line, component.end_line, component.nloc,
            component.token_count))
        self.assertEqual(2, result.nloc)

    def test_component_ends_with_its_last_function_or_snippet(self):
        snippet = component_of(analyze_svelte('''\
<p>{a}</p>
{#snippet s(x)}
  {x}
{/snippet}
'''))
        function = component_of(analyze_svelte('''\
<script>
  let a = 1;
  function f() {
    return a;
  }
</script>
<p>static</p>
'''))
        self.assertEqual([(1, 4), (2, 5)], [
            (snippet.start_line, snippet.end_line),
            (function.start_line, function.end_line)])

    def test_nesting_and_cognitive_complexity_as_in_a_function(self):
        extensions = [NestingDepth(), NestedStructures(), Cognitive()]
        markup = '''\
{#if a}
  {#each items as item}
    {#if item.ok && ready}<p>{item.x ? 1 : 2}</p>{/if}
  {/each}
{:else if b}
  <p>b</p>
{/if}
'''
        script = '''\
  $: if (a) {
    for (const i of items) {
      if (i && b) total += i ? 1 : 2;
    }
  }
  try { await load(); } catch (e) { failed = true; }
'''
        top_level = analyze_svelte(markup, extensions)
        in_snippet = analyze_svelte(
            '{#snippet c()}\n' + markup + '{/snippet}\n', extensions)
        self.assertEqual(complexity(in_snippet.function_list[0]),
                         complexity(component_of(top_level)))
        top_level = analyze_svelte(
            '<script>\n' + script + '</script>\n', extensions)
        in_function = analyze_svelte(
            '<script>\nfunction c() {\n' + script + '}\n</script>\n',
            extensions)
        self.assertEqual(complexity(in_function.function_list[0]),
                         complexity(component_of(top_level)))
        self.assertEqual(7, component_of(top_level).cyclomatic_complexity)

    def test_forgive_global_forgives_the_component(self):
        result = analyze_svelte('''\
<script>
  // #lizard forgive global
  let x = a ? 1 : 2;
  function keep(y) { return y || 0; }
</script>
<p>{x && y}</p>
''')
        self.assertEqual([('keep', 4, 4, 2)],
                         function_summaries(result.function_list))

    def test_cut_off_component_is_reported_once_after_the_functions(self):
        source = '''\
<script module>
  export const k = a ? 1 : 2;
</script>
<script>
  let { items } = $props();
  $: if (items) go(() => items.length > 1);
  function pick(item) { if (item) select(item); }
</script>
{#snippet row(item)}{#if item}{item}{/if}{/snippet}
<ul>{#each items as item}<li onclick={() => pick(item)}>{@render row(item)}</li>{/each}</ul>
'''
        for end in range(len(source) + 1):
            for extensions in ((), (Outside(),)):
                with self.subTest(end=end, outside=bool(extensions)):
                    names = [f.name for f in analyze_svelte(
                        source[:end], extensions).function_list
                             if f.name != '*global*']
                    self.assertLessEqual(names.count(COMPONENT), 1)
                    if COMPONENT in names and not extensions:
                        self.assertEqual(COMPONENT, names[-1])

    def test_vue_reports_no_component(self):
        functions = analyze_file.analyze_source_code('a.vue', '''\
<template>
  <p v-if="a && b">{{ a ? 1 : 2 }}</p>
</template>
<script setup>
const x = a ? 1 : 2;
</script>
''').function_list
        self.assertEqual([], functions)

    def test_lookahead_does_not_move_tokens_between_units(self):
        instance = analyze_svelte('''\
<script>
  const ready = true ? (1) : 2
</script>
''', [Outside()])
        module = analyze_svelte('''\
<script module>
  const ready = true ? (1) : 2
</script>
<script>
  const name = 'Svelte';
</script>
''')
        self.assertEqual(
            [(COMPONENT, 2, 2, 2, 10), ('*global*', 1),
             (COMPONENT, 5, 5, 1, 5)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.token_count) for f in instance.function_list[:1]]
            + [(f.name, f.token_count) for f in instance.function_list[1:]]
            + [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
                f.token_count) for f in module.function_list])

    def test_cut_off_function_ends_before_the_component(self):
        source = '''\
<script>
  function update() {
    if (ready) run();
'''
        self.assertEqual(
            [('update', 2, 3, 2), (COMPONENT, 2, 3, 1)],
            function_summaries(analyze_svelte(source).function_list))
        self.assertEqual(
            ['update', COMPONENT, '*global*'],
            [f.name for f in analyze_svelte(
                source, [Outside()]).function_list])

    def test_open_outer_functions_are_not_reported_as_in_a_script(self):
        sources = ['''\
<script module>if (a) a();</script>
<script>
  function f() { function g() {
''', '''\
<script module>if (a) a();</script>
<script>
  function f() {
    function g() {
      // GENERATED CODE
    }
  }
</script>
''']
        self.assertEqual([['g', COMPONENT]] * 2, [
            [f.name for f in analyze_svelte(source).function_list]
            for source in sources])
        self.assertEqual([[('g', 1), (COMPONENT, 1), ('*global*', 2)]] * 2, [
            [(f.name, f.cyclomatic_complexity)
             for f in analyze_svelte(source, [Outside()]).function_list]
            for source in sources])

    def test_component_ends_at_its_last_line_of_code(self):
        component = component_of(analyze_svelte('''\
<script>
  const visible = (
    true && false
  )
</script>
'''))
        self.assertEqual((2, 4, 3), (component.start_line, component.end_line,
                                     component.nloc))

    def test_line_shared_with_the_module_script_counts_for_both(self):
        sources = [
            '<script module>const shared = true;</script>{shared ? 1 : 2}\n',
            '{a}<script module>let m;</script>{b ? 1 : 2}\n',
            '{a}<script module></script>{b ? 1 : 2}\n',
            '<script module>m</script>{a}<script module>n</script>{b ? 1 : 2}\n',
        ]
        self.assertEqual([(COMPONENT, 1, 1, 2)] * 4, [
            (f.name, f.start_line, f.nloc, f.cyclomatic_complexity)
            for source in sources
            for f in analyze_svelte(source).function_list])

    def test_shared_line_that_another_function_counts_is_the_component_line(self):
        tail = '<script module>let m;</script>{b ? 1 : 2}\n'
        sources = [
            '{#snippet s()}\n{/snippet}' + tail,
            '{#snippet s()}{`a\nb`}{/snippet}' + tail,
            '<script>const x = (\na)</script>' + tail,
        ]
        self.assertEqual([(1, 2, 2, 2)] * 3, [
            (component.start_line, component.end_line, component.nloc,
             component.cyclomatic_complexity)
            for component in (component_of(analyze_svelte(source))
                              for source in sources)])

    def test_shared_line_after_an_open_module_function(self):
        component = component_of(analyze_svelte(
            '{x}\n{y}\n{z}\n<script module>function f() {</script>{a}'
            '<script module>}</script>{b ? 1 : 2}\n'))
        self.assertEqual((1, 4, 4, 2), (
            component.start_line, component.end_line, component.nloc,
            component.cyclomatic_complexity))

    def test_module_attributes_as_the_svelte_compiler_reads_them(self):
        sources = [
            '<script MODULE>if (a) f()</script>\n',
            '<script Context="module">if (a) f()</script>\n',
            '<script context="modul&#101;">if (a) f()</script>\n',
            '<script context="modul&#' + '0' * 5000 + '101;">if (a) f()</script>\n',
            '<script context="&#' + '9' * 5000 + ';">if (a) f()</script>\n',
        ]
        self.assertEqual([
            [(COMPONENT, 2), ('*global*', 1)],
            [(COMPONENT, 2), ('*global*', 1)],
            [('*global*', 2)],
            [('*global*', 2)],
            [(COMPONENT, 2), ('*global*', 1)],
        ], [
            [(f.name, f.cyclomatic_complexity)
             for f in analyze_svelte(source, [Outside()]).function_list]
            for source in sources])

    def test_a_script_end_ends_its_last_statement(self):
        sources = [
            '<script>if (a) f()</script>{#if b}yes{/if}\n',
            '<script>if (a) f()\n</script>{#if b}yes{/if}\n',
            '<script module>if (a) f()</script>\n{#if b}yes{/if}\n',
            '<script>if (a) f()</script>{(b) && c}\n',
            '<script lang="ts">if (a) f<string></script>{#if b}yes{/if}\n',
            '<script>if (a) f()</script>\n<script module>let m;</script>\n'
            '{#if b}yes{/if}\n',
        ]
        self.assertEqual([(1, 1)] * 6, [
            complexity(component_of(analyze_svelte(
                source, [NestingDepth(), NestedStructures()])))[1:3]
            for source in sources])

    def test_code_after_a_script_end_nests_as_in_a_script(self):
        component = component_of(analyze_svelte(
            '<script module>let m;</script>\n<script>\nif (a) if (b) f()\n</script>\n',
            [NestingDepth(), NestedStructures()]))
        self.assertEqual((2, 2), complexity(component)[1:3])

    def test_generated_code_ends_the_component(self):
        source = '''\
<script>
  let x = a ? 1 : 2;
  // GENERATED CODE
  let generated = b ? 1 : 2;
</script>
'''
        self.assertEqual(
            [[(COMPONENT, 2, 2, 2)], [(COMPONENT, 2, 2, 2), ('*global*', 0, 0, 1)]],
            [function_summaries(analyze_svelte(source, extensions).function_list)
             for extensions in ((), (Outside(),))])

    def test_comments_before_preprocessing_are_those_of_the_file(self):
        source = '''\
<script>
  // Copyright Example
  let x; // note
</script>
{x}
'''
        with patch('builtins.print') as printed:
            analyze_svelte(source, [CPreprocessor(), DumpComments()])
        self.assertEqual([' note'], [
            call.args[0] for call in printed.call_args_list][3:])

    def test_lines_of_the_first_and_last_code(self):
        markup = component_of(analyze_svelte('{a ? 1 : 2}\n'))
        script = component_of(analyze_svelte('''\
<script>
  let a = 1;
  /* note
  more */
</script>
'''))
        separator = component_of(analyze_svelte(
            '<script>let x;/* one\u2028two */</script>\n'))
        tag = component_of(analyze_svelte('''\
<script
  lang="ts">
  if (ready) start();
</script>
'''))
        self.assertEqual([(1, 1, 1), (2, 2, 1), (1, 1, 1), (3, 3, 1)], [
            (f.start_line, f.end_line, f.nloc)
            for f in (markup, script, separator, tag)])

    def test_markup_after_an_unclosed_module_function_is_in_it(self):
        result = analyze_svelte(
            '<script module>function f() {</script>{a ? 1 : 2}\n')
        self.assertEqual([('f', 1, 1, 2)],
                         function_summaries(result.function_list))
