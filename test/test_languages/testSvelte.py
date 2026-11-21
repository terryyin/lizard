import time
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

import lizard
from lizard import FileAnalyzer, analyze_file, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_ext.lizardnd import LizardExtension as NestingDepth
from lizard_ext.lizardns import LizardExtension as NestedStructures
from lizard_languages import js_style_regex_context


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
            ['increment'],
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

    def test_top_level_markup_logic_is_outside_functions(self):
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
        script_only = get_svelte_function_list(script)
        with_markup = get_svelte_function_list(script + markup)
        self.assertEqual(
            [(f.name, f.cyclomatic_complexity, f.nloc, f.token_count)
             for f in script_only],
            [(f.name, f.cyclomatic_complexity, f.nloc, f.token_count)
             for f in with_markup])

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
                          for f in result.function_list])
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
             for f in result.function_list])

    def test_closing_brace_on_its_own_line_adds_no_code_line(self):
        result = analyze_file.analyze_source_code('Component.svelte', '''\
<p onclick={
  () => x
}>a</p>
''')
        self.assertEqual(
            [('(anonymous)', 2, 2, 1)],
            [(f.name, f.start_line, f.end_line, f.nloc)
             for f in result.function_list])
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
