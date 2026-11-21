# Svelte analysis

Lizard analyzes `.svelte` files, or `-l svelte`. The `<script>` blocks are
read as in a Vue component. The markup around them is read too: its
expressions and its block tags go through the same tokenizer and state
machine as the script.

## Functions

- Script functions are reported as in a `.js` or `.ts` file.
- A function expression or an arrow function in the markup is reported like
  a callback in a script. This covers attributes, directives, `{...}` tags
  and block heads. It is `(anonymous)`, unless it has a name of its own, as
  in `onclick={function save() {}}`, or `{@const name = ...}` gives it one.
- `{#snippet name(params)}` is a function `name` with those parameters. It
  starts at its `{#snippet}` line and ends at its `{/snippet}` line.

Line numbers are the lines of the `.svelte` file.

## The component

The component itself is one more function, `(component)`. It holds the code
outside every function and snippet: the top-level statements of the
instance script (imports, declarations, `$:` statements, `$derived(...)`,
plain `if` and `for`) and the top-level markup. The Svelte compiler turns
this code into the function of the component.

- Functions, callbacks and snippets in the component stay functions of
  their own, and their decision points count only for them, as for nested
  functions in a script. So an `$effect(() => ...)` or `$derived.by(...)`
  callback is a function, as the same callback is in a `.js` file.
- Markup inside a snippet belongs to the snippet. Markup between the tags
  of a component, outside `{#snippet}`, belongs to the component.
- The module script (`<script module>` or `<script context="module">`)
  runs once for all instances, so its top-level code is not part of the
  component. As in the Svelte compiler, the attribute names are case
  sensitive. It stays outside every function, as top-level code does in a
  script. `-Eoutside` reports it as `*global*`.
- The component starts at its first line of code. It ends at its last
  line of code, or at the end of the last function or snippet in it.
- NLOC and the token count are those of the code that the component holds,
  as for an outer function in a script. A line that starts a function
  counts for both. So does a line with code of the component and of the
  module script.
- When a file ends inside a function, the function ends first, and the
  component comes after it. A function that is still open around it is
  not reported, as in a script.
- A file without code in the instance script or the markup, such as static
  markup or only a module script, has no component.
- `#lizard forgive global` forgives the component.
- A default value in a destructuring declaration adds one, as in a script.
  So `let { size = 'md' } = $props();` adds one.

## Complexity

The block tags count like the script statements that they stand for:

| Markup | Read as | CCN |
|---|---|---|
| `{#if a}` | `if (a) {` | +1 |
| `{:else if b}` | `} else if (b) {` | +1 |
| `{:else}` | `} else {` | 0 |
| `{#each list as item, i (key)}` | `for (list; item, i (key)) {` | +1 |
| `{#await p}` | `try { p;` | 0 |
| `{:then v}` | `v;` | 0 |
| `{:catch e}` | `} catch (e) {` | +1 |
| `{#key k}` | `k;` | 0 |
| `{/if}`, `{/each}`, `{/await}`, `{/snippet}` | `}` | 0 |
| `{expr}`, `{@html expr}`, `{@render expr}`, `{@attach expr}` | `expr;` | 0 |

`&&`, `||`, `??` and `?:` in any markup expression count as in a script.
`{#await p then v}` and `{#await p catch e}` are read as the long forms.

Nesting depth (`-END`), nested structures (`-ENS`) and cognitive complexity
(`-Ecognitive`) see the same statements. So an `{#if}` inside an `{#each}`
nests, and `{:else if}` does not. The `{:else}` of an `{#each}` counts like
the `else` of an `if`.
The end of a script block ends its last statement, as a `;` does, so the
code after `</script>` does not nest in it.

## Lines and tokens

A markup line with an expression or a block tag counts as one line of code.
Plain text, tags, static attributes, `<!-- comments -->` and `<style>`
content add no lines and no tokens. A block tag adds the tokens of the
statement that it stands for, and an expression adds its own tokens and a
`;`.

## Limits

- The rules of the JavaScript reader apply to markup expressions too. In
  `() => () => x` only the outer arrow is reported. An arrow function inside
  a template literal `${...}` is not reported. A default parameter value
  adds one, and `??=` counts like `??`.
- Invalid markup does not stop the analysis. An unmatched closing tag is
  ignored. A closing tag also closes the blocks opened inside it. The blocks
  that are still open close before the next script block. An expression
  whose brackets do not match is skipped. A `{` that does not close holds
  the rest of the markup up to the next script block, as in the Svelte
  compiler.
- A `<script>` tag outside every element is read as a script block, as at
  the top level. This applies inside a block tag such as `{#if}`, and in a
  string, a template or a comment of a markup expression. The Svelte
  compiler reads such a tag as an element or as text.
- The end of a script block ends a function as a line break does. So a
  TypeScript arrow function whose body ends with a type argument, as in
  `() => Array<string>`, holds the markup after `</script>`, as it holds
  the next line in a `.ts` file.
- Cognitive complexity reads markup code after `</script>` on the same line
  as part of the last statement of the script, when that statement has no
  `;`.
- To find the script blocks, lizard ends a `{...}` attribute value in a
  start tag at its first `}`. So a value that does not close, as in
  `<p attr={oops`, hides a script block after it, and that block's
  functions are lost.
- The JavaScript tokenizer finds the `}` that ends an expression, so braces
  in strings, comments, templates and regular expressions do not end it. To
  keep the time linear, it reads up to the `}` that a plain brace count
  gives. It reads further only when a string, a comment, a template or a
  regular expression crosses that point.
