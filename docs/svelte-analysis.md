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

## Complexity

Inside a function or a snippet, the block tags count like the script
statements that they stand for:

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

## Lines and tokens

A markup line with an expression or a block tag counts as one line of code.
Plain text, tags, static attributes, `<!-- comments -->` and `<style>`
content add no lines and no tokens. A block tag adds the tokens of the
statement that it stands for, and an expression adds its own tokens and a
`;`.

## Limits

- Logic in the markup outside every snippet and function, such as a
  top-level `{#if}`, belongs to no reported function. Top-level script
  statements behave the same way.
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
- To find the script blocks, lizard ends a `{...}` attribute value in a
  start tag at its first `}`. So a value that does not close, as in
  `<p attr={oops`, hides a script block after it, and that block's
  functions are lost.
- The JavaScript tokenizer finds the `}` that ends an expression, so braces
  in strings, comments, templates and regular expressions do not end it. To
  keep the time linear, it reads at most twice the markup again for this.
  On invalid markup that reaches this bound, such as many strings that do
  not close, each expression then ends at its first `}`.
