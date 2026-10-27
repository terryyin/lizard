# JavaScript and TypeScript arrow boundaries

An expression-bodied arrow in a ternary branch ends at that ternary's separator.
Each branch reports its own function and CCN. Colons inside the arrow's own
expression, object properties or computed keys keep their syntax ownership.
TypeScript parameter and return annotations are consumed as types before a
runtime separator is interpreted. Function-type aliases create no runtime
functions; functions returning a function type retain their real bodies and
following functions.

For example:

```javascript
function choose(strict, b) {
  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;
  return eq;
}
```

| Function | Lines | CCN |
| --- | --- | ---: |
| (anonymous) | 2–2 | 1 |
| (anonymous) | 2–2 | 2 |
| choose | 1–4 | 2 |

The existing naming convention stays: the first branch in `const f = c ? x =>
x : y => y` takes the binding name `f`; a first branch with parenthesized
parameters is anonymous. Internal decisions contribute to the arrow that owns
them; enclosing and following functions retain their own spans and CCN.

Curried arrows keep the existing outer-only reporting convention. In
`const f = x => d ? y => y : z => z || x`, the inner arrows do not create
additional reported functions or replace `f`'s enclosing ternary context.

These rules share the JavaScript, TypeScript, JSX, TSX and Vue script reader
paths. `.cjs` and `.mjs` use the JavaScript reader too. The existing object-comma
limitation remains: a sibling arrow after a comma can be absorbed into the
preceding object-member arrow, even without a ternary.

## Shared reader responsibilities

JavaScriptFunctions owns expression-arrow context and signature-group eligibility;
TypeScriptStates owns runtime ternary separators and completed group evidence.
Ambiguous return annotations use the existing type-annotation recognizer for
lookahead. The foreground token stream still reaches counters once, including
when demand-driven buffering examines future tokens. Bindings, object members
and declarations retain their existing owners.

The default sole canonical root can use direct state-machine dispatch because
binding candidates belong to child states. Children retain their binding-aware
calls, callbacks and parser-only replay. Custom hooks, roots with binding owners,
state subclasses and alternate state containers keep the generic reader protocol;
state finalization precedes `eof` in either path.

Public regression examples live in
[testJavaScriptTernaryArrows.py](../test/test_languages/testJavaScriptTernaryArrows.py).
Reader hooks, configured states, finalization and worker behavior are covered by
[testTypeScriptReaderDispatch.py](../test/test_languages/testTypeScriptReaderDispatch.py).
