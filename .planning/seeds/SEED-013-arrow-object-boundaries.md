---
id: SEED-013
status: active
planted: 2026-10-27
planted_during: FabioMalpezzi PR review, PR531 disposition
trigger_when: Correct object-arrow and function-type body recognition
scope: medium
---

# SEED-013: Arrow object boundaries

<a id="arrow-object-boundaries"></a>

### Preserve arrow object expressions and typed function bodies

**Identity:** SEED-013#arrow-object-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/009-arrow-object-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"9a0e00646657bceae51f991a4f147d22d4b0253dd82a76bfb9571a059ad7b681","plan":"61880560c5ec8b52464689c0671e005d5a08213b381e23f1e344afb17c62d28e"}}
```

**Beneficiary:** Users measuring JavaScript, TypeScript, JSX/TSX and Vue whose
object expressions currently distort function boundaries and metrics.

**Goal:** Public analysis recognizes objects inside expression-bodied arrows and
preserves real function bodies after function-type return annotations, so
reported function names, start/end lines and CCN—including following
functions—match the source.

**Source:** [PR531](https://github.com/terryyin/lizard/pull/531), authored head
`47aa35735cccba962e507edb179eae1154ab6ad2`, original base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`, reviewed against current target
`7ac449f59b3f155fd159ce9aad6794a422045632`. The report is real and worthwhile;
the raw proposal introduces a valid TypeScript regression and leaves related
object operands broken. Correction and cohesive tests were deferred above the
review mission's fifteen-minute cutoff. No product correction was attempted;
the PR remains unmerged. Naming and metric meanings follow
[ADR-0001](../../docs/adrs/0001-ubiquitous-language-accepted.md); existing
facade, core and reader ownership stay unchanged.

**Key examples:**

These situations define required public behavior through `analyze_file` (and
equivalent CLI), not an exhaustive grammar. The
[public API reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py)
checks explicit names, start/end lines and CCN for the frozen 64 retained
inputs across reachable consumers.

```js
function layout(vertical) {
  const box = f => vertical ? {x: f.a, y: f.b} : {x: f.b, y: f.a};
  return box;
}
function after() { return 1; }
```

Pre-condition: expression-bodied arrow whose body is a ternary of object
literals. Trigger: analyze the file. Result: `box` ends on line 2, `layout` on
line 4, `after` on line 5. Current target ends `box` on line 4 and `layout` on
line 5. The raw proposal fixes this in `.js`, `.ts`, `.jsx`, `.tsx` and a real
Vue script block.

```ts
function filter(value: Json): (event: Obj) => "ok" {
  if (value) return () => "ok";
  return () => "ok";
}
function after(): void {}
```

Pre-condition: function whose return type is itself a function type, with
runtime arrows in the body. Trigger: analyze as TypeScript (and TSX / Vue
script). Result: the two runtime arrows are anonymous on lines 2 and 3;
`filter` spans lines 1–4 with CCN 2; `after` remains on line 5. Target already
has those results. The raw proposal creates two functions named `return` and
extends `filter` through line 5. A template-literal return type has the same
new boundary regression in TS and TSX. The author's boolean return-type control
must continue to work.

```js
function outer(x) {
  const make = x => x in {a: 1} || x;
  return make;
}
function after() { return 1; }
```

Pre-condition: expression-bodied arrow whose expression uses an object operand
after `in` (and the equivalent cases after `void`, `delete`, `typeof`, `await`,
`>` and `>=`). Trigger: analyze the file. Result: `make` ends on line 2 with
CCN 2; `outer` ends on line 4 with CCN 1; `after` stays separate. Both target
and raw proposal extend `make`/`outer` and mis-own conditions for comparison
operands. Block-bodied arrows, parenthesized objects, nested objects,
logical/nullish expressions and JSX attribute arrows remain controls that must
keep working.

**Scope:**

- Required behavior: coherent runtime-arrow versus function-type context in the
  existing `JavaScriptFunctionsMixin`, `TypeScriptStates` and declaration
  owners so object expression bodies and object operands after the listed
  operators end the arrow correctly; typed signatures keep their real bodies;
  following functions stay separate with correct names and CCN. Preserve
  binding/object ownership rather than PR-specific brace or punctuation
  exceptions. Reach every consumer that shares this path: JavaScript,
  TypeScript, JSX/TSX and Vue script blocks (`.cjs`/`.mjs` reuse the same
  reader).
- Proof: discriminating public-analysis tests for those consumers; keep the
  author's related tests in a cohesive small module and leave unrelated large
  existing test files unchanged. Remeasure the corrected candidate against its
  current target and the frozen review baseline
  `2e21e404c1ab9f4e0b49acc80816e816629d0670`, with a paired 95% upper slowdown
  bound strictly below 3% for both representative affected and broader
  workloads. Run the full pytest suite, `make` and final hosted CI
  (Ubuntu/Windows × Python 3.8–3.14) before any exact-head guarded merge.
- Rejection constraints: the strict below-3% performance gate and correct
  public names/lines/CCN are acceptance constraints from the retained review,
  not optional polish. Examples do not forbid other valid object or typed
  forms that the shared readers already support.
- Deferred promises: multiline expression-continuation and ternary-colon
  arrow outcomes remain with
  [SEED-014](SEED-014-multiline-arrow-boundaries.md#multiline-arrow-boundaries)
  and
  [SEED-015](SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries);
  arrow parameter-binding work remains with
  [SEED-016](SEED-016-arrow-parameter-bindings.md#arrow-parameter-bindings).
  Reconcile PR533/PR534's inherited expression-body state when reviewing those
  stories; no formal blocking story dependency is asserted. No separate ENS
  performance claim; no full JavaScript/TypeScript grammar; no merge,
  contributor-head write or acknowledgement during preparation.

**Retained evidence:** Raw paired timings on the 64 affected inputs gave median
slowdown 3.0978% with paired 95% interval [2.0154%, 4.8117%] (gate unproved);
broader fixed-source CLI upper bound 1.2813% (passes). Those diagnostics do not
qualify corrected code. Durable metadata, proposed patch, proof commands and
verdicts live in
[review-findings.md](../../docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md).

**Completion:** Reported object arrows and the listed valid type/operator
boundaries produce the expected public names, boundaries and CCN, with coherent
shared reader design, regression proof and satisfactory final performance/CI.
