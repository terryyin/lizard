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

**Beneficiary:** Users measuring JavaScript, TypeScript, JSX/TSX and Vue whose
object expressions currently distort function boundaries and metrics.

**Goal:** Recognize objects inside expression-bodied arrows while preserving
real function bodies after function-type return annotations, public function
names, boundaries and CCN, including following functions.

**Source:** [PR531](https://github.com/terryyin/lizard/pull/531), authored head
`47aa35735cccba962e507edb179eae1154ab6ad2`, original base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`, reviewed against current target
`7ac449f59b3f155fd159ce9aad6794a422045632`. The report is real and worthwhile;
the raw proposal introduces a valid TypeScript regression and leaves related
object operands broken. Correction and cohesive tests are estimated at
30–45 minutes, above the review mission's fifteen-minute cutoff. No product
correction was attempted; the PR remains unmerged.

**Reproducers and expected results:**

```js
function layout(vertical) {
  const box = f => vertical ? {x: f.a, y: f.b} : {x: f.b, y: f.a};
  return box;
}
function after() { return 1; }
```

box must end on line 2, layout on line 4, and after remain on line 5. Target
reports box ending on line 4 and layout on line 5; the raw proposal fixes this
in JS, TS, JSX, TSX and a real Vue script block.

```ts
function filter(value: Json): (event: Obj) => "ok" {
  if (value) return () => "ok";
  return () => "ok";
}
function after(): void {}
```

The two runtime arrows are anonymous on lines 2 and 3; filter spans lines 1–4
with CCN 2, and after remains on line 5. Target has those results. The proposal
creates two functions named return and extends filter through line 5 in TS,
TSX and a Vue script block. A template literal return type has the same new
boundary regression in TS and TSX. The author's boolean return-type control
must also continue to work.

```js
function outer(x) {
  const make = x => x in {a: 1} || x;
  return make;
}
function after() { return 1; }
```

make must end on line 2 with CCN 2 and outer on line 4 with CCN 1. Both
target and proposal extend make to line 4 and outer to line 5. Equivalent
object operands after void, delete, typeof, await, > and >= also need correct
boundaries and condition ownership. Retain working block-bodied arrows,
parenthesized objects, nested objects, logical/nullish expressions and JSX
attribute arrows as controls.

The [public API reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py)
checks explicit names, start/end lines and CCN for all 64 retained inputs.
Its target and raw-candidate logs distinguish existing defects from introduced
regressions. All inputs and source hashes were frozen before timing.

**Scope and remaining work:**

- Replace the raw proposal's first-character brace heuristic with coherent
  runtime-arrow versus function-type context handling in the existing
  JavaScriptFunctionsMixin, TypeScriptStates and declaration owners. Preserve
  binding/object ownership rather than adding PR-specific exceptions.
- Add discriminating public-analysis tests for all reachable JS/TS, JSX/TSX
  and Vue consumers. Keep the full author tests in a cohesive small module;
  leave unrelated large existing test files unchanged.
- Reconcile PR533/PR534's inherited expression-body state when reviewing them;
  their newline/ternary outcomes remain independently owned by their reviews.
  No formal blocking story dependency is asserted by this shared implementation.
- Remeasure the corrected candidate against its current target and the frozen
  review baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670`, with a 95% upper
  bound strictly below 3% for both representative affected and broader workloads.
  The raw diagnostic timings do not qualify corrected code or ENS behavior.
- Run the full pytest suite, make and final hosted CI before any exact-head
  guarded merge. The current CI matrix is Ubuntu/Windows × Python 3.8–3.14.

**Raw performance:** Twenty alternating pairs on the 64 affected inputs give
a median slowdown of 3.0978% and a paired 95% interval of [2.0154%, 4.8117%].
The strict below-3% gate remains unproved. The broader fixed-source CLI upper
bound is 1.2813% and passes. Four Vue cases are included; no separate ENS
performance claim is made. A bounded unchanged-output profile does not isolate
the full API timing difference. Corrected code needs fresh measurements.

The [durable review evidence](../../docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md)
owns the metadata, proposed patch, proof commands, raw paired measurements,
comparison basis and performance verdict. This queued story has no execution
plan and has not been taken for implementation.

**Completion:** Reported object arrows and the listed valid type/operator
boundaries produce the expected public names, boundaries and CCN, with coherent
shared reader design, regression proof and satisfactory final performance/CI.
