---
id: SEED-015
status: active
planted: 2026-10-27
planted_during: FabioMalpezzi PR review, PR534 disposition
trigger_when: Correct colon ownership for expression-bodied arrows
scope: medium
---

# SEED-015: Ternary arrow boundaries

<a id="ternary-arrow-boundaries"></a>

### Preserve arrow boundaries across ternary and type colons

**Identity:** SEED-015#ternary-arrow-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/011-ternary-arrow-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"e4aa075a9b5f8af7470b714e4c477fd13a57925dd8982045e9fda2cad5f42a94","plan":"c2181dfcb6e9850d638ccbf4e6f9104a0225546dda42414de02212c51d7b65dd"}}
```

**Beneficiary:** JavaScript, TypeScript, JSX/TSX and Vue users relying on
Lizard's function locations and CCN when expression-bodied arrows appear as
branches of conditional expressions.

**Goal:** Public analysis ends an expression-bodied arrow at the ternary colon
that separates it from the next branch, so each branch arrow reports its own
CCN. Colons inside the arrow's own body, genuine TypeScript annotations, and
property or computed-key colons keep their own syntax, and the enclosing and
following functions keep their correct names, spans and CCN.

**Source:** [PR #534](https://github.com/terryyin/lizard/pull/534), authored
head `f4074ae860c7999cc37dd081a6f7ebfa777adc83`, reviewed against target
`7186fdbe` and trunk `7ac449f`. The report is real and useful. The raw
proposal fixes the reported case but loses internal-colon ownership when a
nested arrow creates no function, leaves typed branch separators unresolved,
and leaves both strict timing gates unproved. Coherent correction is
estimated at 30–45 minutes, above SEED-010's fifteen-minute limit. No product
correction was attempted; PR #534 remains unmerged. Naming and metric
meanings follow
[ADR-0001](../../docs/adrs/0001-ubiquitous-language-accepted.md); the facade
and the existing reader owners stay unchanged.

**Key examples:**

These situations define required public behavior through `analyze_file` (and
the equivalent CLI / FileAnalyzer), not an exhaustive grammar. The frozen
[expected rows](../../docs/reviews/fabio-malpezzi-20261027/pr534/inputs.json)
and [supplemental controls](../../docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json)
give explicit names, start/end lines and CCN per consumer; the
[reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py)
checks them. Function names follow the existing binding convention recorded
in those rows (an unparenthesized first branch takes the binding name; a
parenthesized one is anonymous); this story does not change naming.

```javascript
function choose(strict, b) {
  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;
  return eq;
}
```

Pre-condition: a ternary whose branches are both expression-bodied arrows.
Trigger: analyze the file. Result: two anonymous arrows at 2–2 with CCN 1 and
2, then `choose` at 1–4 with CCN 2. Current target reports one anonymous
arrow with CCN 2. The same holds in `.ts`, `.jsx`, `.tsx`, a JSX attribute
(`<Panel f={c ? x => x : y => y || d}/>`) and a real Vue script block. Nested
ternaries (`c ? d ? x => x : y => y && d : z => z || c`: `f` CCN 1, two
anonymous CCN 2, `outer` CCN 3), grouped inner arrows, parenthesized object
bodies and block-bodied branches follow the same rule.

```javascript
function outer(c, d) {
  const f = x => d ? y => y : z => z || x;
  return 1;
}
function after() { return 1; }
```

Pre-condition: the ternary and its colon lie inside an arrow's own body, and
the nested arrows create no function under the existing curried-arrow
convention (`testES6.py::Test_ES6_multiple_arrow_patterns::test_nested_arrows`).
Trigger: analyze the file. Result: `f` at 2–2, CCN 3; `outer` at 1–4, CCN 1;
`after` at 5–5, CCN 1. Target is already correct; raw #534 reports `f` CCN 2
plus a spurious anonymous CCN-2 arrow in JS/TS/JSX/TSX. The same internal
colon inside an enclosing ternary branch
(`c ? x => d ? y => y : z => z || x : w => w`) gives `f` CCN 3 and an
anonymous CCN-1 alternate. Preserve the outer-only curried contract; this
story requests no nested-function detection.

```typescript
function outer(c, d) {
  const f = c ? (x: number): number => x : (y: number): number => y || d;
  return 1;
}
function after() { return 1; }
```

Pre-condition: typed branch arrows whose return-type annotation colon comes
before `=>`. Trigger: analyze as TypeScript (and TSX / Vue script). Result:
two anonymous arrows at 2–2 with CCN 1 and 2; `outer` at 1–4, CCN 2; `after`
at 5–5, CCN 1. Target, raw and the independent child report one CCN-2 arrow.
An object return type (`(x: number): {a: number} => ({a: x})`) and a type
assertion inside the first branch give the same split. Function-type
aliases and functions whose return type is a function type
(`function filter(v: Json): (e: Obj) => "ok" { ... }`) keep their real
bodies and following functions as on target.

```javascript
function outer(c, d) {
  const o = {f: c ? x => x : y => y || d, next: z => z && c};
  return 1;
}
```

Pre-condition: the ternary appears as an object property value. Trigger:
analyze the file. Result: the property colon is not a ternary separator, and
the first branch `f` is reported at 2–2 with CCN 1, separate from the
alternate. A computed key containing a ternary
(`{[c ? "a" : "b"]: x => x || d}`) keeps its target result. The alternate
absorbing the comma sibling `next` is the existing comma limitation below,
not part of this promise.

**Scope:**

- Required behavior: coherent ternary and expression-body colon ownership in
  the existing `JavaScriptFunctionsMixin`, `TypeScriptStates`,
  `JavaScriptBindingsMixin`, `JavaScriptObjectsMixin` and
  `TypeScriptDeclarationsMixin` owners: an enclosing ternary colon ends the
  branch arrow; a nested arrow that creates no function does not overwrite the
  enclosing arrow's ternary context; a genuine signature annotation is
  consumed as a type before it can be taken as a runtime branch separator.
  No parallel parser, metric counter or PR-specific colon switch; the public
  facade stays. Reach every consumer that shares the path: JavaScript,
  TypeScript, JSX/TSX and Vue script blocks (`.cjs`/`.mjs` reuse the reader).
- Proof: discriminating public-analysis tests for the promised frozen rows
  (`reported`, `authored`, `internal_ternary`, `nested_ternary`,
  `nested_arrows`, `nested_arrow_internal`, `grouped_nested_arrow`,
  `branch_object`, `typed_arrows`, `type_assertion`, `typed_object_return`,
  `block_branches`, `plain_ternary`, `jsx_attribute`, the matching `vue_*`
  rows and the four supplemental curried controls) across their consumers,
  plus genuine function-type body controls and following functions. Keep new
  tests in a small cohesive module below 250 lines; leave unrelated large test
  files byte-identical. Remeasure the corrected candidate on the frozen
  92-case affected corpus plus the four supplemental controls and the
  broader fixed-source corpus against its current target and the frozen
  review baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670`, with a paired
  95% upper slowdown bound strictly below 3% for both incremental and
  cumulative affected and broader workloads. Run the full pytest suite,
  `make` and final hosted CI (Ubuntu/Windows × Python 3.8–3.14) before any
  exact-head guarded merge.
- Rejection constraints: the strict below-3% performance gate, the existing
  curried-arrow contract and correct public names/lines/CCN are acceptance
  constraints from the retained review, not optional polish. Examples do not
  forbid other valid ternary, typed or object forms the shared readers
  already support.
- Deferred promises: the existing object-comma limitation, where a sibling
  arrow after a comma in an object literal is absorbed into the preceding
  arrow (it reproduces without any ternary: `{f: x => x, next: z => z && c}`
  reports only `f`), so the frozen `next` rows of `object_property` and
  `computed_key` are not promised here. Object-literal ternary bodies
  (`inherited_*` and `object_inside_ternary` rows) and typed function-body
  regressions stay with
  [SEED-013](SEED-013-arrow-object-boundaries.md#arrow-object-boundaries);
  multiline continuations with SEED-014 (delivered through PR533, merge
  `a2d2ab7`);
  arrow parameter bindings with
  [SEED-016](SEED-016-arrow-parameter-bindings.md#arrow-parameter-bindings).
  Reconcile with whichever of SEED-013/SEED-014 lands first on the shared
  expression-body state. The
  [isolated child](../../docs/reviews/fabio-malpezzi-20261027/pr534/independent-child-patch.json)
  shows the reported improvement works without #531's object heuristic, so
  no formal blocking dependency is asserted. No naming change, nested-arrow
  detection, ENS performance claim or full JavaScript/TypeScript grammar; no
  merge, contributor-head write or acknowledgement during preparation.

**Retained evidence:** Raw paired timings: API median +2.0337%, paired 95%
interval [1.5167%, 3.2720%]; broader CLI +1.2713%, interval [−4.7317%,
4.3104%]. Both strict gates are unproved, not failed; the invalid raw
proposal was measured once per workload and is not rerun. A bounded
unchanged-output profile shows small added object-helper and arrow-state
overhead without isolating the API interval. Durable metadata, patch, corpus,
logs and commands live in the
[PR534 evidence](../../docs/reviews/fabio-malpezzi-20261027/pr534/README.md)
and [findings](../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md).

**Completion:** The promised frozen rows and controls have correct public
names, lines and CCN across the related readers, with coherent ownership,
discriminating regression proof and satisfactory performance/CI. Keep PR #534
unmerged until a final corrected revision satisfies all gates.
