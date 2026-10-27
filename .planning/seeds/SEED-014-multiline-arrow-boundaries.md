---
id: SEED-014
status: active
planted: 2026-10-27
planted_during: FabioMalpezzi PR review, PR533 disposition
trigger_when: Correct multiline expression-bodied arrow boundaries and metrics
scope: medium
---

# SEED-014: Multiline arrow boundaries

<a id="multiline-arrow-boundaries"></a>

### Preserve multiline arrow expressions and surrounding statement metrics

**Identity:** SEED-014#multiline-arrow-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/010-multiline-arrow-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"46587474d8394487721866c351e9d727a71f1a57371ca9e927c37b5d146a9060","plan":"394281cec8891fc850e90d0f9cf7a22d5295c280189118671946b11f78726c6e"}}
```

**Beneficiary:** Users measuring JavaScript, TypeScript, JSX/TSX and Vue code
with expression-bodied arrows, class fields and automatic semicolon insertion.

**Goal:** Public analysis ends each expression-bodied arrow on its expression's
last line, retains valid multiline continuations, and assigns the next
statement's tokens, NLOC and conditions to the enclosing function—with
following methods and functions reported separately under their correct
names, lines, CCN, NLOC and token counts.

**Source:** [PR533](https://github.com/terryyin/lizard/pull/533), authored head
`15423852c04aed302f684395fe9522251d88a2f2`, original base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`, reviewed against target
`7ac449f59b3f155fd159ce9aad6794a422045632`. The report is real and worthwhile.
The raw proposal fixes its examples but introduces valid continuation errors
and exceeds the affected performance limit. Coherent correction and cleanup
are estimated at 30–45 minutes, above the review's fifteen-minute cutoff.
No product correction was attempted; the PR remains unmerged. Naming and
metric meanings follow
[ADR-0001](../../docs/adrs/0001-ubiquitous-language-accepted.md); existing
facade, core and reader ownership stay unchanged.

**Key examples:**

These situations define required public behavior through `analyze_file` (and
equivalent CLI / FileAnalyzer), not an exhaustive grammar. The retained
[public API reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py)
checks explicit names, start/end lines, CCN and, where asserted, NLOC and
token count across reachable consumers.

```js
function m(key, cursor) {
  switch (true) {
    case key.left:
      return () => cursor.prevWord()
    case key.right:
      return () => cursor.nextWord()
  }
}
const pick = (a, b) => a && b
  ? a
  : b || 0
```

Pre-condition: expression-bodied arrows returned from switch cases, plus a
multiline ternary expression arrow. Trigger: analyze the file. Result: the
arrows end on lines 4 and 6, each CCN 1, NLOC 1 and token count 5; `m` has
CCN 3, NLOC 8 and 33 tokens; `pick` ends on line 11, CCN 4, NLOC 3, 15 tokens.
Current target ends those arrows on lines 5 and 7, reports `m` CCN 2 and
`pick` ending on line 10. The raw proposal reaches the expected results in
`.js`, `.ts`, `.jsx`, `.tsx` and a real Vue script block.

```js
class A {
  handler = () => 1
  async load() {
    return 1
  }
}
function after() { return 1 }
```

Pre-condition: class field expression arrow immediately followed by a method
and a later function. Trigger: analyze the file. Result: `handler` spans
line 2, `load` spans 3–5, and `after` spans line 7. Target reports only
`handler` spanning 2–7; raw separates them correctly. A semicolon-free
`x => x + 1` followed by `let a` must have the same end line and NLOC as its
semicolon version, and one fewer token; the following statement owns its
first token.

```js
const f = x => x
  * 2;
function after() { return 1; }
```

Pre-condition: expression-bodied arrow whose body continues on the next line
with a binary operator (and the equivalent cases with `<`, `>`, `**`, call
`(2)`, subscript `[2]`, or tagged template). Trigger: analyze the file.
Result: `f` spans lines 1–2 and `after` stays separate. Target already has
those results; the raw proposal ends `f` on line 1 and loses the
continuation. Include postfix `++` as a new-statement control and operators
at the previous line's end as continuation controls. Keep parenthesized
expressions, comments, case labels and block bodies, with and without
semicolons and with following functions.

**Scope:**

- Required behavior: coherent expression-continuation and statement-boundary
  context in the existing `JavaScriptFunctionsMixin` and `TypeScriptStates`
  owners so multiline expression arrows end on the expression's last line,
  valid continuations remain attached, and the next statement's tokens, NLOC
  and conditions belong to the enclosing function. Preserve binding, object
  and declaration ownership and the public facade. Avoid an incomplete
  operator allowlist or a PR-specific parser switch. Reach every consumer that
  shares this path: JavaScript, TypeScript, JSX/TSX and Vue script blocks
  (`.cjs`/`.mjs` reuse the same reader). Keep metric ownership coherent across
  newlines without costly unconditional per-token bookkeeping or parallel
  metric owners.
- Proof: discriminating public-analysis tests for those consumers; move the
  added author tests into a cohesive module below 250 lines while leaving
  unrelated existing large test files byte-identical. Remeasure the corrected
  candidate against its current target and the frozen review baseline
  `2e21e404c1ab9f4e0b49acc80816e816629d0670`, with a paired 95% upper slowdown
  bound strictly below 3% for both representative affected and broader
  workloads. Run the full pytest suite, `make` and final hosted CI
  (Ubuntu/Windows × Python 3.8–3.14) before any exact-head guarded merge.
- Rejection constraints: the strict below-3% performance gate and correct
  public names, lines, CCN, NLOC and token ownership are acceptance constraints
  from the retained review, not optional polish. Examples do not forbid other
  valid continuations or statement forms the shared readers already support.
- Deferred promises: object-arrow and typed function-body outcomes remain with
  [SEED-013](SEED-013-arrow-object-boundaries.md#arrow-object-boundaries);
  ternary-colon arrow ends remain with
  [SEED-015](SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries);
  arrow parameter-binding work remains with
  [SEED-016](SEED-016-arrow-parameter-bindings.md#arrow-parameter-bindings).
  Reconcile any separately accepted object-boundary fix when integrating.
  An isolated child diagnostic that removes only PR531's object-opening
  heuristic still breaks the listed continuations, which shows independent
  work can start: no formal blocking dependency on SEED-013 is asserted. No
  separate ENS performance claim; no full JavaScript/TypeScript grammar; no
  merge, contributor-head write or acknowledgement during preparation.

**Retained evidence:** Raw paired timings on 150 affected inputs gave median
slowdown +9.7519% with paired 95% interval [5.2695%, 11.8135%] (fails); broader
fixed-source CLI +0.5836%, interval [-0.4817%, 2.5769%] (passes). A bounded
unchanged-output profile implicates added per-token state tracking but does
not isolate the full affected slowdown. Those diagnostics do not qualify
corrected code. Durable metadata, proposed patch, proof commands and verdicts
live in
[review-findings.md](../../docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md).

**Completion:** The reported cases and valid continuations have correct public
names, lines, CCN, NLOC and token ownership across the related readers, with
coherent design, discriminating regression proof and satisfactory
performance/CI.
