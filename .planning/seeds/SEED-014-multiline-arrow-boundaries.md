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

**Beneficiary:** Users measuring JavaScript, TypeScript, JSX/TSX and Vue code
with expression-bodied arrows, class fields and automatic semicolon insertion.

**Goal:** End each expression-bodied arrow on its expression's last line,
retain valid continuations, and assign the next statement's tokens, NLOC and
conditions to its enclosing function. Report following methods and functions.

**Source:** [PR533](https://github.com/terryyin/lizard/pull/533), authored head
`15423852c04aed302f684395fe9522251d88a2f2`, original base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`, reviewed against target
`7ac449f59b3f155fd159ce9aad6794a422045632`. The report is real and worthwhile.
The raw proposal fixes its examples but introduces valid continuation errors
and exceeds the affected performance limit. Coherent correction and cleanup
are estimated at 30–45 minutes, above the review's fifteen-minute cutoff.
No product correction was attempted; the PR remains unmerged.

**Reproducers and expected results:**

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

The arrows end on lines 4 and 6, each CCN 1, NLOC 1 and token count 5.
m has CCN 3, NLOC 8 and 33 tokens. pick ends on line 11, CCN 4, NLOC 3,
15 tokens. Target reports end lines 5 and 7, m CCN 2 and pick ending on 10.
The raw proposal reaches the expected results in JS, TS, JSX/TSX and Vue.

```js
class A {
  handler = () => 1
  async load() {
    return 1
  }
}
function after() { return 1 }
```

handler spans line 2, load spans 3–5, and after spans 7. Target reports only
handler spanning 2–7; raw fixes this. A semicolon-free `x => x + 1` followed
by `let a` must have the same end line and NLOC as its semicolon version,
and one fewer token; the following statement owns its first token.

```js
const f = x => x
  * 2;
function after() { return 1; }
```

f must span lines 1–2. Target does; raw ends it on line 1 and loses the
continuation. The same new error occurs with a following `<`, `>`, `**`,
call `(2)`, subscript `[2]`, or tagged template. Include postfix `++` as a
new-statement control and operators at the previous line's end as continuation
controls. Keep parenthesized expressions, comments, case labels and block
bodies, with and without semicolons and with following functions.

**Scope and remaining work:**

- Establish coherent expression-continuation and statement-boundary context
  in the existing JavaScriptFunctionsMixin and TypeScriptStates owners.
  Preserve binding, object and declaration ownership and the public facade.
  Avoid an incomplete operator allowlist or a PR-specific parser switch.
- Preserve genuine function-type return annotations and the inherited PR531
  counterexamples. The raw head contains both PR531 and PR533 commits. An
  isolated child diagnostic removes only PR531's object-opening heuristic;
  it retains all five authored child-test improvements and restores literal
  type returns, but still breaks the listed continuations. This demonstrates
  independent work can start: no blocking dependency on SEED-013 is asserted.
  Reconcile any separately accepted object-boundary fix when integrating.
- Keep metric ownership coherent across newlines without introducing costly
  unconditional per-token bookkeeping or parallel metric owners. Add
  discriminating analyze_file/FileAnalyzer/CLI proof for related consumers.
  Move the added author tests into a cohesive module below 250 lines while
  leaving unrelated existing large test files byte-identical.
- Remeasure the corrected candidate against its current target and frozen
  review baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670`. Both affected
  and broader workloads require a paired 95% upper slowdown bound below 3%.
- Run full pytest, make and current hosted CI before an exact-head guarded
  merge. Current CI is Ubuntu/Windows × Python 3.8–3.14, fourteen jobs.

**Raw performance:** Twenty alternating pairs on 150 fixed affected inputs
give +9.7519% median slowdown, paired 95% interval [5.2695%, 11.8135%], which
fails. The broader fixed-source CLI gives +0.5836%, interval [-0.4817%,
2.5769%], which passes. Source and output equivalence make these comparisons
both incremental and cumulative for default analysis; ENS was not measured.
A bounded unchanged-output profile implicates the added per-token state
tracking but does not isolate the full affected workload's slowdown.

The [durable evidence](../../docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md)
retains the complete proposal, original tests, explicit expected results,
terminal observations, frozen hashes, replay recipe, raw pairs and profile.
This story is queued, has no execution plan and has not been taken.

**Completion:** The reported cases and valid continuations have correct public
names, lines, CCN, NLOC and token ownership across the related readers, with
coherent design, discriminating regression proof and satisfactory performance/CI.
