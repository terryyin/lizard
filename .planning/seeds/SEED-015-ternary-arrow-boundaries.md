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

**Beneficiary:** JavaScript and TypeScript users relying on Lizard's function
locations and CCN when callbacks appear in conditional expressions.

**Goal:** An enclosing ternary colon ends its first expression-bodied arrow,
while internal ternary colons and genuine TypeScript annotations preserve their
own syntax and the correct function's CCN. Following functions remain distinct.

**Origin:** [PR #534](https://github.com/terryyin/lizard/pull/534), reviewed at
`f4074ae860c7999cc37dd081a6f7ebfa777adc83` against execution target `7186fdbe`
and trunk `7ac449f`. Necessary coherent correction is estimated **30–45 minutes**,
above SEED-010's fifteen-minute limit. Review and measurement are excluded.

**Reproducible expected and actual behavior:**

```javascript
function choose(strict, b) {
  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;
  return eq;
}
```

Through `analyze_file.analyze_source_code("a.js", source)`, target reports one
anonymous arrow at line 2 with CCN 2. Expected: two anonymous arrows at line 2,
CCN 1 and 2, followed by `choose` spanning 1–4 with CCN 2. Raw #534 and the
independent child provide this useful improvement in JS/TS/JSX/TSX/Vue.

```javascript
function outer(c, d) {
  const f = x => d ? y => y : z => z || x;
  return 1;
}
function after() { return 1; }
```

Lizard's existing ungrouped curried-arrow convention detects only the outer
arrow (`testES6.py::Test_ES6_multiple_arrow_patterns::test_nested_arrows`).
Expected and target: `f` at 2–2, CCN 3; `outer` at 1–4, CCN 1; `after` at
5–5, CCN 1. Raw #534 and its independent child instead report `f` CCN 2 and
an extra anonymous arrow CCN 2. Internal colon ownership is lost because the
ternary snapshot is overwritten even when a nested arrow creates no function.
Preserve the existing curried-arrow detection contract.

```typescript
function outer(c, d) {
  const f = c ? (x: number): number => x : (y: number): number => y || d;
  return 1;
}
function after() { return 1; }
```

Expected: two anonymous arrows at 2–2, CCN 1 and 2; `outer` at 1–4, CCN 2;
`after` at 5–5, CCN 1. Target/raw/child report one CCN-2 arrow. A genuine
return-type colon consumes the pending ternary depth before the arrow starts.
Object return-type annotations show the same failure in TS/TSX; Vue reaches
the shared TypeScript reader too.

**Remaining work:**

- Correct ternary/body state ownership and distinguish signature annotations
  before consuming runtime branch separators, using the existing function,
  binding, object and declaration owners. Do not replace the public facade.
- Retain the useful reported correction and the existing curried-arrow contract;
  add discriminating public analyzer/CLI proof for the reproductions, nested
  ternaries, grouped arrows, typed returns, property/computed-key colons,
  genuine function-type bodies and following functions across reached readers.
- Integrate independently of #531's invalid object heuristic. The isolated child
  proves #534's original report and authored test pass without it and restores
  literal function-type bodies. #531 is not a prerequisite for starting this work.
  Its lost object improvement and existing object-comma limitations remain
  visible controls, not authorization for unrelated parser redesign.
- Keep new tests in a small cohesive module and account for necessary cleanup.
  Independently review the final change, run focused and full checks, and prove
  final-candidate CI on the current Ubuntu/Windows × Python 3.8–3.14 matrix.
- Measure the corrected candidate on the frozen affected and broader corpora
  against current target and initial default path. The raw proposal's API
  95% interval is 1.52%–3.27%; CLI is −4.73%–4.31%. Both strict below-3%
  gates are unproved. Obtain sufficient precision after a correct candidate
  exists; do not rerun the already-invalid raw proposal until it passes.
  Include the four supplemental internal-colon controls in that corrected
  candidate's fixed performance corpus before accepting its performance gate.

**Evidence and replay:**
[PR534 evidence](../../docs/reviews/fabio-malpezzi-20261027/pr534/README.md)
retains the full patch/metadata, 92-case frozen corpus, four supplemental
internal-colon controls, expected/actual logs, isolated imports, paired raw
timings, bounded unchanged-output profile, commands and unresolved limits.
[Findings](../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md)
distinguish inherited defects, new regression and existing limitations.

**Priority:** Maintenance work aligned with the near-future direction, queued
after the three already-prioritized review corrections, preserving their order.
No blocking supplier agreement, Take, execution plan or implementation is created.

**Evaluation:** All three reproductions meet their explicit names/spans/CCN,
internal colons and genuine typed bodies retain their ownership, relevant
regressions pass, and each corrected affected/broader incremental/cumulative
performance upper 95% bound is strictly below 3%. Keep PR #534 unmerged until
a final corrected revision satisfies all source gates.
