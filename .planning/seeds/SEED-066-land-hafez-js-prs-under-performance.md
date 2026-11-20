---
id: SEED-066
status: active
planted: 2026-11-20
planted_during: lizard process pr wrap-up
scope: land Hafez JS cognitive/nesting PRs under performance gates
---

# SEED-066: Land Hafez JS PRs under performance gates

<a id="land-hafez-js-prs-under-performance"></a>

### Land Hafez JS cognitive/nesting PRs under performance gates

**Identity:** SEED-066#land-hafez-js-prs-under-performance
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"460776e6ea779df5389d7794c24ccf3efbf8ca09b60893c8bab7c0eab5719e0a"}}
```

**Goal:** Users of `-Ecognitive`, `-END`, and `-ENS` on JavaScript/TypeScript receive the accepted correctness fixes from PRs #576, #578, and then #577 once each change meets paired performance screening (or an explicit recorded bound), without merging drafts out of order.

**Beneficiary:** Maintainers and users blocked on wrong cognitive/nesting scores while the fixes stay unmerged for cost.

**Scope:** Reuse review evidence under `.planning/evidence/pr-576/`, `pr-578/`, and `pr-577/`. Quiet-machine remeasure; cheap optimizations on contributor branches when needed (`_route` / shared `ImpliedSemicolons`); merge #576 then undraft/merge #577; optimize and merge #578. Deferred: unrelated PRs and changing metric definitions.

**Key examples:**

- Nested-callback cognitive case scores 3 after #576 lands.
- Abandoned `(prefix ? …)` scores 1 after #577 lands on top of #576.
- ASI braceless sibling `if`s report ND/NS 1 after #578 lands.
- Required paired screens pass the bound recorded for this story, or an explicit developer-approved bound is recorded before merge.

**Approved performance bounds (developer, 2026-11-20):** the default 3% paired gate is replaced for this story by: #576 + #577 cumulative paired median ≤15% on `-Ecognitive` JS/TS-like workloads only; #578 paired median ≤25% on `-END`/`-ENS` JS/TS-like workloads only. Runs without these extensions keep the 3% gate. Quiet-host paired API evidence (40 pairs, 2 s batches; no-op control CI −2.5…+3.2%): #576 +8.6% [5.4, 11.8] nested-callback corpus, +5.0% [0.7, 9.5] mixed; #576+#577 +13.2% [10.0, 16.6] / +13.1% [10.4, 15.9]; #578 ND +21.8% [17.4, 26.4], NS +21.3% [17.5, 25.3], plain +0.2% [−1.6, 2.0]. Maintainer corrections: #576 bracket-only `_route`; #577 counting folded into `process` (no extra per-token call). A gated `ImpliedSemicolons` scan gave no measurable gain and was not kept. The bounds are on the paired median because the developer approved them with these intervals shown (upper limits 16.6% and 26.4%).

**Source:** Deferred from SEED-062/063/064 process-PR dispositions on 2026-11-20.
