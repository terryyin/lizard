---
id: SEED-067
status: active
planted: 2026-11-20
planted_during: SEED-066 wrap-up
scope: cognitive complexity — a comma ends a ternary
---

# SEED-067: A comma ends a ternary in cognitive complexity

<a id="cognitive-comma-ends-ternary"></a>

### A comma ends a ternary in cognitive complexity

**Identity:** SEED-067#cognitive-comma-ends-ternary
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/034-cognitive-comma-ends-ternary/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"b3d2f1021ef414aef9bd42471e89e93b247eb414eaae9555428bdb97ed592a37","plan":"85c521ff9cd8d851c6d584132ee8ba6c2b0092e59e6f829c41776d72db71bb18"}}
```

**Goal:** Users of `-Ecognitive` on languages whose profile counts ternaries get a score where a `,` that separates peer arguments, elements, or expressions ends the open ternary, so the next ternary in that list is a sibling (not nested).

**Beneficiary:** Callers whose argument lists, array/object elements, or comma-separated expressions hold more than one ternary (reported for JavaScript/TypeScript in PR #576; the counter is shared across ternary-counting profiles).

**Scope:**

- Required: the cognitive counter ends an open ternary frame at a `,` that separates peers at that expression depth, for every language whose profile counts ternaries.
- Required restriction: a ternary that is structurally nested inside another ternary's branch still receives the nesting increment (parentheses or deeper structure keep nesting).
- Scope limits: no change to when a `?`/`:` pair starts a ternary, to the base ternary increment, or to CCN / other metrics. Deferred: any broader rewrite of ternary scoring rules beyond peer-list termination.

**Key examples:**

- Pre: `function f(a, b) { go(a ? x : b, b ? 1 : 2) }` under `-Ecognitive`. Trigger: analyze. Result: cognitive complexity 2 (not 3; already 3 on master before #576, and #576 made the template form regress the same way).
- Pre: ``function f(a, b) { go(a ? `${a}` : b, b ? 1 : 2) }``. Trigger: analyze. Result: 2 (the form #576 left at 3).
- Pre: `function f(a, b) { const xs = [a ? 1 : 2, b ? 3 : 4]; }`. Trigger: analyze. Result: 2 (peer elements, not nested).
- Pre: `function f(a, b) { x = a ? (b ? 1 : 2) : 3 }`. Trigger: analyze. Result: nested ternary still counts as nested (higher than two sibling ternaries).

**Source:** Reported in PR #576's description as a separate bug; deferred from SEED-066 on 2026-11-20.
