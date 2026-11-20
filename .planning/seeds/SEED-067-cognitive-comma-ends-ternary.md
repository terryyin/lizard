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

**Goal:** `-Ecognitive` ends a ternary at a `,` that separates arguments or elements, so the next ternary in the same list is not counted as nested in the previous one.

**Beneficiary:** Users of `-Ecognitive` on C-like languages (reported for JavaScript/TypeScript) whose argument lists hold more than one ternary.

**Scope:** The cognitive counter's ternary frame and its end tokens, for every language whose profile counts ternaries. Deferred: changing the definition of a ternary increment.

**Key examples:**

- `function f(a, b) { go(a ? x : b, b ? 1 : 2) }` scores 2, not 3 (already 3 on master before #576).
- ``go(a ? `${a}` : b, b ? 1 : 2)`` scores 2.
- `x = a ? (b ? 1 : 2) : 3` still counts the nested ternary as nested.

**Source:** Reported in PR #576's description as a separate bug; deferred from SEED-066 on 2026-11-20.
