# Source-only review — SEED-067#cognitive-comma-ends-ternary

**Launch:** Fresh general-purpose reviewer; isolation required; no plan, product,
proof, or project files. Input: Goal, Scope, key examples, and exclusions from
`.planning/seeds/SEED-067-cognitive-comma-ends-ternary.md#cognitive-comma-ends-ternary`
only (story text as of established start).

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Peer `,` ends an open ternary for cognitive scoring — Under `-Ecognitive`, a `,` that separates peer arguments, elements, or expressions at that expression depth ends the open ternary frame, so a following ternary in that list is a sibling (not nested); two such peer ternaries score cognitive complexity **2** (not 3). Conditions: languages whose profile counts ternaries; trigger: analyze. Source: Goal; Scope required; examples `go(a ? x : b, b ? 1 : 2)` → 2; ``go(a ? `${a}` : b, b ? 1 : 2)`` → 2; `[a ? 1 : 2, b ? 3 : 4]` → 2; Beneficiary | slice 1 | observed: test/test_extensions/testCognitivePeerCommaEndsTernary.py::TestPeerCommaEndsTernary (JS call/template/array → cog 2; C++ peer args → cog 2) |
| 2 | Nested ternary still gets nesting increment — A ternary structurally nested inside another ternary’s branch still receives the nesting increment (parentheses or deeper structure keep nesting); nested form scores **higher** than two sibling ternaries. Conditions: same cognitive ternary-counting profiles; trigger: analyze. Source: Scope required restriction; example `x = a ? (b ? 1 : 2) : 3` | slice 1 | observed: test/test_extensions/testCognitivePeerCommaEndsTernary.py::TestPeerCommaEndsTernary.test_javascript_nested_ternary_still_nests → cog 3 |

## Unnumbered source context

- Identity: `SEED-067#cognitive-comma-ends-ternary`; Source: PR #576 separate bug; deferred from SEED-066 (2026-11-20).
- Baselines / reasons: already 3 on master before #576; #576 left the template form at 3.
- Scope limits: no change to when `?`/`:` starts a ternary, base ternary increment, or CCN / other metrics.
- Deferred: broader rewrite of ternary scoring beyond peer-list termination.
