# Source-only review

**Launch:** Fresh general-purpose reviewer (no conversation inheritance),
2026-11-20. Input: SEED-068#js-asi-parenthesized-assignment Goal, Scope, key
examples and exclusions only. No plan, product, or proof.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | In JavaScript/TypeScript without semicolons, when `name = (…)` (plain or member target, `.js` and `.ts`) ends at a line break with no `=>` and the next line starts with a structure keyword (`if`, `while`, …), that keyword’s ND level belongs to the enclosing function—exactly as after `;` (today that ND is one level too low). Goal + Required + Minimal/Nested/while/.ts/js-yaml ND examples. | slice 1 | observed: test/test_js_asi_parenthesized_assignment_nd.py::test_minimal_if_after_parenthesized_assignment (+ nested/while/.ts/js-yaml ND); focused-proof-final + make-full |
| 2 | Under those same conditions, CCN, NS and token counts stay as today apart from the missing `;` token. Required + Minimal CCN 2 / NS 1 + js-yaml CCN 7. | slice 1 | observed: test_minimal_ccn_and_ns_unchanged (CCN 2 NS 1); js-yaml CCN 7; token-count-probe3.log (delta 1 for missing ;) |
| 3 | Arrow functions that look like parenthesized assignments remain functions: parameter list spanning lines (`x = (a,\n b) => …`) and typed arrows (`x = (a): T => …`). | slice 1 | observed: test_multiline_parameter_arrow_remains_a_function + test_typed_arrow_remains_a_function |
| 4 | With a plain statement between the parenthesized assignment and the structure keyword—`r = (m < 2)` / `w()` / `if (r) w()`—ND remains 1 (unaffected today and after). | slice 1 | observed: test_plain_statement_between_keeps_nd |

## Unnumbered context

- **Beneficiary:** Users analyzing no-semicolon JS/TS (e.g. `js-yaml`) whose ND is one level too low when such an assignment is directly followed by a nested structure; contributes to matching ND between semicolon and no-semicolon styles.
- **Preserved note:** A line break before `=>` cannot occur in valid JavaScript, so it needs no handling.
- **Deferred:** Other ASI discrepancies (SEED-069 covers the shared scan’s cost, not this behavior).
