---
id: SEED-078
status: active
planted: 2026-11-26
---

# SEED-078 Follow-ups from the SEED-077 PR and issue batch

Source review: SEED-077#process-prs-past-48h at frozen master `f7a11e5248a388c1cee706b5aa0d2f234837c44d`.

<a id="nd-without-logical-operators"></a>

### Measure ND from control structures only

**Identity:** SEED-078#nd-without-logical-operators

**Beneficiary:** Users who gate code on nesting depth (`-END`), including the reporter of [#693](https://github.com/terryyin/lizard/issues/693) using it in a safety-critical pipeline.

**Goal:** ND reports only the nesting of control structures, as [ADR 0001](../../docs/adrs/0001-ubiquitous-language-accepted.md) describes, so `&&`/`||` in a condition no longer adds a level.

**Scope:** The developer approved this as a deliberate metric change on 2026-11-26. Remove the logical-operator contribution from ND for every language that ND supports. Update the tests that pin the current behavior. Note the change in release notes, because existing ND values and thresholds drop. Out of scope: CCN, NS and cognitive complexity.

**Key examples:**

- From #693: `plain` (if inside a loop) and `logical` (the same with `a && b` in the if) both report ND 2. `logical` reports 4 today.
- A logical operator outside any condition adds no ND.
- `-ENS` results are unchanged.

<a id="cognitive-ternary-in-control-headers"></a>

### Count ternaries in control headers for all brace languages

**Identity:** SEED-078#cognitive-ternary-in-control-headers

**Beneficiary:** Users of `-Ecognitive` on C-like languages, including JavaScript, TypeScript, JSX, Vue, C, Java and C#.

**Goal:** A ternary inside an `if`/`for`/`while` header adds its cognitive increment in every brace language.

**Scope:** [#684](https://github.com/terryyin/lizard/pull/684) fixes this only when the language is plain JavaScript. The fix belongs in the shared brace counter (`cognitive_brace.py`). Merge or supersede #684 when this lands.

**Key examples:**

- `if (a ? b : 0) {}` followed by a ternary in a `for` header scores 4, not 2, in .js, .ts, .jsx, .vue, .c, .java and .cs.
- #684's JavaScript tests pass unchanged.
