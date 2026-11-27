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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/048-nd-without-logical-operators/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"f1c6adc0e8adf7ef4080fa738de67848bab31919e70b7d130e03b1471f483576","plan":"da5122d6b84d773d0da3156a639736780176a570dc7a620ce74d655034a902bd"}}
```

**Beneficiary:** Users who gate code on nesting depth (`-END`), including the reporter of [#693](https://github.com/terryyin/lizard/issues/693) using it in a safety-critical pipeline.

**Goal:** ND reports only the nesting of control structures, as [ADR 0001](../../docs/adrs/0001-ubiquitous-language-accepted.md) describes, so `&&`/`||` in a condition no longer adds a level.

**Scope:** The developer approved this as a deliberate metric change on 2026-11-26.

- Required behavior: `&&` and `||` never open an ND level, inside or outside a condition, for every language whose nesting depth counts them today (the default C-family set and Groovy's own set). Control structures and their `else if` handling keep their current ND.
- Update the tests that pin the old behavior.
- Note the change in the release notes, because existing ND values and `-N` thresholds drop.
- Scope limits: CCN, NS (`-ENS`) and cognitive complexity are unchanged. The ternary `?`/`?:` is not a logical operator and keeps its current ND contribution. Python's `and`/`or` already add no ND.

**Key examples:**

- From #693, in C: `plain` (an `if (a)` inside a `for`) and `logical` (the same with `if (a && b)`) both report ND 2. `logical` reports more today (3 for `for(;;){ if (a && b) { x(); } }`).
- `int c = a && b;` alone in a function reports ND 0. It reports 1 today.
- `if (a || b && c)` inside a loop reports the same ND as `if (a)` there.
- The same functions report the same NS with `-ENS` before and after the change.
