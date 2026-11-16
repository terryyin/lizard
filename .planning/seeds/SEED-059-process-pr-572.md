---
id: SEED-059
status: active
planted: 2026-11-16
planted_during: lizard process pr
scope: ternary-between-comparisons CCN fix from PR #572
---

# SEED-059: Process PR #572

<a id="process-pr-572"></a>

### Process PR #572 — Count a ternary between comparisons (`a < b ? c > d : e`)

**Identity:** SEED-059#process-pr-572
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"6be7827d14a2a2825df24bf3ef00f449b4e16ae3546a8934bac2a2c19f541c25"}}
```

**Goal:** Maintainers disposing
[PR #572](https://github.com/terryyin/lizard/pull/572), and users measuring
code whose ternary sits between comparisons, receive one complete
disposition: the ternary's `?` counts toward cyclomatic complexity while
generic wildcards and nullable type arguments still add none, when the
contribution remains valid on current trunk, or the PR stays unmerged with an
evidence-backed reason and any required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/572. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
users of every reader sharing the token pattern (C/C++, Java, C#, JavaScript,
TypeScript and others) whose CCN is one too low for
`a < b ? c > d : e`.

**Scope.** This pull request only. Compare the claimed ternary case and the
generic `?` controls on current trunk through `analyze_file` across the
readers that consume `lizard_languages/token_pattern.py`. Correct remaining
defects on a reachable contributor branch, preserving contributor history.
Qualify the final candidate with the full suite, style checks, and
performance screening, since the change alters the shared tokenizer pattern
used for every token of every language. Merge only when those gates and
required hosted checks hold; otherwise record the precise unmet gate.

Deferred promises: other PRs, and broad tokenizer or generics redesign beyond
the named `<...?...>` token. Rejection constraints: do not merge an
inadequate, obsolete, or conflicted candidate to finish the cycle; do not
weaken discriminating expectations to disguise a failure; do not accept a
material runtime regression or a catastrophic-backtracking pattern without an
explicit recorded decision.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given `return a < b ? c > d : d;` in a function, CCN is 2 (trunk reports 1).
- Given Java `List<? extends T>`, `Map<?, ? super T>` parameters, or a
  nullable type argument such as `List<int?>` / `Array<string?>`, the `?`
  adds no branch and parameter counts are unchanged.
- Given a long run of `? ` after `<`, tokenizing stays fast (no freeze).
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `0555c57345a5aa62118c8fa1fa2bcffb6463e749` on
`ssenge:fix-ternary-between-comparisons`; base `master`;
`maintainer_can_modify` true; merge state MERGEABLE; evidence under
`.planning/evidence/pr-572/`.
