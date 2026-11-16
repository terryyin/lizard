---
id: SEED-059
status: active
planted: 2026-11-16
planted_during: lizard process pr
scope: JSX empty text token between child elements from PR #571
---

# SEED-059: Process PR #571

<a id="process-pr-571-jsx-empty-text-token"></a>

### Process PR #571 — Fix the empty JSX text token between two child elements

**Identity:** SEED-059#process-pr-571-jsx-empty-text-token
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"98e4799bf0a9f137116c2e0a8252fe232e856453787e2ced91324de812a83367"}}
```

**Goal.** Maintainers disposing
[PR #571](https://github.com/terryyin/lizard/pull/571), and users analyzing
TSX/JSX with `-Eduplicate` or token metrics, receive one complete disposition:
adjacent JSX child elements no longer emit an empty text token that crashes
duplicate detection or inflates token counts when the contribution remains
valid on current trunk, or the PR stays unmerged with an evidence-backed reason
and any required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/571. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
TSX/JSX users whose `-Eduplicate` runs stop with `IndexError: string index out
of range` when an element immediately follows another child with no text
between them.

**Scope.** This pull request only. Compare the claimed JSX tokenizer empty-flush
behavior on current trunk through `lizard_languages/tsx.py` and the PR's
regression tests. Correct remaining defects on a reachable contributor branch,
preserving contributor history. Qualify the final candidate with the full suite,
style checks, and applicable performance screening for tokenizer changes. Merge
only when those gates and required hosted checks hold; otherwise record the
precise unmet gate.

Deferred promises: unrelated languages, other PRs, and broader JSX reader
refactors beyond empty text-token flush between sibling elements. Rejection
constraints: do not merge an inadequate, obsolete, or conflicted candidate to
finish the cycle; do not weaken discriminating TSX expectations to disguise a
failure; do not accept a material runtime regression without an explicit
recorded decision.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given `function Panel() { return <div><p>x</p><B /></div>; }`,
  `lizard -Eduplicate` completes without `IndexError` and reports the function.
- Given tokenizer input `<div><div>x</div><B /></div>`, the token stream has no
  empty `''` text token between the sibling children.
- Given the existing JSX slash-in-text regex control (`Rates` with sibling
  `<th>` elements), token count reflects the absence of those empty tokens
  (45 rather than 47) without changing CCN, NLOC, or function bounds.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `bf59d607c9f74dd69781e2e29581dc27ba848392` on
`FabioMalpezzi:fix/jsx-empty-text-before-element`; base
`f9323a803c4e3f0616566f44036bb8854b8ad136`; `maintainer_can_modify` true;
checks 16 success / 1 skipped; evidence under `.planning/evidence/pr-571/`.

**Outcome.** Merged. Final contributor head
`bf59d607c9f74dd69781e2e29581dc27ba848392` landed as merge commit
`60b2fb5933112ddab2bd540cd29236cd5ec3951b`. Acknowledgement:
https://github.com/terryyin/lizard/pull/571#issuecomment-6904239426.
No product corrections were required. Performance gates (paired API,
api-parameters, CLI; `--seconds 4` after noisy short batches) all passed with
95% upper slowdown strictly below 3%. Full pytest 3085 passed; `make` exit 0.
