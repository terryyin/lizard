---
id: SEED-058
status: active
planted: 2026-11-16
planted_during: lizard process pr
scope: JSX element text // closing-tag fix from PR #567
---

# SEED-058: Process PR #567

<a id="process-pr-567"></a>

### Process PR #567 — Fix JSX element text with "//" swallowing the closing tag

**Identity:** SEED-058#process-pr-567
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"b92a6dce1c9d1c5467b042adca3a2482f2af076e3de4268d887df1bfd737ae3c"}}
```

**Goal:** Maintainers disposing
[PR #567](https://github.com/terryyin/lizard/pull/567), and users analyzing
JSX/TSX whose element text contains `//` (for example a URL), receive one
complete disposition: subsequent functions after such elements are reported
with correct names and boundaries when the contribution remains valid on
current trunk, or the PR stays unmerged with an evidence-backed reason and any
required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/567. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
JSX/TSX users whose later functions vanish when element text contains `//`
(regression versus 1.25.0).

**Scope.** This pull request only. Compare the claimed JSX element-text case
on current trunk through `lizard_languages` TSX support and the PR's regression
test. Correct remaining defects on a reachable contributor branch, preserving
contributor history. Qualify the final candidate with the full suite, style
checks, and applicable performance screening for per-token/parser changes.
Merge only when those gates and required hosted checks hold; otherwise record
the precise unmet gate.

Deferred promises: unrelated languages, other PRs, and broad tokenizer
refactors beyond the named element-text `//` case. Rejection constraints: do
not merge an inadequate, obsolete, or conflicted candidate to finish the
cycle; do not weaken discriminating JSX/TSX expectations to disguise a
failure; do not accept a material runtime regression without an explicit
recorded decision. Fabio cohort correction bound: defer correction above 15
minutes (review and measurement excluded).

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given `function Card() { return <p>Visit https://example.com today</p>; }`
  followed by another function, both functions are reported with their own
  line ranges rather than swallowing the second.
- Given `//` inside a `{...}` child expression or ordinary JS/TS code outside
  element text, `//` remains a line comment.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `7d449349c96df49b87850ff6694ef0b0563fc62d` on
`FabioMalpezzi:fix/jsx-url-in-element-text`; base `master`;
`maintainer_can_modify` true; merge state MERGEABLE/CLEAN; evidence under
`.planning/evidence/pr-567/`.

**Outcome:** Merged. Final contributor head
`7d449349c96df49b87850ff6694ef0b0563fc62d` landed as merge commit
`967fda152a52cb0f6f8a4df4b7577ec20dab857e`. Acknowledgement:
https://github.com/terryyin/lizard/pull/567#issuecomment-6904054746.

