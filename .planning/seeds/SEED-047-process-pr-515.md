---
id: SEED-047
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: typo cleanup from PR #515
---

# SEED-047: Process PR #515

<a id="process-pr-515"></a>

### Process PR #515 — Correct typos in docs and comments from code review

**Identity:** SEED-047#process-pr-515

**Goal.** Maintainers disposing
[PR #515](https://github.com/terryyin/lizard/pull/515), and readers of Lizard
docs and source comments, receive one complete disposition: the spelling and
spacing fixes land when they remain valid on current trunk (including relocated
doc paths), or the PR stays unmerged with an evidence-backed reason and any
required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/515. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
users reading docs and comments that still contain the reported typos.

**Scope.** This pull request only. Compare each claimed spelling/spacing change
on current trunk through the affected files (including successors of moved
docs). Correct worthwhile remaining typos on a reachable contributor branch,
preserving contributor history. Qualify the final candidate with the full suite
and style checks; performance screening is unaffected for comment/docs-only
edits. Merge only when those gates and required hosted checks hold; otherwise
record the precise unmet gate.

Deferred promises: unrelated wording, other PRs, and refactoring beyond the
named typos. Rejection constraints: do not merge an inadequate or obsolete
candidate to finish the cycle; do not invent functional changes under a typo
PR.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given the changelog archive entry for 2015.01.07, "linux kernal" becomes
  "linux kernel" where that string still exists.
- Given the CLI docs warning-threshold headings, a missing space after the
  colon before `lizard` is restored.
- Given the DefaultOrderedDict, Perl nested-sub, and Ruby-like language module
  comments, "combinated", truncated "anonymou", and "langauges" are spelled
  correctly.
- Given `prompt-history.md`, "behavoir" becomes "behavior".
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `d271c89c78855ed5d6503ea030eaa85d18de43d7` on
`marcelpetrick:master`; `maintainer_can_modify` true; evidence under
`.planning/evidence/pr-515/`.

**Outcome.** Merged as `41831619a79da8461095d719bb51b3d91d5fd9ba` with
contributor head `d271c89c78855ed5d6503ea030eaa85d18de43d7` (maintainer merge of
trunk plus re-homed docs fixes; original typo commits retained). Local
`pytest`/`make` green on the candidate (3069 passed, 7 skipped). Hosted Python
application checks green on that head. Acknowledgement:
https://github.com/terryyin/lizard/pull/515#issuecomment-6699917853.
