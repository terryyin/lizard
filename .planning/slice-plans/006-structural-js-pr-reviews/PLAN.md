# Structural JS/TS PR reviews

**Mapped story identity:** SEED-010#review-structural-js-prs
**Source:** [canonical story](../../seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-structural-js-prs)

## Goal and scope

Give each of frozen PRs #537–#543 one complete, independently useful disposition
under the
[complete-cycle contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md):
worthwhile verified merge with acknowledgement, or an evidence-backed unnecessary /
inadequate conclusion with any >15‑minute follow-up linked and left unmerged.

**In scope:** Exactly #537, #538, #539, #540, #541, #542, #543 from the
[frozen set](../../../docs/reviews/fabio-malpezzi-20261027/frozen-review-set.json)
and [story map](../../../docs/reviews/fabio-malpezzi-20261027/story-map.md).
**Excluded:** #544–#547, later arrivals, other authors, and unrelated readers.
No phase of a selected PR's cycle is handed to another story.

**Architecture / PFE:** Reuse existing JS/TS owners
(`javascript_functions`, `typescript_states`, `javascript_objects`, shared
bindings/parameter surfaces) under Accepted ADR-0000 and ADR-0001. Do not add
parallel parser counters or PR-specific switches. No new consequential
architectural direction is required for this review batch.

**Assumptions:** Structural-before-lexical backlog order is priority, not a
supplier dependency. #525 remains open and out of this story; #542's known
bracket-expression early-end case stays deferred to that separate change unless
execution finds it inside #542's owned diff. Completed first-batch merges and
later trunk corrections are controls.

## Outside-in proof

Public `analyze_file` / CLI on the claim fixtures; contributor tests once present;
focused JS/TS suite from the contract; then full pytest, `make`, required CI, and
the contract's performance protocol before any merge. Per-slice expected results
and refreshed baselines: [SLICE-PROOF.md](SLICE-PROOF.md).

## Settled decisive premises

Observed in this preparation workspace at `b263e6d` (2026-10-28) via
`analyze_file` on each PR body's claim fixture:

| Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- |
| Frozen heads #537–#543 still open and match story-map OIDs | all | `gh pr view` | True — refresh again before each cycle |
| #537 claim still fails on current target | 1 | `add` param=0; `load` param=1 (expected 2 each) | Defect remains (load drifted from original param=0) |
| #538 claim still fails | 2 | `setup` ends line 4 CCN 1; `m` absent | Defect remains |
| #539 claim still fails | 3 | `total` CCN 1; `merge` CCN 2 | Defect remains |
| #540 claim still fails | 4 | Both label callbacks present; `a` 1–6 CCN 2 — matches PR's fixed expectation | **Already corrected on trunk** — cycle still runs; likely already-fixed / unnecessary disposition unless a remaining gap is found |
| #541 claim still fails | 5 | `get`/`set` methods absent; `create` ends at 8 | Defect remains |
| #542 claim still fails | 6 | `types` ends line 5 | Defect remains |
| #543 claim still fails | 7 | `onClick` absent; `withDefaults` named as method | Defect remains |
| Required hosted checks remain the python-app matrix builds plus `changes` / `web` | all | Recent merged #536 status rollup | True — refresh applicability at each merge |

## Ordered slices

### 1. Arrow parameters after member access — PR #537
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/537 — frozen head `c89470c442d28195bebba45597910c7a481a5043`
Proof: [slice 1](SLICE-PROOF.md).
Behavior: Given a member-assignment arrow with two parameters on current target → complete review cycle for #537 → public analysis reports those two parameters while ordinary member calls remain calls, or an evidence-backed non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 9.

### 2. Immediately invoked function boundaries — PR #538
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/538 — frozen head `c869db90cf2ab6ba93b5c368b71f66e3eaaa6014`
Proof: [slice 2](SLICE-PROOF.md).
Behavior: Given a function body that starts with an IIFE then continues → complete cycle for #538 → outer function end line/CCN and following methods are preserved, or a non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 10.

### 3. Parenthesized expression metrics — PR #539
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/539 — frozen head `daa7d2068ad1bbaaa201b9685aa5d21582ec0015`
Proof: [slice 3](SLICE-PROOF.md).
Behavior: Given an abandoned provisional signature inside an enclosing expression → complete cycle for #539 → enclosing expression keeps tokens/CCN; real arrows keep defaults/body CCN — or a non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 11. Prefer before slice 7 because #543 overlaps abandoned-group / spread-call metrics.

### 4. Switch label boundaries — PR #540
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/540 — frozen head `d225a6d60af856ac155b882d1a3770b7d66457ef`
Proof: [slice 4](SLICE-PROOF.md).
Behavior: Given `case`/`default` labels with callbacks → complete cycle for #540 → callbacks are reported without treating label colons as types (already observed on current trunk for the claim fixture), with genuine annotations/ternaries preserved, or record the already-fixed / other disposition with evidence.
All complete-cycle gates belong to this slice. Original slice: 12.

### 5. Get and set member names — PR #541
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/541 — frozen head `644480ce9f46892fae258696583a6c18649c05dc`
Proof: [slice 5](SLICE-PROOF.md).
Behavior: Given members literally named `get`/`set` beside real accessors → complete cycle for #541 → named members and following functions have correct boundaries, or a non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 13.

### 6. Array values and computed names — PR #542
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/542 — frozen head `ddf075a0cdee4079e584919d818c28b6468a84b0`
Proof: [slice 6](SLICE-PROOF.md).
Behavior: Given array property values and bracketed computed keys → complete cycle for #542 → function end boundaries and following functions remain correct for the cases #542 owns; the known `[m[0]]` early-end left to #525 stays out of scope unless it is inside this PR's change — or a non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 14.

### 7. Object spread expressions — PR #543
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/543 — frozen head `f6a329b488bb1a6e5aa9af43f0cf3e5e761d8353`
Proof: [slice 7](SLICE-PROOF.md).
Behavior: Given object spreads of calls or grouped expressions carrying callbacks → complete cycle for #543 → nested callbacks retained without fictitious methods, rechecking overlap with #539's disposition, or a non-merge disposition is recorded.
All complete-cycle gates belong to this slice. Original slice: 15.

## Current decisions

- One Behavior slice per frozen PR; each owns the entire complete-cycle contract.
- No sibling supplier agreement; lexical batch is deferred promise only.
- #540's trunk observation steers that cycle toward confirming already-fixed
  behavior and recording disposition, not assuming a merge is still required.
- No project S/M/L band or numeric slice timing policy; the contract's 15‑minute
  correction limit (excluding review/measurement) bounds eligible product edits.

## Learnings

- On current trunk, #537's `load` reports one parameter rather than the original
  zero-parameter baseline; the claim still fails the expected two-parameter result.
- #540's claim fixture already matches the PR's fixed expectation on trunk.

## Concern evidence

- Slice-plan refinement: not needed — seven independent complete-cycle Behaviors
  match the refined story, proof is mapped per slice, decisive premises were
  observed in this checkout, and no unresolved boundary/design/sizing concern
  remains within the understood outcome.
- Remaining slice-specific concerns: none identified.

## Story obligations

None yet. During execution, every reported gap, loss, limitation, or interim
behavior uses this section with
**Source:** [canonical story](../../seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-structural-js-prs).
