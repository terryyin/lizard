# Review and dispose of FabioMalpezzi's open PRs

**Identity:** SEED-010#review-fabio-malpezzi-prs
**Source:** [Refined story](../../seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-fabio-malpezzi-prs)

## Goal and scope

Give each PR in the execution-start open queue an evidence-backed disposition:
merge a worthwhile, fully verified revision and thank its author; explain an
unnecessary change; or defer a real problem to a linked correction story.
The source owns the strict slowdown limit below 3% and correction estimate
cutoff above fifteen minutes. Review and measurement time are not that cutoff.
Execution is authorized by the established start dated 2026-10-27.

Retain the source's exclusions. Do not promise all PRs will merge or assume
their proposed implementation is correct. Keep publication of planning records
separate from hosted PR merges; Dough Land does not merge PRs.

## Shared execution inputs

- [Review contract, performance protocol, promise ownership](REVIEW-CONTRACT.md)
- [Design constraints, preparation evidence, execution context](CONTEXT.md)
- [Slice baselines, expectations, focused commands](SLICE-PROOF.md)
- [Frozen execution-start queue](execution-queue.json)

Each slice is one Behavior disposition loop. Read its source PR's whole change,
discussion and checks; reproduce on current target; apply the shared contract.
Preserve the source's strict slowdown limit below 3% and correction cutoff above
fifteen minutes, excluding review and measurement. No partial candidate may merge.

## Ordered slices

### 1. Python nesting boundaries — PR #527
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/527
Proof and expected result: [slice 1](SLICE-PROOF.md).

### 2. Regex literal context — PR #528
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/528
Proof and expected result: [slice 2](SLICE-PROOF.md).

### 3. Objects inside arrow expressions — PR #531
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/531
Proof and expected result: [slice 3](SLICE-PROOF.md).

### 4. Multiline arrow boundaries — PR #533
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/533
Proof and expected result: [slice 4](SLICE-PROOF.md).

### 5. Arrows in ternary branches — PR #534
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/534
Proof and expected result: [slice 5](SLICE-PROOF.md).

### 6. Identifier parameters — PR #532
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/532
Proof and expected result: [slice 6](SLICE-PROOF.md).

### 7. TypeScript this declarations — PR #535
Type: Behavior
Status: done
Source: https://github.com/terryyin/lizard/pull/535
Proof and expected result: [slice 7](SLICE-PROOF.md).

### 8. Arrow parameter forms — PR #536
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/536
Proof and expected result: [slice 8](SLICE-PROOF.md).

### 9. Arrow parameters after member access — PR #537
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/537
Proof and expected result: [slice 9](SLICE-PROOF.md).

### 10. Immediately invoked function boundaries — PR #538
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/538
Proof and expected result: [slice 10](SLICE-PROOF.md).

### 11. Parenthesized expression metrics — PR #539
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/539
Proof and expected result: [slice 11](SLICE-PROOF.md).

### 12. Switch label boundaries — PR #540
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/540
Proof and expected result: [slice 12](SLICE-PROOF.md).

### 13. Get and set member names — PR #541
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/541
Proof and expected result: [slice 13](SLICE-PROOF.md).

### 14. Array values and computed names — PR #542
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/542
Proof and expected result: [slice 14](SLICE-PROOF.md).

### 15. Object spread expressions — PR #543
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/543
Proof and expected result: [slice 15](SLICE-PROOF.md).

### 16. JavaScript line-comment termination — PR #544
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/544
Proof and expected result: [slice 16](SLICE-PROOF.md).

### 17. Conditions in template interpolations — PR #545
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/545
Proof and expected result: [slice 17](SLICE-PROOF.md).

### 18. Keyword member names — PR #546
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/546
Proof and expected result: [slice 18](SLICE-PROOF.md).

### 19. Ternary and optional markers — PR #547
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/547
Proof and expected result: [slice 19](SLICE-PROOF.md).

## Current execution evidence

Slice1's disposition and regression/performance proof are accepted and delivered.
Independent source-only review: source_review_1 launch and return in this execution's
conversation. Its separate commitments match the source: reproduce/value/review,
shared consumers, conditional correction or >15m follow-up, repeated comparable
performance including cumulative effects, final tests/CI, exact-head merge and
thank-you. Each disposition keeps its own proof and remaining owners in the
linked per-PR record; unfinished PRs remain with their ordered slices.
PR527 merged as e871ae4. Expanded control performance also passes; its evidence
and the corrected acceptance ordering are in [527-review.md](527-review.md).
PR528 is deferred to SEED-012#regex-literal-context; [528-review.md](528-review.md)
owns its correctness/performance rejection and linked queued outcome.
PR531 is deferred to SEED-013#arrow-object-boundaries; [531-review.md](531-review.md)
retains its original improvement, introduced type regressions and unproved timing gate.
PR533 is deferred to SEED-014#multiline-arrow-boundaries; [533-review.md](533-review.md)
retains its continuation regressions, failed performance gate and independent child proof.
PR534 is deferred to SEED-015#ternary-arrow-boundaries; [534-review.md](534-review.md)
retains typed-colon loss, the curried-state regression and both unproved timing gates.
PR532 merged as9bc4456 and was thanked; [532-review.md](532-review.md) retains
the corrected identifier/binding behavior, independent refactor, four passing
performance gates, current-base full/HTTP proof and exact-tree fourteen-job CI.
PR535 merged asd7be8b9 and was thanked; [535-review.md](535-review.md) retains
the minimal receiver guard, corrected dependency ancestry, seven passing timing
gates, full1780/7 plus make/HTTP, independent refactor and final fourteen-job CI.

## Story obligations

### G1. Python NS performance controls omitted
Reported: slice 1 — "Independent refactor review found the Python NS API corpus contains the original PR example but omits nearby nested-function, async, match, continuation and class controls; the coordinator merged PR527 before this missing performance gate was resolved."
Story clause: "Measure runtime before and after the change using the same environment, input corpus and options"
Disposition: proved by slice 1: 527-review.md expanded control-corpus protocol and docs/reviews/fabio-malpezzi-20261027/controls/527-python-controls.json; all22controls plus originalexample measured, upper95%bound-.6803%<3%; broader CLI input unchanged and prior complete proof reused.

### G2. Regex candidate breaks division and exceeds slowdown limit
Reported: slice 2 — "The authored PR528 candidate misreads valid member/contextual-name division, loses CCN/tokens, and slows affected inputs19.4428%; coherent correction is estimated30–45minutes and queued as SEED-012#regex-literal-context."
Story clause: "Correct defects and refactor the overall affected changes into a coherent, minimal solution."
Disposition: excluded "Corrections estimated above fifteen minutes belong to the linked follow-up stories, not to this review's implementation scope."

### G3. Arrow object proposal breaks typed bodies and leaves operators unresolved
Reported: slice 3 — "Raw PR531 introduces five literal function-type return regressions, leaves related object operands broken, and has an unproved3% performance gate; coherent correction is estimated30–45minutes and queued as SEED-013#arrow-object-boundaries."
Story clause: "Correct defects and refactor the overall affected changes into a coherent, minimal solution."
Disposition: excluded "Corrections estimated above fifteen minutes belong to the linked follow-up stories, not to this review's implementation scope."

### G4. Multiline arrow proposal breaks valid continuations and slows analysis
Reported: slice 4 — "Raw PR533 ends seven valid continuations early and slows affected analysis 9.7519%; a child without PR531's invalid heuristic still has these errors, and coherent correction is estimated 30–45 minutes, queued as SEED-014#multiline-arrow-boundaries."
Story clause: "Correct defects and refactor the overall affected changes into a coherent, minimal solution."
Disposition: excluded "Corrections estimated above fifteen minutes belong to the linked follow-up stories, not to this review's implementation scope."

### G5. Ternary arrow proposal loses typed branches and outer curried context
Reported: slice 5 — "Raw PR534 leaves typed branches merged and overwrites the outer ternary snapshot when a curried inner arrow is intentionally ignored; both timing gates remain unproved, and coherent correction is estimated 30–45 minutes, queued as SEED-015#ternary-arrow-boundaries."
Story clause: "Correct defects and refactor the overall affected changes into a coherent, minimal solution."
Disposition: excluded "Corrections estimated above fifteen minutes belong to the linked follow-up stories, not to this review's implementation scope."

### G6. Shared parameter extraction reaches accepted NS output
Reported: slice 6 — "The corrected PR532 changes shared FunctionInfo.parameters, so the prior PR527 CLI -ENS timing does not prove the final combined revision; a cumulative starting-baseline versus corrected CLI -ENS comparison is still required before merge."
Story clause: "Also check the cumulative effect of the accepted set against the starting baseline."
Disposition: proved by slice 6: cumulative-cli-ns.json compares starting 2e21e404 with the immutable corrected combined snapshot on the fixed 100-source corpus; 20 alternating pairs, median -0.1271%, paired 95% interval [-1.43994%, +0.46245%]. Full pytest (1721 passed, 7 skipped) covers the accepted Python NS controls. The separate ordinary CLI gate subsequently passed40pairs with upper +2.36131% before merge, as retained in532-review.md.

### G7. Receiver proposal changes a real destructured parameter name
Reported: slice 7 — "PR535's child-only patch on corrected PR532 changes the public names of a valid {first, this: receiver} binding from receiver to first across JS/TS/JSX/TSX; the receiver special case must be limited to the parameter list's first token."
Story clause: "Correct defects and refactor the overall affected changes into a coherent, minimal solution."
Disposition: proved by slice 7: the three-line at_start guard preserves receiver/tail names in all four affected readers; testTypeScriptThisParameters.py has28passing tests versus four raw-child binding-name failures. Consumer proof and current-base full1780/7 plus make pass; HTTP receiver count changes2→1 while accepted532 controls hold. Source-only reviewer7 comparison and exact fixtures/assertions are retained in535-review.md.
