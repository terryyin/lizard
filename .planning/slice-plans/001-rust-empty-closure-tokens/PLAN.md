# Qualify and correct Rust empty-closure token counting

**Identity:** SEED-058#process-pr-569
**Source:** [Process PR 569](../../seeds/SEED-058-process-pr-569.md#process-pr-569)
**Source review:** [Independent commitments](source-review.md)

## Goal and scope
Complete the existing PR review with a bounded correction of the wordcount regression, retain the intended one-token empty closure, and deliver only a qualified PR. Exclude unrelated reader behavior or extension redesign.

## Current decisions
- Execution remains in the established isolated workspace and story branch; claim c0d8eec4f57c14b9a0bb60ad453bf38e962f43b9, publisher codex-pr569-834371e2. Original launch baseline 00310c5d; frozen production baseline c0d8eec4. Contributor head 733b99a4.
- PFE: Rust preprocessing already aliases empty closure bars to `|`; all consumers understand that punctuation. Yield it once. A second spelling (`| |`) requires a duplicate exclusion in wordcount and changes operator vocabulary unnecessarily. Keep the responsibility in Rust preprocessing. No architecture choice; ADR-0001 names token count, CCN, language reader and extension consistently.
- File-size decision: testRust.py is 321 lines. Slice 1 moves tokenization tests (character literals, closure bars, logical OR, nested comments) cohesively to testRustTokens.py, leaving basic function/match parsing in testRust.py. Preserve the public analyzer entry and all assertions. No production subsystem split.
- Correction route: maintainer access confirmed by divergent non-mutating Git dry-run to FabioMalpezzi/lizard, branch fix/rust-empty-closure-one-token. Preserve original commit; publish a child correction with only product/test paths.
- Runtime screening uses frozen Rust-heavy API and mixed-language broad CLI corpora, serial 20-pair measurements. Runner default 3% is a screen, not a new project requirement. There is no historical cohort threshold for this owner.

## Decisive premises
- Public analyzer on c0d8eec4 yields 13 tokens/CCN 1 for the supplied example; proposed candidate yields 12/1. Twelve frozen cases cover named/move/nested/comment closures, OR, strings, match arms, return and later function boundaries. Results at /Users/terryyin/.cache/lizard-pr569/*-semantics.json.
- Public FileAnalyzer with wordcount on proposed candidate emits a bogus `| |` word. Spaced bars are already excluded by the existing `|` representation. Observed in the same Nix Python 3.9 environment before planning. New regression test must fail on the proposal and pass on the correction.
- Full proposed suite: 3147 passed, 7 skipped; make passed. Final production bytes must receive the required full checks again.

## Ordered slices

### 1. Preserve empty-closure metrics for all token consumers
Type: Behavior
Status: done
Behavior: Given a Rust empty closure, analyze it through the public API; count its opener once without a logical-OR condition or a punctuation word, while named closures, logical OR and later function metrics remain unchanged.
Proof: Public analyzer tests in testRustTokens.py and current Rust/general extension suites; regression red on proposal, green on correction; absolute token count and CCN, wordcount, Halstead and boundary controls. Then full `nix develop -c python -m pytest` and `nix develop -c make` required by AGENTS.md and lizard-process-pr. Serial paired API/CLI and affected wordcount consumer timing; final hosted matrix/check applicability on exact contributor head before merge. Independent post-change refactor, preserve contributor history, merge guarded by exact head only after all gates pass.

## Concern evidence
Refinement not needed: one cohesive correction and qualification outcome, no independent second behavior. No remaining slice-specific concerns; write access and pre-correction behavior settled. Hosted merge and final target CI remain lifecycle gates.

## Story obligations

### G1. Empty closures leak punctuation into wordcount
Reported: slice 1 — "Independent review found `| |` in wordcount after preprocessing an empty closure."
Story clause: "inspect shared consumers"
Disposition: proved by slice 1: testRustTokens.py::test_empty_closure_does_not_add_punctuation_to_wordcount; red on proposal, green on correction.

## Accepted proof
- /root/correct_tokens preserved all20 original Rust methods exactly by AST comparison;88 focused Rust/wordcount/Halstead and225 extension tests passed. Wordcount regression failed on the proposed `| |` marker, then passed on the existing pipe token.
- Independent /root/refactor_tokens: none — already clean; ## REFACTOR COMPLETE. No proof invalidated.
- Final `nix develop -c python -m pytest`:3152 passed,7 skipped; `nix develop -c make`:exit0. Logs /Users/terryyin/.cache/lizard-pr569/final-{pytest,make}.log. No installed commit hook; make covers required style, no formatter or generation trigger.
- Twelve frozen API cases preserve names,boundaries,NLOC,parameters,CCN,cognitive/nesting values; only intended token/Halstead totals differ. Example13→12 tokens,CCN1. corrected-semantics.json and baseline-semantics.json retained beside logs.
- Baseline c0d8eec4 has identical production to launch00310c5d and PR basef9323a80. Timing has20 alternating pairs; Rust API1.2s batches,wordcount/CLI4s. Corpora20 Rust files/600 functions and137 mixed Python/Rust source files. Original noisy wordcount run retained, longer retry selected prospectively. Runner3% is screening,not a new policy.
- Paired95% intervals: {"api-rust": [-8.00422030527854, 0.4898819239016836], "api-wordcount-long": [-3.308896121689491, 1.9007841261833123], "cli-broad": [-2.687529894593401, 1.792221972436208]}. Benchmark production hashes match corrected candidate.

## Consumer dispositions
| Consumer | Observation |
| --- | --- |
| Rust core,API and CLI | One fewer token per empty closure; CCN and boundaries unchanged. New tests and12-case API snapshots; full suite and paired API/CLI. |
| Wordcount/tag cloud | Existing pipe exclusion remains sole rule; red/green public FileAnalyzer map and225 extension tests,paired wordcount. |
| Halstead | Eight operators in example; no new punctuation vocabulary. Public assertion and focused Halstead suite. |
| Cognitive,nesting,condition tags | Pipe has no condition/nesting meaning; cognitive/ns snapshot and extension suites preserve metrics. |
| Duplicate detection | Corrected stream intentionally shortens sample windows; full duplicate suite within225 extension tests passed. |
| Test discovery/helpers | All20 old methods AST-preserved;215/157-line cohesive split;3152 full suite discovers added cases. |
| Website analyzer | Shared public analyzer consumer; website API tests included in full suite. No frontend/assets change. |
| Historical/planning/doc trace matches | Raw /Users/terryyin/.cache/lizard-pr569/consumer-trace.md retained; these mentions do not consume the live token stream. No runtime effect or stale live reference. |

## Integration and retrospective
- Current targete12c55f8 includes separately owned JSX and cognitive-dict fixes. Preserved them through adapter merge290641fe; integrated `python -m pytest`:3155 passed,7 skipped; integrated make:exit0. Logs integrated-{pytest,make}.log outside cleanup.
- Contributor correction91603058 is a child of original733b99a4 with only product/test paths; all runtime files equal the fully timed/tested24caf457 tree. Merge639c5386 preserves that exact contributor head in execution history. Its complete runtime/test bytes equal GitHub test merge79e57374 (parentse12c55f8,91603058). Thus local integrated proof applies to the hosted merge candidate; original timing remains exact-contributor evidence. TSX generate_tokens and cognitive extension initialization are independent target changes,with no new module-level execution or reached Rust/wordcount path in the timed inputs.
- Delivered branch639c53862445684d002772be777f716ba7b402df; observer /tmp/dough-ci-501/watch-YCG2tS owned bycodex-pr569-834371e2;branchCI pending. Claim trunkc0d8eec4 had no matching trunk observer and remains explicitly unobserved,not registered on this branch observer.
- Automatic retrospective: original contextual review→same-source bounded planned correction reviewed as one execution. Attributable733b99a4,24caf457,91603058; merge3ce37329 imports proposal,290641fe imports unrelated target fixes,639c5386 imports equivalent corrected contributor history. No new product/correctness/refactor findings; no further product changes recommended. Unit tests drove the reader/extension correction through real public analyzer collaborators; no live-service E2E interaction exists in this slice. Existing CLI and website API workflows pass in full suite.
- Process review enabled byskipProcessRetrospective:false. Updated existing ODF-264 with observed unbounded payload/retrieval occurrence; no guidance changes or product follow-up. This is contextual PR qualification/bounded correction,so automatic feature-story Intent Coverage is inapplicable; the original20 assertions were AST-preserved and wordcount red/green observed. No deferred process review remains.

## Execution complete

Product advice: No additional product changes recommended. Exact contributor91603058 CI run43781573967 passed all14 matrix jobs plus changes/ci-proof; web skipped as applicable. Required hosted merge remains landing-owned.

## Closure evidence
- Hosted mergecf064ade5a9788cf3aa8b74f3cb9a464c50fae5e accepted with exact head91603058. Material correction acknowledged once: https://github.com/terryyin/lizard/pull/569#issuecomment-6904202063.
- Execution branch036951858ac5cbc0e6c6f8c4a809568684851902 completed exact CI run43782141571 attempt1; shutdown confirmed,unread0.
- Latest accepted target60b2fb59 carried one more independent TSX change; adapter merge preserved it. Final combined local pytest3156 passed,7 skipped; makeexit0 (closure-{pytest,make}.log). Rust changes remain byte-identical to the qualified contributor; no extra product/refactor finding.
- Initial closure freshness check chose contributor91603058 and refused not-taken-at-base; target sync restored unique base60b2fb59 and returned unchanged. Process review amended with DD-489; no product follow-up or deferred review.
- Contextual/correction Intent Coverage selection remains inapplicable. Lasting knowledge lives in Rust reader and public analyzer tests. No follow-ups need queueing.
