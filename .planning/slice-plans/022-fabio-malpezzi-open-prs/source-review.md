# Source review — SEED-034#process-fabio-malpezzi-prs

Independent source-only reviewer (no plan/product/proof in context). Launch
retained in the execution conversation. Returned commitments follow.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Each PR in the execution-start frozen set receives one complete, independently useful disposition | slice 4 | |
| 2.1 | Disposition may be a worthwhile change verified and landed with acknowledgement | slice 1 | observed: docs/reviews/fabio-malpezzi-SEED-034/pr551/execution-summary.json disposition merged; merge b6b6b8c6; thanks comment |
| 2.2 | Disposition may be an evidence-backed unnecessary / inadequate conclusion with follow-up linked and PR left unmerged | slice 4 | |
| 3 | Process every PR by FabioMalpezzi open against terryyin/lizard when execution starts | slice 1 | observed: PLAN.md Current decisions execution-start freeze #551–#554 |
| 4.1 | Refresh the author's open PRs at execution start | slice 1 | observed: PLAN.md freeze decision (gh pr list FabioMalpezzi) |
| 4.2 | Freeze that review set | slice 1 | observed: PLAN.md freeze #551–#554 with head OIDs |
| 4.3 | Freeze the starting trunk revision | slice 1 | observed: PLAN.md starting trunk 4a05e405… |
| 4.4 | Keep later arrivals for separate intake | slice 1 | observed: PLAN.md / seed deferred later arrivals |
| 5 | Every frozen PR owns the full Lizard PR workflow with previous-batch correction and performance limits | slice 1 | observed: docs/reviews/fabio-malpezzi-SEED-034/pr551 complete-cycle evidence |
| 6 | Reproduce claimed behavior on current target through public analyzer or CLI | slice 1 | observed: docs/reviews/fabio-malpezzi-SEED-034/pr551/reproduction.json target red / corrected green |
| 7.1 | Check nearby controls when reproducing | slice 1 | observed: reproduction.json handler-control matches on target and corrected |
| 7.2 | Check shared-language consumers when reproducing | slice 1 | observed: consumer-trace-dispositions.md |
| 7.3 | Record when trunk already produces the expected result | slice 1 | observed: reproduction.json target claim mismatches (defect remained) |
| 8 | Correct and independently refactor worthwhile changes only within the 15-minute budget (review/measurement excluded) | slice 1 | observed: contributor fix + hot-path guards + post-change refactor (helpers/tests) within budget |
| 9.1 | Above budget: leave the PR open | slice 4 | |
| 9.2 | Above budget: link a backlog story with reproducer, expected result, remaining work | slice 4 | |
| 10.1 | Qualify with discriminating regressions | slice 1 | observed: testJavaScriptClassFields.py / testTypeScriptFields.py claim cases |
| 10.2 | Qualify with the full suite | slice 1 | observed: full-pytest-after-opt.log 3021 passed, 7 skipped |
| 10.3 | Qualify with style checks | slice 1 | observed: make-after-opt.log exit 0 |
| 10.4 | Qualify with required CI | slice 1 | observed: Actions run 41996874747 all required builds SUCCESS on b0733889d |
| 11.1 | Paired 95% upper slowdown strictly below 3% on affected API vs current target | slice 1 | observed: attempt6/incremental-api.json passes; upper CI 0.710% |
| 11.2 | Paired 95% upper slowdown strictly below 3% on broader CLI vs current target | slice 1 | observed: attempt6/incremental-cli.json passes; upper CI 0.677% |
| 11.3 | Same API/CLI bounds measured against the current target | slice 1 | observed: attempt6 summary incremental gates |
| 11.4 | Same bounds measured cumulatively against the frozen baseline | slice 1 | observed: attempt6 cumulative-* mirrors incremental (frozen trunk equals target) |
| 12 | Resolve interactions between PRs before accepting them | slice 4 | |
| 13 | Merge qualified PRs guarded by exact head | slice 1 | observed: gh pr merge --match-head-commit b0733889d → merge b6b6b8c6 |
| 14 | Thank the contributor when authorized during execution | slice 1 | observed: https://github.com/terryyin/lizard/pull/551#issuecomment-6653712649 |
| 15.1 | Record the final revision for each PR | slice 1 | observed: execution-summary.json corrected_head b0733889d |
| 15.2 | Record the disposition for each PR | slice 1 | observed: execution-summary.json disposition merged |
| 15.3 | Record the specific gate for any unfinished decision | slice 1 | observed: execution-summary.json (no unfinished gate; merged) |
| 16 | Already-correct trunk, unworthy changes, or corrections above 15 minutes receive a recorded reason or linked follow-up instead of a merge | slice 4 | |
| Q1 | #551–#554 expected starting queue until execution freezes the set | slice 1 | observed: PLAN.md freeze confirms #551–#554 |
| Q2 | Previous-batch complete-cycle contract remains product policy | slice 1 | observed: REVIEW-CONTRACT applied in pr551 evidence |
| Q3 | Sibling order is impact priority, not a technical prerequisite | slice 1 | observed: PLAN.md Current decisions |
| Q4 | Key examples are claims to verify, not automatic acceptance | slice 1 | observed: reproduction + value judgment before merge |
| E1 | Class field without `;` then async/static/get/set → report methods in JS/TS; multiline handler=async field stays one field | slice 1 | observed: reproduction.json corrected claim_match + control_match; ClassFields/TypeScriptFields tests |
| E2 | TypeScript generic function declaration reported with end line and parameters | slice 2 | observed: docs/reviews/fabio-malpezzi-SEED-034/pr552/reproduction.json corrected all_match; testTypeScriptFunctions.test_generic_function_declaration; merged eec8c38b |
| E3 | Quoted TypeScript optional member `?` not counted as ternary; string ternary still adds one | slice 3 | observed: docs/reviews/fabio-malpezzi-SEED-034/pr553/reproduction.json corrected_all_match; test_optional_quoted_member_is_not_a_ternary; merged at 692a456c |
| E4 | `.catch` / `?.catch` under -ENS not nested structures; real if/try-catch still raise NS | slice 4 | |
| X1 | Deferred: PRs after the freeze | slice 1 | observed: seed/plan deferred later arrivals |
| X2 | Deferred: other authors | slice 1 | observed: seed scope FabioMalpezzi only |
| X3 | Deferred: unrelated language readers | slice 1 | observed: seed/plan exclusions |
| X4 | Do not invent an S/M/L sizing policy | slice 1 | observed: PLAN.md Current decisions |
| X5 | Do not merge an inadequate candidate to finish a cycle | slice 1 | observed: merged only after suites, perf attempt6, CI |
| X6 | Do not treat queue order as a supplier dependency without a justified agreement | slice 1 | observed: PLAN.md no sibling supplier agreement |
| X7 | Do not defer correction/performance/refactor/CI/merge/acknowledgement of a frozen PR to another story | slice 1 | observed: #551 complete disposition in this story |
