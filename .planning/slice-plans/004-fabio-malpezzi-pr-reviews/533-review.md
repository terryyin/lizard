# PR533 disposition

**Source:** [PR533](https://github.com/terryyin/lizard/pull/533)
**Frozen and refreshed head:**15423852c04aed302f684395fe9522251d88a2f2,
FabioMalpezzi/lizard, pr/js-arrow-function-ends-with-its-line.
**Original base:**01afe9c228ba2a3dc5253259d8415d2ef47f0b71.
**Current target:**7ac449f59b3f155fd159ce9aad6794a422045632.
**Execution increment:**c56a2fe20b4aac1ed2c9595f5384cfa125f32333.

Disposition: leave open/unmerged and defer to
[SEED-014#multiline-arrow-boundaries](../../seeds/SEED-014-multiline-arrow-boundaries.md#multiline-arrow-boundaries).
The defect is real and worthwhile. Coherent correction, test coverage and
cleanup are estimated at30–45minutes, above the source's fifteen-minute limit.
Review and measurement time are excluded. No product correction was attempted;
there was no hosted write, push, merge or thank-you message.

## Evidence and correctness

The [durable findings](../../../docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md)
retain whole-diff/discussion/check review, expected results, snapshot setup,
proof commands, import assertions, raw observations, performance and limits.
The complete head contains PR531's47aa357 and child1542385. Discussion comments,
reviews and threads are empty. Historical four-job CI passed; current CI has
fourteen Ubuntu/Windows×Python3.8–3.14 jobs. That new gate is unobserved here.
Fetched upstream changes since c56a2fe affect CI/claims/records and a metadata
test, with no analyzer changes. Relevant Accepted ADRs0000/0001 are followed.

Target reproduces the reported arrows ending5/7 rather than4/6, m CCN2 rather
than3, pick ending10 rather than11 and handler consuming load/after. Raw fixes
these public names, boundaries, CCN, NLOC and token ownership, including Vue.
The unchanged five authored child tests fail on target and pass on raw.

Raw introduces seven valid newline-continuation errors: multiplication,
less/greater comparisons, exponentiation, calls, subscripts and tagged
templates end on line1 instead of2. Explicit controls in inputs.json and the
retained reproduction logs expose those regressions. Original PR531 literal
function-type return regressions and unresolved object operands remain.
The raw focused JS/TS/TSX/Vue suite passes278tests, skips5, and misses the new
errors. analyze_file and FileAnalyzer results agree for all150 corpus cases;
CLI logs expose the decisive original, continuation and inherited-type results.

An isolated child diagnostic removes only PR531's object-opening predicate
and helper, retaining arrow state initialization/reset and child behavior.
It preserves all five child-test improvements and restores literal function
type returns; all seven new continuation errors remain. Thus an independent
correction need not wait for SEED-013. Shared state is no blocking dependency.
The diagnostic is not an accepted product fix and changes no execution sources.

## Performance and retained proof

Frozen manifest:150 affected inputs plus100 original source files,250 hashes.
The literal central paired.py harness is unchanged; API analyzes only related
cases and CLI the original four source targets. Twenty alternating warmed pairs
per workload, calibrated above1s, retain all raw durations/medians/SD and95%
paired intervals in [533-api.json](../../../docs/reviews/fabio-malpezzi-20261027/pr533/533-api.json)
and [533-cli.json](../../../docs/reviews/fabio-malpezzi-20261027/pr533/533-cli.json).

API median slowdown +9.7519%, interval[5.2695%,11.8135%]: fails below3% gate.
CLI +0.5836%, interval[-0.4817%,2.5769%]: passes. A broader pass cannot offset
the affected failure. No retry sought a pass for already-invalid raw code.

Default path equivalence makes this both incremental versus7ac and cumulative
versus2e21: only unused NS methods differ; constructor/initializer are unchanged;
all150 API results and complete CLI output/exit match. No ENS claim is made.
One bounded unchanged-output5000-iteration block-arrow profile shows increased
TypeScriptStates.__call__ self time0.05965s→0.08606s over235000 calls. Per-token
line/function/complexity snapshots are plausible overhead; full-corpus causality
remains unresolved and corrected code needs fresh measurements.

All verification commands reached terminal results. The coordinator owns final
full pytest/make, independent refactor, source/obligation acceptance and delivery.
No changed-product consumer trace is applicable to this evidence-only deferral;
the queued correction must trace every changed-effect consumer and prove it.

## Follow-up and limits

SEED-014 owns valid continuation/statement distinction, exact metric ownership,
genuine type/body context, consumer tests and acceptable final performance.
It is queued after013 through the installed backlog command, with no Take or
plan. Identity evidence at executionc56a2fe and clean trunk7ac reports014unused,
no history seed/record and gaps[]. Existing queue order and other work remain.
All new files are below250lines. No existing product/test file was changed;
new author tests are retained as an isolated evidence module. Historical CI,
raw diagnostic timings and passing old tests do not qualify a future merge.

Independent source_review_4 read only the original selected seed and returned
its commitments, qualifications and alternative journeys. The original trigger
and expected metrics are established through the API/CLI; raw533 fixes them but
fails the explicitly valid continuations and strict performance requirement.
Its independent-child proof establishes that the invalid parent heuristic is
not a necessary prerequisite; it does not establish a mergeable child. The
source's >15minute branch now has a concrete queued home with beneficiary,
reproducers, expected results, remaining work and PR link. That story owns all
corrected-code consumers and merge/thank-you gates; unmerged raw code is not
presented as accepted. G4 records the source's explicit implementation deferral.
Final independent refactor_533 review validated all 250 hashes, 150 case mappings,
intervals, profile equality, original authored assertions, links and queue order.
It returned ## REFACTOR COMPLETE with no edits or uncovered consumers; all 55
changed files are below 250 lines. Current unchanged product's full pytest passed
1,687 tests, 7 skipped in 3.37s (/tmp/lizard-slice4-pytest.log); prior make proof applies.
