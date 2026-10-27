# PR531 — Objects inside arrow expressions

Disposition: defer the real, worthwhile defect to
[SEED-013#arrow-object-boundaries](../../seeds/SEED-013-arrow-object-boundaries.md#arrow-object-boundaries)
and leave the authored PR unmerged. Coherent brace/type/operator correction and
regression cleanup are estimated at 30–45 minutes, above the source's fifteen-
minute cutoff. No product correction was attempted; no hosted write occurred.

Source: https://github.com/terryyin/lizard/pull/531.
Frozen and refreshed open head: 47aa35735cccba962e507edb179eae1154ab6ad2.
Fork branch: FabioMalpezzi/lizard, pr/js-object-in-an-arrow-function-body.
Original base: 01afe9c228ba2a3dc5253259d8415d2ef47f0b71.
Current target: 7ac449f59b3f155fd159ce9aad6794a422045632.
Cumulative baseline: 2e21e404c1ab9f4e0b49acc80816e816629d0670.

The report reproduces through analyze_file and the CLI: box ends at 4 rather
than 2 and layout at 5 rather than 4. Raw531 fixes it in JS/TS/JSX/TSX and Vue,
but string/template literal function-type returns introduce false return-named
functions and extend their enclosing body into the following function. Word
and comparison operators still misread object operands. The 64-case public
reproducer distinguishes 44 target failures from 34 raw failures (5 introduced,
29 existing). It asserts names, start/end lines and CCN without parser mocks.

The whole affected function/object/binding/declaration concept and shared
consumers are reviewed in the
[durable evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md).
That record retains metadata, complete patch, discussions/checks, explicit
expected/actual outputs, CLI logs, raw timing pairs, source hashes and profile.
It also distinguishes non-discriminating existing failures from new regressions
and leaves unrelated arrow parameter/newline/ternary work with later reviews.

The raw JS/TS/ES6/TSX/Vue suite passed 273 tests, 5 skipped. The initial runner's
mixed reader import was diagnosed through before/after module paths: facade
script-dir insertion for python-c. The retained final fresh subprocess asserts
raw reader and helper paths and passes; real-script API/timing/profile and CLI
evidence is unaffected. Existing passing suites omit the new counterexamples.

The fixed corpus has 100 original source files plus 64 related inputs, 164 hashes.
Each workload has 20 warmed alternating pairs with batches above 1 s. Affected API
median +3.0978%, paired 95% CI [+2.0154%, +4.8117%]: the strict <3% gate is unproved.
Broader CLI median -.0551%, paired 95% CI [-.2284%, +1.2813%] passes. No rerun was
made to obtain a pass. A bounded unchanged-output profile records 15,000 new brace
decision calls without claiming to explain the entire timing difference.

The default-path comparison owns incremental/cumulative cost: all 100 production
files were compared, with only PR527 NS methods differing between target and
the frozen baseline; those methods are not called and the imported initializer
wrapper is unchanged. ENS performance is not separately established; four Vue
cases are explicitly included. PR528 remains absent. Final corrected-code full
pytest/make/current fourteen-job hosted CI and performance belong to SEED-013.

PR533/PR534 both inherit raw531 and use its expression-body state; their report
examples remain identically wrong with/without531. Later review slices own
reconciliation. Shared implementation does not create a formal backlog supplier
agreement or justify merging an invalid prerequisite.

Before allocation, identity evidence read all sources without gaps on execution
HEAD 16eed (highest 012) and clean trunk 7ac (highest 011); full candidate 013 was unlisted
without history seed/record. The installed backlog add placed it after regex012
under the unchanged maintenance direction. No plan or Take was made for it.
Slice completion, source acceptance, independent refactor and delivery remain
with the coordinator; this record does not mark the slice done.

Independent source_review_3 read only the selected seed and returned its separate
commitments/qualifications/journeys. Source comparison: the real report and user
benefit are established; valid literal-type/operator source requires the recorded
names/spans/CCN, while raw531 fails those expectations. Repeated comparable raw
timings meet the measurement promise without falsely accepting an unproved gate.
The authorized >15minute branch now has a linked canonical story with beneficiary,
reproducers, expected results, remaining work and PR link; future correction,
consumer tracing, final gates/merge/thanks remain explicitly with that story.
No merge/thank-you is due for this unmergeable raw revision; later selected PRs
retain their own disposition owners. G3 records the source's explicit deferral.
Current unchanged product's full `nix develop -c python -m pytest` passed
1687tests,7skipped; prior make proof remains applicable. No new product test or
source change was made in this slice.
Independent refactor_531 validated all164hashes/64inputs, intervals, profile
equality, links and backlog order; no edits or uncovered consumers, and returned
## REFACTOR COMPLETE. All32changed files are below250lines (maximum196).
