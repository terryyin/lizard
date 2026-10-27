# Corrected performance qualification — slice 4

G6's required affected and broader incremental/cumulative upper paired 95%
slowdown bounds are all strictly below 3%. The highest upper bound is 1.5194%.
This receipt qualifies the measured product bytes; slice status, independent
refactoring, final make/hosted checks and contributor delivery remain with the
coordinator. No commit, push, branch integration or plan-status edit occurred.

## Candidate, corpus and harness

Execution checkout/branch remain the retained SEED-016 identity. Base is
`9276aea2aa337c070169ba40f88b8ce207530243`. Measured `candidate-5` adds exact
patch SHA256 `83578aae44003d5ebcf715f491c9509a045d2e12ace471f3ca2a17c83f9a9bf3`;
full product-manifest SHA256 is
`e90d76c23366e9b30c1fe86f09cb110c3877ba3d0798ab3169c9c2d0309a6ee0`.
The runner verified the checkout's product bytes equal this snapshot before
and after every gate. Final committed product bytes must match that manifest.

Incremental target: `9979f106a955aef2460ed6fe7ebb5782b755bcad`.
Frozen cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
All three are independent Git archives; the candidate's literal patch was
applied to its archive and every product file matched the checkout.
No copied/symlinked mutable source dependency is used.

All 65 original affected sources are byte-identical to frozen `all-inputs.json`
(SHA256 `6ea7b81c31bebfea53d4a335203098001bc68cc30d73c13eb9227e49fa102d52`).
36 supplements freeze the new public regressions' actual language setups,
including Vue/cjs/mjs wrappers, receiver-only/trailing-comma/empty trailing
patterns, typed callbacks with following functions, grouped returns, generics,
defaults and count/duplicate sources. [Supplement provenance](corrected-proof/supplements.json)
names every source test. All 101 sources are timed in both affected workloads.
The 100 original broad Python sources match both the original frozen review
manifest and the Git baseline. [Corpus manifest](corrected-proof/corpus-manifest.json)
SHA256 is `b0c8608fdcc852184628727ed6295ff850abb9c5c494a9783641a34ef52be9df`.

Canonical `paired.py` SHA256 is the intended
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.
At this freeze the older cache already matched it; prior drift is historical.
The fresh corrected root leaves all historical raw/adapted measurements intact.
[Guard adapter](corrected-paired.py) uses the unchanged canonical algorithm,
asserting real facade/reader/core/helper import roots before/after API batches.
Real worker scripts execute from `/private/tmp`; CLI commands use absolute
snapshot `lizard.py` and snapshot cwd. Runtime-script/interpreter/Nix hashes are
in [runtime-scripts.json](corrected-proof/runtime-scripts.json).

## Measured correction

Only `lizard_core/function_info.py`, `lizard_languages/javascript_bindings.py`
and `lizard_languages/typescript_states.py` change product behavior/cost.
`JavaScriptBindings._read_parameter` remains the sole extraction rule.
Named signatures and provisional canonical lists share
`FunctionInfo._append_parameter`; named `add_parameter` still updates the long
signature. Provisional parentheses no longer allocate full metric models or
build unused signature text. The actual candidate callsite suppresses discarded
BindingEvents. Parameter names/counts share one compiled original regex; count
uses one pass while retaining structural slots and mutable `full_parameters`.
There is no second collector, parser-local count or stale memoized public data.
Accepted unsuperseded ADR-0000/0001 remain unchanged; no exception is proposed.

Short calibration/profile runs are diagnostic, not gates. They exposed these
unused models/events and repeated collection. The first semantically correct
optimization (`candidate-2`) failed the real names/counts gate:
paired 95% `[7.2113%,8.6191%]`. Its complete 20 pairs and exact patch remain in
[failed raw result](corrected-proof/candidate-2-target-api-parameters.json).
That clear failure stopped the other gates. The supported source correction
above produced candidate5; it was not an unchanged-code retry seeking green.

## Expensive proof decision and results

One Territory.local host/Nix interpreter (Python 3.9.11), 16 logical cores,
initial load 3.03/3.57/3.83 and about 17GB free+inactive pages; no competing owned
heavy proof was known. Host load/memory/swap before/after every phase are retained
in the `candidate-5-*-host.json` receipts. Owned heavy work stayed serial in one
outer Nix runner. Prospective protocol: 20 alternating warmed pairs, 2s API /4s
CLI calibration targets; every recorded batch exceeds 1s. All pairs are retained;
none was discarded or averaged across workloads. Canonical paired log-ratio
two-sided 95% t intervals use t(19)=2.093. Literal commands/options, pilots,
loops, durations, medians and standard deviations are in each result JSON.

| Comparison | Workload | Loops | Median base→candidate seconds | Paired95% slowdown | Verdict |
| --- | --- | ---: | --- | --- | --- |
| Incremental | API |126|1.98973→2.00444|[0.66685%,1.51931%]|pass|
| Incremental | Names/counts API |122|1.97730→1.99279|[0.32232%,1.34596%]|pass|
| Incremental | Broad CLI |25|4.06954→4.04569|[-0.83020%,-0.23094%]|pass|
| Incremental | Broad CLI `-ENS` |23|4.06767→4.05248|[-0.79666%,-0.15539%]|pass|
| Cumulative | API |123|2.12515→1.93634|[-9.74847%,-8.61930%]|pass|
| Cumulative | Names/counts API |118|2.12680→1.94038|[-9.53106%,-8.57134%]|pass|
| Cumulative | Broad CLI |25|4.03119→4.05104|[-0.42065%,0.59108%]|pass|
| Cumulative | Broad CLI `-ENS` |23|4.06829→4.03953|[-1.71111%,0.10707%]|pass|

[Summary](corrected-proof/summary.json) lists all gates; exact result filenames are
`candidate-5-{target,baseline}-{api,api-parameters,cli,cli-ns}.json` alongside
their raw logs. API batches range 1.90350–2.19012s; CLI 4.00986–4.23544s.
Per-side standard deviations range 0.01609–0.05935s. Every upper bound is <3%.
`-ENS` is included because shared FunctionInfo data is read by CLI output and
warnings while the accepted nesting extension patches its initialization;
the default CLI path independently passes too. A broad result hides no affected
failure, and no ENS-only claim is made.

## Semantic observations and literal commands

Setup copies the checked-in guarded scripts byte-for-byte to the named real
`/private/tmp` files. All interpreter subprocesses share the one outer Nix entry:

```sh
PIP_QUIET=1 nix develop -c python /private/tmp/lizard-corrected-measure.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected/candidate-5-measure.log 2>&1
```

Terminal exit0. [Runner](corrected-measure.py) owns the exact eight harness
commands and [semantic prerequisite script](corrected-verify.py), serially.
[Semantic log](corrected-proof/candidate-5-semantic.log) retains each literal
command and terminal code. Its observations are:

- `python -m pytest -q`: 1944 passed, 7 skipped in 12.07s. The new
  `testJavaScriptFormalSlots.py` exact names/counts assertions cover empty
  named/anonymous slots, receiver/trailing exclusions, callback phantoms,
  grouped returns, generic/default CCN and following functions. Its actual
  CSV/checkstyle/HTML/warning, duplicate-key, CLI and website JSON assertions
  still observe the corrected values. Sources/real temporary files and
  thresholds supply preconditions; no parsing or analyzed result is mocked.
- `testJavaScriptArrowParameters.py` retains reported forms, aliases, sigils,
  wrappers, defaults and real consumer assertions. `test_identifier_boundaries`,
  `test_keyword_names_and_annotations`, `test_binding_colons_are_not_annotations`
  and `test_shared_parameter_names` in `testJavaScriptParameterNames.py` observe
  JS/TS/JSX/TSX/Vue and Python/C++/Ruby/PHP names/counts. Full-suite shared-reader
  regressions also include Go `test_one_with_parameter` (count 2), Python's
  default/lambda/type-bracket parameter tests and the established PHP count 1.
- The unchanged `reproduce.py` runs all 65 frozen API rows and actual CLI against
  candidate5, with imports verified before/after. 63 pass, including all 34
  accepted 532/535 controls. Exact source exclusions remain: member-assignment
  count 0 (its promised wrapper count 2 passes), abandoned outer CCN 2 rather than 3.
  [Full observations](corrected-proof/candidate-5-reproduce.jsonl) retain values.
- Guarded snapshot workers compare all 101 affected tuples against accepted
  candidate0: name, long_name, names, count, CCN, NLOC, tokens and boundaries are
  exactly equal. The frozen sources are the setup; the public analyzer creates
  all tuples. [Candidate observations](corrected-proof/candidate-5-tuples.json)
  and accepted observations are both retained.
- Real 100-file broad CLI and CLI-ENS outputs are byte-identical to accepted
  candidate0 (warning exit 1 on both). Escaped `*-semantics.log.part*.json`
  preserve exact output bytes, hashes and line ranges, including padded tables.
  Website smoke `test_application_check_fetches_built_assets_original_logos_and_real_analysis`
  observes actual HTTP sample metrics 3/6/1; fixture assets supply only startup
  setup. No deployed-site or browser-rendering claim is made.

## Consumer trace, integrity and remaining work

The installed consumer-trace command against 9276aea is retained literally in
the semantic log. Effect terms include `_append_parameter`, `parameter_count`,
`full_parameters`, `parameters`, `read_token`, `javascript_bindings` and
`typescript_states`. [Part1](corrected-proof/consumers-part1.md),
[part2](corrected-proof/consumers-part2.md) and
[part3](corrected-proof/consumers-part3.md) preserve all 367 located rows with
run/reused/excluded dispositions. Runtime reader/model/metric consumers use the
full suite and public parity above. The unchanged frontend's direct `fn[field]`
rendering reuses inspected slice3 evidence; historical/prose/lifecycle hits are
excluded with reasons. No reached runtime consumer has an undisposed row.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/corrected-export.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected/export.log 2>&1
```

Terminal exit0. [Exporter](corrected-export.py) independently confirms the 100
broad hashes against original receipt/Git, exact executed script copies, all 160
final timing pairs and every gate. It retains literal patches as escaped JSON
chunks (source/chunk hashes included), avoiding replay from a display diff.
Every new record stays ≤250 lines. [Evidence manifest](corrected-proof/evidence-manifest.json)
hashes all exported records; record whitespace/EOF and Git diff are checked.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/corrected-integrity.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected/integrity.log 2>&1
git diff --check
```

Both exit 0. The integrity signal verifies every exported hash, reassembles
literal escaped chunks to their source hash, checks measured product bytes and
checks all new records' line limits, whitespace and EOF, including untracked
evidence. No further broad test or performance rerun was needed.

Two process deviations are preserved: slice3's three concurrent Nix entries
and serial recovery remain in `slice3-proof.md`; this slice once launched the
candidate2 reproducer while full pytest still had a running session. Both
terminal observations were successful/expected, with only the existing resolver
warning and no setup error. All later corrected proof used one serial outer
entry; no repeat was run solely to erase that deviation.

G6 has no missing performance observation for these bytes. G4/G5 source
exclusions remain with 537/539. G7/final make, exact-head hosted CI and authorized
contributor delivery remain slice 5. The CI observer ended unavailable because
run discovery failed; this is no hosted CI pass. No subprocess/watch remains
owned by this slice. Coordinator plan wording and shared stash are untouched.

Coordinator acceptance used fresh non-inheriting `slice4_source_review`, whose
only input was the original seed section and execution amendment. Its launch
and individual commitments remain in session history. Source comparison,
the three product diffs, actual consumer assertions, guarded canonical harness,
raw result excerpts and all eight summary intervals establish G6 without changing
the names/slots/annotations/CCN/controls promises. Final qualification remains G7.
The old observer reached terminal `finished` after its unavailable event was
acknowledged; PID99850 is gone. This confirms process shutdown, not green CI.
A later serial GitHub run-list access probe exited0; exact-head CI is still due.
Fresh independent `performance_refactor` returned `none — already clean` and
`## REFACTOR COMPLETE`. No runtime, test or evidence edits invalidated accepted
boundaries; no test or benchmark was repeated. All current-diff files fit the
250-line ceiling, including the coordinator's plan and acceptance notes.
The first guarded commit refused five diagnostic logs with blank lines at EOF.
No commit or push occurred. Their unchanged cache originals were re-exported
through the existing escaped-chunk representation, now also selected for blank
EOF lines; byte-reassembly hashes and `corrected-integrity.py` passed, then
`git diff --cached --check` passed. Product bytes and all timed data are unchanged.
