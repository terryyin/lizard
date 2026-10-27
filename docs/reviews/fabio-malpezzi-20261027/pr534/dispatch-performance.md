# Exact dispatch-revision performance qualification

Candidate: `13c633b8ed1ab5af4a30decfd87ceaeaf7044093`.
Current target: `34cd43ddd1915438f3b440088672bfe4a08d96bf`.
Cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
All earlier experiments, failures and diagnostic profiles remain unchanged.
This is a new exact committed-code observation after sole-state delivery.

## Fixed prospective selection and resource decision

The original 128 affected API and unchanged 100-file CLI each compare against
current and frozen. The separately fixed augmented 166 API also compares against
both: original 92 + 4 supplemental + 32 labelled signatures, then all 38 query/trivia
controls. Both complete corpora and every source/expected row are byte-for-byte
copies of the previous setup2 freeze. No new source or benchmark weight changed.
All SIX paired 95% upper bounds must be strictly below 3%; neither augmented nor
broad results can offset failure in the original workload.

The immutable original paired.py, 20 alternating warmed pairs, 1.5-second
calibration and every measured batch over 1 second are fixed before timing.
Historical harness SHA256 is
`3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`;
historical manifest SHA256 is
`113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5`.
The historical manifest still describes its original input set; separate complete
128/166 and broad source maps describe these unchanged selections.

Territory.local had 16 CPUs/48 GiB, load 5.83/6.14/6.31, about 15.8 GiB inactive
memory plus 0.27 GiB free. A native VM used roughly 5 CPU cores, outside execution
ownership. No other owned heavy proof was active; the ordinary CI observer
remained active. Six serial comparisons use one active analysis/CLI process
(the API keeps alternating workers), modest memory and about 7–10 minutes.
Available capacity supports this bounded launch. Fresh launch load was
5.72/6.10/6.27. Changed contention/failure/noise needs reassessment; no favorable
retry is authorized. Full host/process observations and the prospective decision
are retained in the new cache.

## Freeze and public observations

Cache:
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-dispatch-13c633b8`.
Fresh origin/master fetch confirmed the exact current target before freezing.
Exact Git archives contain no copied mutable installation or dependency state.
The same selected-checkout Nix Python 3.9.11/interpreter/options execute each
comparison, with setup separate and subprocess cwd rooted in its copied corpus.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/dispatch-qualification-freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/dispatch-qualification-measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/dispatch-qualification-postcheck.py
```

All three commands terminated with numeric exit 0. There was no failed setup or
statistical retry. Actual facade/registry/reader/state/core/helper imports are
asserted before and after collection. Candidate 166 expected public rows pass
with independent FileAnalyzer agreement; all seven fields match the accepted
reader-dispatch replay, and all 104 accepted product hashes match this commit.
Baseline semantic differences remain recorded comparison context.

All three actual CLI outputs use the SAME new corpus paths and are compared
byte-for-byte, warning exit 1. Historical output correspondence is checked only
by exact old-to-new absolute corpus-prefix substitution. Sources, expected
metrics, harness and replay inputs were never normalized or rewritten.

## Results and terminal receipts

| Comparison | Median slowdown | Paired95% interval | Minimum batch | Gate |
| --- | ---: | --- | ---: | --- |
| Original 128 API / current | +3.794% | [+3.102%, +4.156%] | 1.485s | Fails |
| Original broad CLI / current | +0.632% | [+0.136%, +1.278%] | 1.563s | Passes |
| Original 128 API / frozen | -6.156% | [-6.529%, -5.771%] | 1.481s | Passes |
| Original broad CLI / frozen | +0.056% | [-0.791%, +0.816%] | 1.522s | Passes |
| Augmented 166 API / current | +2.882% | [+2.524%, +3.556%] | 1.504s | Fails |
| Augmented 166 API / frozen | -3.835% | [-4.251%, -3.210%] | 1.421s | Passes |

Each distinct comparison ran once with 20 warmed alternating pairs. Every child
harness command, the overall measurement driver, freeze and postcheck exited0.
Numeric exits establish complete observations, not a passing performance result.
Original 128/current API is a material failure, with its entire interval above 3%.
Augmented 166/current API straddles 3% and remains unproved under the strict gate.
Both CLI and both frozen API comparisons pass. This candidate is NOT
performance-qualified. No retry, profiling or optimization followed these runs.

Each dispatch-{original,augmented}-{current,frozen}-api.json and
dispatch-original-{current,frozen}-cli.json retains every raw duration, pilot,
loop count, median, variability and paired interval. dispatch-summary.json holds
all six results and command receipts. dispatch-postcheck.json indexes all terminal
receipt/evidence hashes. Freeze fixes all revisions, drivers, source/corpus/input
maps and literal options before timing. The two complete affected manifests are
separate from the unchanged broad 100 manifest and historical manifest.

Before/after public metrics are identical within each snapshot; candidate 166
expected rows and all seven accepted fields still pass. All source, corpus,
harness and driver hashes hold after timing. Complete three-version CLI bytes
are identical, warning exit 1; actual SHA256:
`88f28b6a85933dd2e1674ca75210fcb891f6511715bcb6a70f3ebbeb39e3e494`.

Raw logs, numeric per-child receipts, freeze/measure driver receipts and pre/post
observations remain in the cache above. G4 remains unresolved; no plan status,
source promise, runtime or test was changed. Independent refactoring/delivery
and any next coherent optimization belong to the coordinator. All commands are
terminal, no extra watch was created, and no throughput waiver is claimed.
