# Exact canonical-root revision performance qualification

Candidate: `737aeaf7334b8e64f6ab0011c0f2ea65c356b06d`.
Current target: `34cd43ddd1915438f3b440088672bfe4a08d96bf`.
Cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
All six fixed paired 95% upper slowdown bounds pass strictly below 3%.
All earlier failures, raw timings and diagnostics remain unchanged.

## Prospectively fixed protocol and resource choice

Original128 means the original92 +4 supplemental +32 signature controls.
Augmented166 separately adds all38 query/trivia controls. The unchanged broad
workload is100 files. Complete input/corpus bytes match the prior dispatch
freeze. Each API corpus and the broad CLI compare separately against current
and frozen. No corpus, weight, expected row or threshold changed.

The immutable original paired.py hash is
`3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
Historical manifest hash is
`113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5`.
Literal options stay20 alternating warmed pairs,1.5-second calibration and
all measured batches above1 second. All SIX upper bounds must pass; neither
augmented nor broad results can offset an original-workload failure.

Territory.local had16 CPUs/48 GiB, load about6.2/6.2/6.3 and about16 GiB
inactive memory before setup. A native VM used about four CPU cores outside
our ownership. No other owned heavy proof was running; the existing ordinary
CI observer stayed active. Six serial one-core comparisons, alternating API
workers and an estimated7–10 minutes fit the available capacity. Launch load
was7.01/6.43/6.41. Host/process/memory evidence and the bounded decision are
retained. New contention/failure/noise would require reassessment; no favorable
retry was authorized or performed. Other native worktree/VM work was unknown.

## Freeze and actual public correspondence

Cache:
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-737aeaf7`.
New exact Git archives use the selected-checkout Nix Python3.9.11 interpreter;
no mutable dependency installation or environment was copied into snapshots.
Setup is separate from analysis; each timed process uses its fixed corpus cwd.
The coordinator freshly fetched and confirmed current34cd before delegation.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-qualification-freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-qualification-measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-qualification-postcheck.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-qualification-integrity.py
```

Freeze, measurement and postcheck terminated with numeric exit0. All six child
harness commands terminated0. There was no setup failure, retry or extra watch.
Recurring Nix pip platformdirs warnings did not change the interpreter or exits.
The freeze timestamp is2026-10-27T19:00:11.541502+00:00 (Oct28 Singapore).
Exact freeze SHA256 is
`bc84eef6719fb545c208445e29305c0d85c3f1e265e31ee16e2c0699bf9043f2`.

Before AND after timing, actual facade/registry/reader/core/state/helper import
paths resolve inside each named archive. Candidate166 expected rows pass through
analyze_file and independent FileAnalyzer; all seven public fields match the
accepted root-dispatch working-metrics replay. All104 candidate product hashes
match root-dispatch-proof.json and the committed737 archive. Per-snapshot
before/after public metrics are equal. Baseline semantic differences remain
recorded comparison context rather than silently aligned expectations.

All three snapshots use the SAME new absolute corpus arguments for their CLI.
Full stdout bytes are equal before/after, each warning exit1. Historical output
correspondence checks only literal old-to-new corpus-directory substitution.
Actual new full CLI output SHA256 is
`21eeb81e90987827207bc93a9d930a7b34eef76b1325574de746d6362d500a0e`.
No input/source/harness/expected-metric normalization was used.

## Complete independent results

| Comparison | Median slowdown | Paired95% interval | Minimum batch | Result |
| --- | ---: | --- | ---: | --- |
| Original128 API/current | +1.0216% | [-0.4004%, +1.7136%] |1.4819s |Pass |
| Broad100 CLI/current | -0.4146% | [-0.9790%, +0.3111%] |1.4860s |Pass |
| Original128 API/frozen | -8.8258% | [-8.9108%, -8.1846%] |1.5036s |Pass |
| Broad100 CLI/frozen | -0.1801% | [-0.3750%, +0.6326%] |1.4844s |Pass |
| Augmented166 API/current | +2.3928% | [+1.7760%, +2.5280%] |1.5092s |Pass |
| Augmented166 API/frozen | -4.5242% | [-5.0737%, -4.2216%] |1.4521s |Pass |

Every distinct comparison ran exactly once. Twenty warmed alternating pairs per
gate retain every raw duration, pilot, loop count, baseline/candidate median,
standard deviation, paired geometric change and interval in the corresponding
root-{original,augmented}-{current,frozen}-{api,cli}.json. The six literal
child commands and log/receipt hashes are retained in the cache. root-summary.json
contains the six result summaries and all post-observation command receipts.
root-postcheck.json indexes copied evidence and every numeric terminal receipt.

The measurement driver terminated0 at2026-10-27T19:06:58.850361+00:00;
its owned session98118 is closed. All public fields, source/corpus maps,
original harness, literal drivers and input hashes still match after timing.
root-freeze.json fixes the exact revisions, interpreter, drivers and complete
original/supplemental/augmented input hashes before timing. Source maps retain
all104 products; affected maps retain128/166 inputs separately from broad100.

This exact candidate is performance-qualified for the six fixed workloads.
No new general performance claim follows. Qualification acceptance, independent
refactor/delivery, final full pytest/make/hosted CI and contributor publication
remain with the coordinator. This return changes evidence only: no runtime,
test, plan/status, promise, formatter, hook lint, commit, push or external action.

## Coordinator source acceptance

Fresh original-source-only /root/source_review7 read only the seed/contract
and selected-correction amendment; its full179-item inventory and exact launch
are retained in the parent execution conversation. It belongs to the unchanged
measurement obligation, now slice9. Compared against those promises, the four
original incremental/cumulative affected/broad gates independently pass, and
all38 additional relevant controls also pass the two separately frozen API
gates. No corpus, source promise, expected row or threshold was weakened.

Inspected literal freeze/driver assertions, complete raw summaries/receipts
and immutable source/input/import/public correspondence locations. Exact737
preserves the166 public rows and independent FileAnalyzer observations before
and after; original harness and104 product hashes remain held. All120 warmed
pairs and numeric child/driver/postcheck exits are retained. Earlier failures
remain truthful evidence. This proves G4; final local/hosted/contributor
acceptance remains slice10. Independent refactor and guarded delivery follow.

Independent qualification9_refactor returned REFACTOR COMPLETE. Its only edits
make the post-measurement integrity checker allow the owned PLAN acceptance
and exclude its own generated output, then regenerate that manifest. The four
frozen drivers and timing/public/corpus/product boundaries remain unchanged;
no tests or timings were repeated. Integrity, independent audit and whitespace
checks exited0; two installed-trace consumers have dispositions and no gaps.
The previous manifest remains cache/refactor-pre-integrity.json.
