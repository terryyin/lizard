# Committed optimization performance qualification

Candidate: `8dac7b80c2478d65b54d8ec96d6aa36bf4530b91`.
Current target: `9979f106a955aef2460ed6fe7ebb5782b755bcad`.
Cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
The earlier `74bc899` experiment remains unchanged in
[qualified-performance.md](qualified-performance.md).

## Decision before timing

Observed on Territory.local before launch: 16 CPUs, 48 GiB RAM, load
4.980/3.557/3.592; 301,269 free and 632,996 inactive 16-KiB pages.
No paired benchmark, pytest or profile command was found. Other native
worktree activity outside that observation remains unknown. The coordinator
confirms no other owned heavy work; its ordinary CI observer remains active.

Decision: run four comparisons serially. Each uses the same prepared Nix
Python 3.9.11, two alternating API workers or one serial CLI process, 20
warmed pairs and 1.5-second calibration. Existing measurements imply roughly
one minute plus setup per comparison. This is a credible modest workload
with available CPU and memory. The slice-5 agent owns commands through their
terminal results. Reconsider before another attempt if contention, failure
or unresolved noise changes the basis; do not repeat for a favorable result.
All four upper paired 95% slowdown bounds must be strictly below 3%.
The driver refreshed this decision at launch: load 4.852/3.815/3.683;
148,073 free and 706,367 inactive pages, with no competing observed command.
The complete launch observation is retained in `optimized-decision.json`.

## Frozen inputs and public observations

The separate cache is
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80`.
It contains exact Git archives of the three revisions, unchanged immutable
`paired.py`, original manifest, 128 affected source files and 100 broad files.
The original 92 + 4 supplemental + 32 labelled additions are unchanged.
Complete source/corpus manifests, original input hashes and driver bytes
are checked before and after timing. No mutable installation is copied.

`qualification-verify.py` observes actual imported facade, registry, core,
reader, state, function and test-helper paths. Candidate: 128/128 expected
rows pass with independent FileAnalyzer agreement. Baseline differences
remain recorded comparison context. All three broad CLI observations return
the expected warning exit 1 and have identical complete output SHA256
`49e9a7922e48f71ac1702f2da38939d93c24b4339d0d3618dfdfdebe1287150b`.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/optimized-qualification.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/optimized-qualification.py --measure
```

Setup runs in the selected checkout; subprocesses run the literal immutable
harness from the copied corpus directory with the same selected interpreter.
Each distinct result has its own raw pairs, pilots, loop count, medians,
standard deviations, geometric paired change and two-sided 95% interval.
The driver refuses to overwrite the freeze, decision or any result.

## Results

| Comparison | Median slowdown | Paired geometric change | Paired 95% interval | Gate |
| --- | ---: | ---: | --- | --- |
| Current target, API | +7.102% | +7.026% | [+6.432%, +7.623%] | Fails |
| Current target, CLI | +0.485% | +1.452% | [+0.072%, +2.851%] | Passes |
| Frozen baseline, API | −4.411% | −4.411% | [−4.995%, −3.823%] | Passes |
| Frozen baseline, CLI | +0.570% | +0.998% | [+0.544%, +1.453%] | Passes |

Each distinct comparison ran once with 20 warmed alternating pairs, and
every raw timed batch exceeded one second. Exact pilots, loops, raw pairs,
medians, standard deviations, commands and interpreter are copied in
`optimized-{current,frozen}-{api,cli}.json`; terminal logs remain in the cache.
The driver and corpus hashes are unchanged. `optimized-freeze.json` and
`optimized-{candidate,current,frozen}-source-hashes.json` identify the actual
source bytes; `optimized-postcheck.json` retains imports, assertion results
and evidence hashes after all four commands ended.

The current-target affected API interval is a material failure, entirely
above the strict 3% budget. Three passing gates do not offset it. G4 remains
open; this revision is not performance-qualified for contributor publication.
No noise waiver, repeat, new profile or runtime edit was made. A subsequent
coherent optimization needs a new exact-revision qualification with all four
unchanged gates; the original and this experiment must remain available.

## Remaining-plan reassessment

The second material failure invalidates the premise that moving bookkeeping
alone meets incremental throughput. No repeat is justified by this tight
interval. The smallest remaining outcome is the same qualified correction;
dropping annotations, narrowing the corpus or weakening 3% would change it.
Keep completed slices 1–4 and both experiments. Refine unfinished 5 into a
bounded Structure profile/owner-query simplification, then exact-revision
qualification as 6 and unchanged contributor delivery as 7. Existing canonical
last-token ownership may avoid traversing nested state at non-signature colons;
the profile must evaluate that hypothesis before edits. This remains the same
reader responsibility/PFE/Accepted ADR direction, with no raw-source shortcut.
Seven cohesive slices fit the supplied unconstrained sizing; no resplit or
human decision is needed. G4 receives proof in 6 and G5 in 7. The missing
retained numeric driver exit of this failed experiment stays explicit in
`optimized-postcheck.json`; final qualification owns terminal exit receipts
as well as complete unchanged raw pairs, hashes and public observations.
