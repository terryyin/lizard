# Corrected PR534 performance qualification

The initial corrected candidate is `74bc899db957d27004f9fd58b095e7394fba93dd`.
Current target is `9979f106a955aef2460ed6fe7ebb5782b755bcad`; a fresh fetch
before freezing found no advancement. Cumulative baseline is
`2e21e404c1ab9f4e0b49acc80816e816629d0670`. The three source archives are
immutable comparison inputs, not execution workspaces.

## Prospective decision and immutable inputs

On Territory.local, before timing: 16 CPUs, 48 GiB RAM, load
2.359/2.834/3.288. `vm_stat` showed 115,332 free 16-KiB pages and 842,792
inactive pages; historical compression/swap totals do not establish current
pressure. No `paired.py`, pytest or profiling command was running. Other
native worktree activity remains unknown. No other heavy proof is owned by this
execution. The comparison uses two alternating API workers or one serial CLI
process, with modest memory needs. Existing timings suggest approximately one
minute plus calibration per comparison.

Decision: run four comparisons serially, each 20 alternating warmed pairs
calibrated to 1.5 seconds. Every actual timed batch must exceed one second.
The slice-4 agent owns commands through their terminal results. Reconsider
before another expensive attempt if observed contention, changed conditions or
an unresolved interval invalidates this basis. Do not repeat for a favorable
result; preserve the initial result and fix any justified quiet/longer retry
prospectively. The upper paired 95% slowdown bound must be strictly below 3%
for every affected/broad incremental/cumulative comparison.

An isolated cache preserves the original historical corpus:
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance`.
It contains the unchanged 100-file broad corpus, original 92 affected inputs,
four curried supplemental controls, and 32 labelled signature/runtime/lexical
inputs: 128 affected files total. Original `manifest.json` remains unchanged.

| Input | SHA256 |
| --- | --- |
| Immutable `paired.py` | `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5` |
| Original manifest | `113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5` |
| Complete affected manifest | `bfb1b659eaad41a0bcc7c96f43e5b2b66bccde194f7cd5f15323b03e7dc2e053` |
| Broad manifest | `7fdc88d87a7c29416715d59335ef6c983d32451afe679e0e154fd55dfaf8a84d` |
| Labelled addition manifest | `8c7d9247025f266575e74d3f308cea79f6e9161d27ddf4fc6775a659d37b9985` |

`freeze.json`, the per-snapshot source manifests, and all raw observations are
retained in that cache. `qualification-freeze.py` asserts every original input
hash before copying, archives the exact revisions, and writes additions only
to the isolated copy. `qualification-verify.py` asserts actual imported facade,
registry, core, state, function, reader and test-helper paths before collecting
public `analyze_file`/independent `FileAnalyzer` tuples. Candidate: 128/128
expected rows pass. Baseline mismatches are retained as comparison context.
All three broad CLI runs exit 1 for the expected threshold warnings and have
the same complete output SHA256
`1fc07b51660a04c6d0177bdd18e895bc75289317ad84d1262606a15b4906f9bf`.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-run.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-run.py --measure-only
```

The final command reuses the accepted pre-timing observations only after
checking the candidate summary and every frozen source hash. It invokes the
literal immutable harness from the copied `corpus` directory with the selected
Nix interpreter, `--pairs 20 --seconds 1.5`, and `--workload api` or `cli`.
Setup runs in the selected execution checkout; no mutable dependencies are
copied into the comparison archives or corpus.

## Initial qualification and bounded diagnosis

| Comparison | Median slowdown | Paired 95% interval | Gate |
| --- | ---: | --- | --- |
| Current target, API | +8.886% | [+8.480%, +9.567%] | Fails |
| Current target, CLI | +0.936% | [−0.635%, +0.729%] | Passes |
| Frozen baseline, API | −3.020% | [−3.647%, −2.699%] | Passes |
| Frozen baseline, CLI | +0.175% | [+0.018%, +1.311%] | Passes |

Every workload ran once with 20 warmed alternating pairs; all actual batches
were at least one second. Raw pairs, medians, standard deviations, loops,
pilots, interpreter and commands are in the four cache JSON results and logs.
The tight incremental API failure is material, not unresolved noise. No timing
retry is authorized by it. The three passing gates cannot offset that failure.

Prospective diagnosis: after all four comparisons terminate, run one serial
5000-analysis profile per current/candidate snapshot on the unchanged
`block_branches.js` control. Its full public tuples must match before profiling.
This answers whether shared preprocessing bookkeeping adds cost even without
an active signature probe; it does not allocate the full affected slowdown,
where corrected function counts and typed syntax differ. The prior healthy
headroom and low timing variability remain credible for this short diagnostic;
no competing owned workload is active. Preserve profiles, then simplify the
existing owner if supported. Do not time uncommitted edits as candidate 74bc899.

The control's seven public fields match in the pre-timing observations and
both profile records. Current/candidate profile totals are 1.4484/1.5639s;
`preprocess` own time is 0.0631/0.1017s over 5,000 analyses. Additional
string-prefix checks are visible in the profile. This supports removing
whole-stream bookkeeping; a single profile does not allocate the full 9%
affected slowdown. Full profiles remain in the cache, with a compact exact
selection in `initial-profile-summary.json`.

## Structural optimization, awaiting exact-revision timing

The runtime state now retains the opening prefix and emptiness of its completed
parenthesis group. Shared preprocessing queries that context only at a colon;
it no longer tracks every token's prefix or maintains a second parenthesis
stack. The existing function/signature owner supplies group eligibility and
canonical ternary depth. Confirmed annotation tokens share the existing marker
dispatch, while provisional binding summaries remain with their arrow.
Type recognition, separator confirmation and the original analysis stream
remain unchanged. No raw-source fast path was added.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput.py test/testOutputCSV.py test/testApplication.py test/test_options.py test/test_extensions/testCognitiveComplexity.py test/test_languages/testOptionalOperators.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-verify.py /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance/inputs.json --require-expected > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance/optimization-metrics.jsonl
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-compare.py
```

Results: 715 passed, five pre-existing skips; 128/128 expected public rows pass
with independent FileAnalyzer equality and import assertions. The comparison
observes all 128 cases against the exact semantic candidate archive: names,
start/end lines, CCN, NLOC, token and parameter counts are identical. Setup
supplies only the retained source inputs. Assertions are the public tuple
checks in `testJavaScriptTernaryArrows.py` and the verification/comparison
scripts; no parser logic is mocked. No tests or expected values were changed.

The exact uncommitted runtime patch is retained as cache `optimization.patch`,
SHA256 `b0c264731cf517bdfd0d022e810c6400f7dbebfcf8308a9bd2fb0cc6195c6397`.
`qualification-optimization-proof.json` explicitly labels it **not timed**.
The source manifests, four complete initial timing results and freeze metadata
are copied beside this record. Those initial results still describe 74bc899;
they establish no performance verdict for the optimization. The next planned
qualification must freeze its new committed revision and satisfy all four
unchanged strict gates. Initial-only scripts refuse to overwrite the earlier
freeze/results; retain those files on any later comparison.

## Optimization consumer trace

The installed trace was run with the retired `return_annotation_depth`, new
`return_annotation_context`, group fields, moved `parameter_group`, all owner
module paths, shared `preprocess`, TypeScriptReader/States, and reached JS/TS
test helpers. The complete literal command and generated record are retained
as cache `optimization-consumers.md`: 296 populated dispositions, zero blanks.

| Consumer | Disposition and observation |
| --- | --- |
| JS/TS/JSX/TSX/CJS/MJS/Vue and registry | Run: 715-case focused selection and 128 public/FileAnalyzer comparisons; existing explicit tuple, lexical, binding, surrounding-function and Vue-offset assertions |
| Core preprocessing, facade, CLI/CSV and cognitive profile/extension | Run: combined core/CLI/CSV/cognitive tests; all seven public fields remain equal; unchanged formatting boundary retains its prior real typed CLI observation |
| JS/TS widget helpers and fixtures | Run: active suite assertions; the five unchanged skips are not represented as passing proof |
| Inherited comment handling | Run: JS/TS comment, regex and template range controls; independent CLikeReader preprocessing remains unchanged |
| Other language/C-preprocessor implementations | Excluded: they do not consume the TypeScript group/annotation context |
| Frozen review records and standalone research scripts | Excluded: immutable comparison/history inputs, not current runtime callers |
| Source/plan/guidance and unchanged CLI/cognitive descriptions | Reused: no outward contract or wording changed; final hosted CI remains a later obligation |

Broad literals moved unchanged with the eligibility helper are not retired
values; the named helper and old/new module contracts were explicitly traced.
No uncovered semantic consumer remains. Performance qualification, independent
refactoring/delivery and final full-suite/make/hosted actions remain with their
planned owners.
