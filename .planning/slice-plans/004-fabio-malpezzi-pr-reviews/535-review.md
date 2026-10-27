# PR535 — TypeScript receiver declarations

Disposition: merged d7be8b90ac5872faf86e329e7d7249c98029f045, corrected head
1a44c0cad59ea3f3dcabe3c1ac601d905437a7c7. All14 final CI jobs succeeded.
Thank-you: https://github.com/terryyin/lizard/pull/535#issuecomment-6436990416.

Source/frozen initial head: https://github.com/terryyin/lizard/pull/535,
`171079e625966ac9c17aee29a5d74256befd85ae`, parent `d0713bf5` (original532).
Fresh Git target `3a11bc6a5452be40a32721b5f6e2ab2fa787ca83` differs from
corrected532 merge `9bc4456` only in five planning/profile files. Selected
checkout HEAD is `c3ca2c73`; analyzer sources match current target. The connector
reports original PR base `01afe9c`. Complete original/child diffs, metadata,
empty discussion/reviews/threads and checks are retained in the durable evidence.
Historical four-job run40418659485 is superseded by final14-job run40472106878.

Benefit: TypeScript's leading fake `this` receiver is not supplied by callers;
excluding it prevents false parameter counts/warnings and incorrect output.
Current target reports onClick2 instead of1 and this-only functions1 instead
of0. The full raw head and child-only proposal fix the reported example.

Dependency: raw535 inherits superseded532 identifier/binding logic. It cannot
replace current corrected532's function reader. A child-only snapshot preserves
the current root-only colon guard and applies only535's two reader changes.
That child still changes `function unpack({first, this: receiver}, tail) {}`
from public parameters `['receiver','tail']` to `['first','tail']` across
JS/TS/JSX/TSX. The original tests checked counts and missed this name regression.

Correction plus cleanup estimate recorded before product edits:10minutes;
completed within that estimate without an upward revision. The three-line
correction recognizes `this` only when existing JavaScriptBindings.at_start is
true and reuses the existing parameter annotation state. It keeps corrected532's
root-depth handling. No parallel count/state, core change or unrelated arrow work.
ADR-0001's metric vocabulary and ADR-0000's human ownership remain respected.

Changed product paths: `lizard_languages/javascript_functions.py` (three lines)
and new `test/test_languages/testTypeScriptThisParameters.py` (114lines).
The new file preserves all three authored receiver examples and adds valid
TS/TSX functions/methods, nested `this` binding keys, typed/default parameters,
body/member `this`, initializer CCN, following boundaries and actual CLI/output
consumers. Existing532 controls retain every full-dollar/type-keyword assertion.

Observable proof:

- 34fixed public-analyzer cases: current target has seven receiver failures;
  raw full head has ten failures including inherited532 regressions;
  child-only has four binding-name failures; corrected has zero.
- New28-test file: current target11failed/17passed; child-only4failed/24passed;
  corrected28passed. Authored assertions are atlines28–30, method metrics43–46,
  binding names/counts57–58, typed defaults/boundaries66–72, member/default
  controls82–84, duplicate/CSV/checkstyle/HTML/warning consumers93–101,
  real CLI threshold and metric row111–114.
- Complete focused JS/TS/ES6/TSX/Vue plus extensions/analyzer/output/CLI/options:
  628passed,5skipped. Extra OptionalOperators/registry consumers pass44tests via
  finish-proof.py. Four installed consumer traces classify current readers,
  metrics and output callers; the actual current website adapter/UI is traced
  in the read-only9bc validation checkout and awaits coordinator HTTP/full proof.
- One initial fixture used the C++-style name Reader::read. All observed TS
  revisions use read; only that name expectation changed. First-fixture logs
  preserve the original failure and unchanged parameter-count expectation.
- Non-parameter metrics and complete broad CLI/-ENS equality are asserted by
  final-invariants.json. Prior PHP/generic diagnostics remain unchanged in532.

Timing: immutable current target versus corrected incremental; starting2e21
versus final combined correction cumulative. External fixed corpus has29affected
JS-family files,34parameter-consumer files and the original100broad Python
source files. Harness hash ec1c9c5e…, Python3.9.11, Territory.local, default
extensions;20alternating paired batches calibrated1.2s, actual minima>1s.

| Workload | Median change | Paired95% interval | Result |
| --- | ---: | --- | --- |
| Incremental API | +1.0673% | [-0.8308%,+1.1712%] | pass |
| Incremental API parameters | -0.0295% | [-1.3579%,+0.2173%] | pass |
| Incremental broad CLI | -0.0735% | [-1.1639%,+1.3758%] | pass |
| Cumulative API | -0.2765% | [-1.1022%,+1.0163%] | pass |
| Cumulative API parameters | +0.0804% | [-1.1567%,+0.9431%] | pass |
| Cumulative broad CLI initial | +2.7396% | [-0.7258%,+3.2665%] | inconclusive |
| Cumulative broad CLI -ENS | +0.7247% | [-1.0627%,+1.2723%] | pass |
| Cumulative broad CLI longer | +0.0997% | [-2.0304%,+0.1009%] | pass |

The initial cumulative ordinary CLI interval remains inconclusive. The single
prospectively fixed retry uses40alternating4s-calibrated batches, fresh headroom
and unchanged source/corpus/harness assertions. It passes at terminal completion
with medians3.84734/3.85117seconds and upper bound+0.1009%; every pair is retained.
The existing Python NS API proof is reused for unchanged Python reader/
FunctionInfo/extension paths; CLI -ENS startup was measured directly.

Commands, full raw pairs, medians/SD, revisions, source/input hashes and retained
original failures are in
`docs/reviews/fabio-malpezzi-20261027/pr535/README.md` and adjacent artifacts.
External originals stay under `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535`.
No invalid raw performance run was needed after its correctness rejection.

All source commitments for this PR are observed below: source-only comparison,
independent refactor, final correctness/performance/full/HTTP/CI proof, corrected
contributor publication, guarded merge and thank-you. Other PRs remain ordered.

## Coordinator acceptance

Fresh source-only reviewer7 independently enumerated the original seed promises
without reading plan/product/proof. Root compared that return to the actual
three-line guard, all28 test assertions, current-target/child red observations,
shared consumers, ten-minute estimate and unfiltered incremental/cumulative
timings. No deferred promise or threshold was silently weakened. G7 is proved
by the four-reader binding-name assertions and real consumers.

Root validated the complete anticipated merge on current3a11bc6, including the
original authored test: full pytest1780passed/7skipped, make exit0 and actual
HTTP receiver count2→1. Receiver-only count0 and accepted532 count controls also
pass, with actual module paths asserted. current-base-proof.json records tree
cf67b36f7ecb3f066d72727ff9a1a5db1c6afaff. No product changed after timing.
Fresh independent refactor_535 returned REFACTOR COMPLETE: none — already clean,
with source/test hashes matching frozen and validated bytes, no uncovered consumer
and no test/performance rerun. Two read-only Nix wrappers briefly raced the shared
ignored .venv setup; the failed ensurepip read supplies no proof. Final sequential
session90108 exited0 and imported Python3.9.11/pytest8.4.2 successfully. Accepted
runtime evidence predates that race; full/HTTP proof used the separate validation
environment. Root prepared a history-preserving dependency merge in an isolated
contributor worktree and resolved its sole reader conflict using validated index
bytes. The resulting complete candidate tree exactly matches cf67b36f.
Agent-commit1a44c0c and the non-forced fork push succeeded. Upstream moved to
95d65b2 through only two planning-file changes; current runtime/test/CI/setup
bytes match accepted proof. The updated validation tree c4a174ed exactly matches
the hosted and landed tree, with parents95d65b2 and1a44c0c. Final head/base
refresh and all14 successful CI jobs preceded the exact-head guarded merge.
Thank-you6436990416 succeeded afterward. hosted-publication.json/final-ci.json
retain exact evidence. Contributor worktree retired; validation is clean atd7be8b9.
The slice increment published f25aeca2e4cbf149ab90073e97d4fee43dabbf32 without
rebase, suffix basec3ca2c73. Its execution-branch monitoring is explicitly
unobserved because the prior observer is terminal finished; this does not replace
the final PR's separately proved14-job CI. The thank-you now links immutable proof.
