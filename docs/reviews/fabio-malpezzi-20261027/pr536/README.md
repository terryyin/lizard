# PR536 arrow parameter review

Disposition: corrected under SEED-016#arrow-parameter-bindings and
[merged as 1bc7cbd](https://github.com/terryyin/lizard/commit/1bc7cbd64bef352300195c66888139bdf8d8d8e0),
with [one acknowledgement](https://github.com/terryyin/lizard/pull/536#issuecomment-6447350182).
Qualified contributor head: `1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94`.
[Final qualification](https://github.com/terryyin/lizard/blob/4bfd49bc7eb7cd136f6adb3bfad3ab93f9a1631b/docs/reviews/fabio-malpezzi-20261027/pr536/slice5-proof.md) retains full pytest/make, exact-head CI,
history-preserving publication and merge receipts; [performance proof](https://github.com/terryyin/lizard/blob/4bfd49bc7eb7cd136f6adb3bfad3ab93f9a1631b/docs/reviews/fabio-malpezzi-20261027/pr536/slice4-proof.md)
retains all eight passing strict performance gates and corrected semantics.

## Historical initial review and deferral

The original review below deferred the inadequate raw proposal without product
edits; SEED-016 subsequently completed its technical correction. Identity
evidence from local f25aeca and then-current trunk53e4850 is retained in identity-evidence.json.
Installed add queued the story after15 without Take or an executable plan.
execution-summary.json is the original implementation-return receipt; its pending
allocation field describes that earlier handoff, not the merged disposition above.
Before product edits, the complete correction and cleanup was estimated at
30–45 minutes, above the source's fifteen-minute limit. Implementation elapsed:
zero. Review and measurement are excluded. No partial correction was started.

Frozen contributor head: `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`.
Its parent and reported GitHub base are
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
Refreshed current target: `d7be8b90ac5872faf86e329e7d7249c98029f045`.
Execution checkout HEAD remains `f25aeca2e4cbf149ab90073e97d4fee43dabbf32`.
The PR was OPEN at the frozen head. Discussion, reviews, inline comments and
review threads are empty. Historical CI40418663046 passed four jobs; it does
not establish the current 14-job Ubuntu/Windows × Python3.8–3.14 requirement.
metadata.json, inline-comments.json, review-threads.json and
required-workflow.yml retain these observations.

The complete actual change is 39 lines in javascript_bindings.py, three in
typescript_states.py and 41 authored test lines. No prior PR is in its parentage.
bootstrap.py archives the exact parent, raw head and current target, then applies
the actual unmodified Git patch cleanly to a copied current target (`adapted`).
This retains accepted PR532 identifier/annotation/binding handling and PR535's
first-token receiver guard. A raw full-head replacement would discard those
accepted fixes. literal-git-patches.json preserves exact raw/adapted patches;
raw.diff and adapted.diff are whitespace-normalized DISPLAY ONLY copies.
No executor reads those display copies. bootstrap.py reconstructs and applies
the immutable Git diff bytes, never a display file.

The four reported arrows on current target expose parameter counts 0,0,0,0;
both raw and adapted proposals produce the expected 2,1,2,2 through analyze_file
and the actual CLI. Accurate counts benefit API users and avoid incorrect
parameter-limit warnings. TypeScript's official
[parameter destructuring guide](https://www.typescriptlang.org/docs/handbook/2/functions.html#parameter-destructuring)
describes an object argument unpacked into local variables; each formal binding
pattern is one parameter. The ECMAScript
[formal-parameter and binding grammar](https://tc39.es/ecma262/2023/multipage/grammar-summary.html)
also permits empty object/array binding patterns. A formal parameter remains a
slot even when its pattern binds no identifier. ADR-0001's parameter-count
meaning (`docs/adrs/0001-ubiquitous-language-accepted.md`) applies; ADR-0000
preserves human architectural ownership. No architectural exception is needed.

The proposal keeps a second name collector beside existing `_dec` and
FunctionInfo extraction. Anonymous `({key: renamed}, tail) => ...` exposes
`['key','tail']`, while the same named arrow exposes `['renamed','tail']`.
A compound pattern exposes `['key','head','last']` rather than the named
convention `['other','tail','last']`. Nested aliases and JSX/TSX reproduce the
same mismatch. This is a material public-API discrepancy, not just an internal
identifier choice; duplicated-parameter-list output consumes these names.

`({}, [], tail) => ...` is still counted as one parameter instead of three.
The same empty-pattern omission already exists for named arrows; a complete
solution must distinguish formal parameter slots from bound identifiers through
the shared FunctionInfo representation, without a parser-local parallel count.
The anonymous typed-callback input emits a phantom zero-parameter function in
addition to the four-parameter arrow. That phantom also exists on current target,
but correct anonymous capture must skip callback annotation arrows, so its
handling belongs in this follow-up's remaining work. Generic parameter and
return-type controls, rest/default parameters and nested default CCN were tested.

The member assignment `module.exports.add = (a,b) => ...` remains 0 parameters;
the wrapper-call callback improves 0→2. This is overlap with later PR537, not
a prerequisite or supplier agreement. Abandoned `(value && other)` still loses
one enclosing CCN on both target and proposal, a separately observed PR539
concern. The proposal improves ternary default CCN ownership: arrow 2→4 and
outer 3→2. Neither observation authorizes unrelated arrow/newline changes here.

Recommended follow-up beneficiary: JavaScript/TypeScript/JSX/TSX users of arrow
metrics, parameter warnings and parameter-name consumers. Reproducer: the four
reported forms plus `run(({key: renamed}, tail) => {})`,
`run(({}, [], tail) => {})` and the typed-callback case in inputs.json.
Expected result: reported 2,1,2,2; consistent binding names with named controls;
three empty-binding parameter slots; no callback-type phantom function; correct
initializer/default CCN. Remaining work: unify captured bindings with existing
JavaScriptBindings/JavaScriptFunctions/FunctionInfo owners, preserve accepted
532/535 controls, handle annotation syntax, remove redundant bookkeeping,
add discriminating public tests and rerun strict incremental/cumulative gates.
Source: https://github.com/terryyin/lizard/pull/536. No product story identity is
invented in this implementation return; the coordinator owns allocation.

The external proof root is
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536`.
inputs.json has 31 new fixed files, including every authored/report example and
nearby named/anonymous/binding/CCN/call/type controls. Unchanged accepted PR532's
21 inputs and PR535's 13 inputs give 65 files. The performance corpus additionally
contains the same original 100 Python source files. manifest.json hashes all 165
inputs; source-hashes.json freezes 100 product sources per immutable snapshot.
The unchanged central paired.py hash is
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.

Sequential literal runtime commands from the selected execution checkout:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/bootstrap.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/run-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/profile-all.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/finish-proof.py
```

run-proof.py captures initial target/raw/adapted reader groups; a subsequent
reproduce.py run adds the exact parent observation. Reproduce scripts run from
/private/tmp, prepend the intended immutable source and assert facade, registry,
JS binding/function, TS states and FunctionInfo paths before/after collection.
Pytest runs in each snapshot with explicit PYTHONPATH. Timing workers use the
real script outside the execution checkout, cwd/private/tmp and prepend their
specific snapshot. After timing, finish-proof.py rechecks every module identity,
source/input hash and unchanged observation. The root profile.py is avoided by
importing cProfile before inserting snapshot source paths, from/private/tmp.

One initial expectation assumed outer-first nested function order in the typed
return example; actual project convention reports inner functions first.
All initial logs are retained as *-initial-fixture.log. Only that expected order
was corrected, adding exact parameter names; no source/input bytes changed.
The remaining failures retain their semantic expectations. In particular empty
bindings and callback annotations were not weakened to make the proposal pass.
The pre-existing PHP/generic-declaration diagnostics in PR532 remain unchanged.

Focused snapshot results: target 364 passed/5 skipped; raw 302/5; adapted 367/5.
The raw suite predates accepted PR532/535 tests, so its green result is weaker.
Adapted preserves every accepted input assertion, while 31 new cases include
11 remaining failed files. Tests and analyzer failures are preserved in the logs.
Actual setup/assertion owners: reproduce.py's analyze_source_code call and
counts/parameters/ccn comparisons against inputs.json; existing
testJavaScriptParameterNames.py::test_identifier_boundaries,
::test_keyword_names_and_annotations, ::test_binding_colons_are_not_annotations,
::test_typed_defaults_keep_initializer_complexity and output/duplicate consumers;
testTypeScriptThisParameters.py receiver/default/binding/CLI assertions.
New cases are keyed arrow-reported, arrow-authored*, arrow-binding*, arrow-alias*,
arrow-empty-binding*, arrow-typed, arrow-defaults and arrow-ternary-default.

Installed consumer traces include the active current-target website checkout;
the selected checkout's legacy website is not mistaken for the live adapter.
Every located consumer receives run/reused/excluded disposition. Fresh corrected
HTTP behavior is excluded because no PR536 code lands; final full/make/HTTP and hosted
merge gates remain mandatory for any future candidate. Supplemental
analyzer/output/CLI/options/extensions/registry/Vue consumers pass 311 tests.
The installed trace copies split at 230 lines when necessary; all retained files
fit 250 lines, and untracked no-index whitespace checks pass.

measurement-decision.json chooses 20 alternating warmed pairs with 1.2s API and
4s CLI calibration prospectively, retaining every raw pair and host observation.
The longer CLI choice follows535's documented short-batch noise, not a failed536
attempt. There are no invalid-proposal timing retries. Initial host observations
show 16 logical cores, load 10.27 and 47% free memory; only one Nix/proof entry is
active and coordinator-owned heavy work remains idle. Existing platformdirs/pip
warnings recur during Nix setup; every wrapper reaches its terminal result.

All six measured workloads reached terminal results; percentages below use
paired 95%intervals, with raw pairs/medians/variation in each JSON record.

| Comparison | Workload | Median change | Paired95%interval | Gate |
| --- | --- | ---: | --- | --- |
| parent→raw | API | +4.916% | [+3.917%, +5.428%] | fails |
| parent→raw | names/counts | +4.321% | [+3.443%, +4.956%] | fails |
| parent→raw | broad CLI | -0.905% | [-2.568%, +1.200%] | passes |
| target→adapted | API | +6.595% | [+4.267%, +8.578%] | fails |
| target→adapted | names/counts | +7.642% | [+6.762%, +8.426%] | fails |
| target→adapted | broad CLI | -3.460% | [-2.406%, +1.139%] | passes |

The shortest actual batches are 1.174/1.188/3.603s for raw and
1.101/1.210/3.604s for adapted. No pair was removed or run repeated.
Passing broader CLI runs cannot cancel failed affected-workload gates.

The bounded cProfile observation runs 500 loops over five valid named-signature
inputs: annotations.ts, defaults.js, destructured.js, this-binding.ts and plain.js.
Output hashes are identical parent↔raw and target↔adapted, and before/profile/after
within each snapshot. Profiles assert actual module paths. The proposal adds
60,000 calls to `_read_name` and 131,000 total calls in each pair, despite unchanged
output and `_dec` already extracting parameters. `_read_name` accounts for
0.021s cumulative time; read_token grows 0.057→0.085s raw and0.057→0.086s adapted.
These single instrumented observations support redundant collector bookkeeping
as one source of overhead; they do not quantify its exact share of the paired
runtime regression or serve as another timing gate. No optimization was attempted.

finish-proof.py's stronger CLI assertions verify the source bytes, four actual
filename/location rows and every numeric metric against API results. All four
snapshot observations are unchanged after timing and profiling. final-invariants
proves source/input hashes unchanged and all 100 current-target product source
bytes equal accepted PR535's corrected snapshot. No PR536 product change is accepted;
accepted PR535 cumulative API, names/counts, CLI and CLI-ENS evidence remains valid.
No new final CI, hosted comment, merge, publication, stash action or observer
action is performed.

Subsequent trunk53e4850 adds React frontend/runtime/CI setup, with no analyzer
product-source changes. Root inspected the unchanged index.py parameter_count
serialization and current React Playground METRICS table consumer. Accepted535
cumulative analyzer proof remains valid; future merges require the current14
Python jobs plus applicable change-detection/conditional web job gates.
Root's full selected-checkout pytest passed1749/7 (24warnings,2.79s) after the
canonical addition. Independent refactor_536 returned REFACTOR COMPLETE with no
edits, no uncovered consumer and no unnecessary runtime rerun. Future corrected
code, final-candidate full/format/CI and passing performance remain SEED-016's work.
