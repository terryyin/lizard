# PR531 review and correction

The object-arrow defect was real and worth correcting. During the original
review it exceeded the fifteen-minute correction cutoff and was deferred; the
follow-up correction landed through this PR. Coherent expression-body tracking
and function-type annotation consumption replaced the raw first-character brace
heuristic. Public regressions live in `test/test_languages/testArrowObjectBoundaries.py`.
Merged head `7c7ecbebceed21315df08616b858dbe792fff129` (merge commit
`ac2b454a5ada49e472c5d96635cbbba091c57bc8`); acknowledgement
https://github.com/terryyin/lizard/pull/531#issuecomment-6441355594.
Corrected-candidate performance evidence is retained beside this file as
`corrected-vs-*.json`.

## Source and review basis

- PR: https://github.com/terryyin/lizard/pull/531
- Author/fork: FabioMalpezzi, FabioMalpezzi/lizard.
- Source branch: pr/js-object-in-an-arrow-function-body.
- Frozen and refreshed head: 47aa35735cccba962e507edb179eae1154ab6ad2, open.
- Original PR base: 01afe9c228ba2a3dc5253259d8415d2ef47f0b71.
- Current target: 7ac449f59b3f155fd159ce9aad6794a422045632.
- Review starting baseline: 2e21e404c1ab9f4e0b49acc80816e816629d0670.
- Execution branch remains at 16eed2248cf873d4b7bec42792198378a4e73ad8.

The GitHub connector supplied complete metadata, three-file patch, empty
discussion, empty commit-status list and completed successful workflow
40418647257. Its four historical Ubuntu/Windows Python 3.8/3.10 jobs passed.
Those checks do not satisfy the current target's fourteen-job Python 3.8–3.14
matrix. Full metadata and checks are in review-input.json; proposed-patch.json
preserves every changed hunk. A final metadata refresh still reported the same
open head. No permission, branch publication, merge or comment was attempted.

Users benefit from correct function names, end lines and condition ownership:
the original error makes a short arrow consume its enclosing function and
misattributes following source. Naming and metric meanings follow
[ADR-0001](../../../adrs/0001-ubiquitous-language-accepted.md); existing facade,
core and reader ownership remain unchanged. No architectural exception is needed.

## Observable correctness

inputs.json contains 64 real-source examples with explicit expected
`[name, start_line, end_line, CCN]` summaries. reproduce.py uses the real
analyze_file API and a reused analyzer across files; it mocks no parser.
NLOC, token and parameter metrics are also printed, but are not claimed correct
by all summary assertions. The profile control asserts its complete metric tuples.

| Case | Expected | Target | Raw candidate |
| --- | --- | --- | --- |
| Reported layout/box | box 2–2, layout 1–4, after 5–5 | box 2–4, layout 1–5 | correct |
| Function type returning "ok" | anonymous arrows on 2/3, filter 1–4, after 5–5 | correct | two return-named functions, filter 1–5 |
| Template literal return type | same function boundaries as string type | correct | same new boundary/name regression |
| x in object, void/delete/typeof/await object | make 2–2 CCN 2, outer 1–4 CCN 1 | extended make/outer | still extended |
| > or >= object followed by logical OR | make CCN 2, outer CCN 1 | make CCN 1, outer CCN 2 | still wrong |

The original and author-added object examples run as .js, .ts, .jsx and .tsx.
Type-only syntax runs as .ts/.tsx. JSX attribute arrows run as .jsx/.tsx. Four
Vue script-block cases cover reported objects, in, literal function types and
a valid block-bodied control, with explicit one-line offsets. Block bodies,
parenthesized/nested objects and the author's boolean function-type annotation
remain controls. The proposal also fixes the nullish-object and JSX examples.

Terminal API observations are 44 failed expectations out of 64 on target and
34 out of 64 on the raw candidate. Five failures are newly introduced literal
type regressions; 29 are existing word/comparison-operand defects left unfixed.
These latter failures are deliberately non-discriminating incomplete-fix
evidence, not newly introduced regressions. Corresponding controls pass.
Unrelated arrow parameter, newline and ternary-branch bugs are not asserted
fixed, and no parameter-count expectation is weakened to claim this fix.

Literal replay commands from the execution checkout:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-target
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-readonly
```

Both exit 1, for the distinct failures above; terminal logs are
531-reproduction-target.log and 531-reproduction-candidate.log. Four CLI logs
also show the original example and literal-type regression through lizard.py
on both revisions, with literal invocations and exit 0 (no threshold warning).

The raw candidate's JS/TS/ES6/TSX/Vue focused suite passed: 273 passed, 5 skipped,
278 collected in 0.26 s. Its exact complete Python command, cwd, PYTHONPATH,
assertions and terminal result are in 531-focused-candidate-isolated.log.
It checks facade/reader paths before running and reader/test helper paths after.
This pass exposes missing regression coverage; it does not override reproduce.py.

The first diagnostic runner failed the author's test with target behavior.
531-focused-import-diagnostic.log proves that the raw facade loaded target
reader modules before and after pytest collection. lizard.py::_script_dirs
prepends the directory of abspath(sys.argv[0]); running python -c from the
execution checkout injects that checkout ahead of the selected snapshot.
531-focused-candidate-correct-imports.log rejects that mixed setup explicitly.
A fresh subprocess with cwd/PYTHONPATH at the raw snapshot removed this cause
and produced the path-asserted passing result. No parser correction occurred.
The API/harness/profile use real script paths outside source trees and the CLI
uses each snapshot's lizard.py, so this cause does not invalidate those results.

## Whole affected concept and consumers

TypeScriptStates composes function, binding, object and declaration owners.
The raw patch flags expression bodies, but its brace decision treats an
alphanumeric first character or _$)]> as an operand. This excludes genuine
word/comparison operators and fails to recognize string/template literal types.
TypeScriptTypeAnnotationStates returns from function-type parentheses and
runtime dispatch sees its =>; the boolean flag then lacks the context needed
to distinguish a typed signature's real body from an arrow's object operand.
Adding punctuation exceptions would retain that split ownership problem.

| Consumer | Observation/disposition |
| --- | --- |
| JavaScriptReader → TypeScriptReader/TypeScriptStates | run: .js API/CLI reproduction and focused JavaScript/ES6 suites |
| TypeScriptReader | run: .ts reports/operators/types plus TypeScript suites |
| TSXReader (.jsx/.tsx) and its separate tokenizer | run: both extensions, literal types and JSX attributes, TSX suite |
| VueReader → TypeScriptReader | run: four real script blocks and Vue suite; included in API timings |
| .cjs/.mjs aliases | reused: inspected unchanged JavaScriptReader.ext routing to the same reader |
| Ruby/C++ and other readers | excluded: neither shared tokenizer nor CodeStateMachine/core is changed |
| NS/other optional metric extensions | no separate performance claim; future corrected-candidate proof follows actual reach |

No production diff was made, so no implementation consumer-trace is substituted
for a correction's required installed trace. The future story owns that trace,
discriminating regression tests and independent coherent refactoring. The two
author tests should move together to a cohesive small module if adopted; the
large original testES6.py can remain byte-identical to target. File size alone
did not cause deferral; the demonstrated brace/type/operator correction did.

## PR533/PR534 dependency claims

Both authored child heads contain PR531 as their parent and reuse its
_expression_body state; their final deltas reference that state without adding
its initial definition. The implementation inheritance claim is real. The
PR533 and PR534 report examples have identical wrong public summaries on target
and raw531, so PR531 does not itself deliver either later outcome. See
dependency-evidence.json for parent/head/delta and public observations.
Later slices own reconciliation of their authored dependence with this deferral.
No canonical blocking supplier agreement follows merely from the shared state;
independent review can proceed without merging this inadequate proposal.

## Fixed paired performance

Manifest SHA256: a224a3afcfa549f1d1d686be5416439d293105941894e8850a700777adb62411.
The 100 original source files are copied byte-for-byte from baseline, plus
64 related affected inputs, all frozen before timing. README.md gives replay.
The literal shared harness warms both sides and alternates order for 20 pairs.
API batches use 99 repetitions; CLI batches use 8 complete invocations. Every
observed batch exceeds one second. The default options are identical.

Host Territory.local: arm64, 16 CPUs, 48 GiB, 49% memory free. Before launch,
load was 13.59/17.17/14.02; unrelated worktree loads were unknown. The coordinator
confirmed no competing owned Nix/heavy work. One serial API+CLI run was chosen;
no repeated run was made to obtain a pass. host-and-proof-decision.json retains
the decision, interpreter and reconsideration condition.

| Workload | Baseline/candidate median s | Baseline/candidate SD s | Median change | Paired geometric change | Paired 95% interval | Gate |
| --- | --- | --- | --- | --- | --- | --- |
| 64 affected API inputs | 1.208374 / 1.245806 | .019195 / .032467 | +3.0978% | +3.4041% | [+2.0154%, +4.8117%] | unproved; upper bound fails |
| 100 fixed source inputs via CLI | 1.299542 / 1.298826 | .019030 / .029802 | -.0551% | +.5236% | [-.2284%, +1.2813%] | pass |

Raw pairs, full commands, interpreter and variability are in 531-api.json and
531-cli.json. The API interval straddles 3%; it neither proves an acceptable
slowdown nor establishes that the true slowdown exceeds 3%. This raw invalid
candidate remains deferred without further timing attempts.

```sh
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-target --candidate /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-readonly --workload api --pairs 20 --seconds 1.2 --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/531-api.json
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-target --candidate /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-readonly --workload cli --pairs 20 --seconds 1.2 --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/531-cli.json
```

comparison-basis.json compares all 100 production source files. Target versus
the cumulative baseline differs only in PR527's NS methods. NS is disabled;
the API harness imports its module, but the FunctionInfo initializer wrapper
is unchanged and the changed methods are not called. Default CLI paths do not
load NS. Thus this comparison owns both incremental and cumulative default
analysis cost; it makes no ENS claim. PR528 was deferred and is absent.

The bounded profile runs 5,000 analyses of a working block-bodied arrow with
identical complete public tuples on both sides. Raw adds 15,000 calls to
_opens_an_object and 15,000 total calls; profile totals are 1.507970 s versus
1.507137 s. This records additional dispatch work without isolating the whole
API timing difference or replacing the paired gate. The script and both JSON
profiles retain source hash, metrics, counts and selected function timings.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr531/profile-control.py /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-target /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/531-profile-baseline.json
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr531/profile-control.py /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531-readonly /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/531-profile-candidate.json
```

## Retained gaps and next owner

SEED-013 owns coherent correction, added public regressions, installed consumer
trace, corrected-code performance including relevant extension paths and final
full-suite/make/hosted-CI acceptance. A future corrected revision needs fresh
proof; the raw diagnostics do not satisfy that promise. Original parser/code
quality is not repaired by this evidence-only disposition.

Identity evidence before allocation read both execution HEAD16eed and the clean
trunk-evidence checkout at7ac. All sources read, no gaps; execution highest012,
trunk highest011; candidate013 was unlisted without history seed/record. The
new queued story follows existing regex012 under the unchanged maintenance
direction; no Taken entry or execution plan was created. The coordinator owns
review acceptance, story obligations, independent refactoring and delivery.
