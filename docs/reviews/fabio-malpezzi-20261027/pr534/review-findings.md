# PR #534 disposition: defer the ternary-arrow correction

The report is real and useful: current target detects one anonymous arrow in
`choose`, with CCN 2, where the two branches require CCN 1 and 2. Raw #534
provides that improvement in JS/TS/JSX/TSX and Vue, but introduces a nested
internal-colon regression and leaves typed branch separators unresolved.
The strict timing gate is also unproved. Estimated coherent correction and
cleanup: **30–45 minutes**, excluding review/measurement. No product correction
was attempted. Follow-up:
[SEED-015#ternary-arrow-boundaries](../../../../.planning/seeds/SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries).

## Source and current target

The complete PR description/metadata is in `review-input.json`, and the complete
base-to-head diff is `proposed-patch.json`: two commits, three files, 71 additions,
one deletion. Its discussion is empty. Frozen/refreshed head remains
`f4074ae860c7999cc37dd081a6f7ebfa777adc83`, open and unmerged.
Historical run 40418655900 passed four jobs; that does not prove the current
fourteen-job matrix for a corrected candidate. No hosted action was performed.

Target snapshot is `7186fdbe`; fetched trunk remains `7ac449f`. Their analyzer
sources are byte-identical. Relative to initial `2e21e404`, only NS-extension
methods differ. Default analysis does not select NS. The API harness imports its
class only during worker setup, outside timing; no NS initialization is used.
Prior #527 performance remains with its own accepted evidence; no ENS claim is
made here. ADR-0000 and ADR-0001 remain Accepted, with no conflict or exception:
retain the existing parser owners and public metric meanings.

## Decisive public observations

Expected rows are explicit in `inputs.json` and `supplemental-inputs.json`;
every observation retains name, start/end line, CCN, NLOC and token count.
`reproduce.py` checks public analyzer and independent FileAnalyzer agreement.

| Input | Expected | Target | Raw and independent child |
| --- | --- | --- | --- |
| `reported.js` | Two anonymous arrows at 2–2, CCN 1/2; choose 1–4, CCN 2 | One anonymous CCN-2 arrow | Required two arrows; choose preserved |
| `internal_ternary.js` | f 2–2, CCN 2; outer CCN 1 | Correct | Correct |
| `nested_ternary.js` | Three branch arrows, CCN 1/2/2; outer CCN 3 | One combined arrow | Correct |
| `grouped_nested_arrow.js` | Grouped inner anonymous CCN 2, first branch f CCN 1, alternate anonymous CCN 2 | Alternate absorbed | Correct |
| `typed_arrows.ts` | Two anonymous arrows at 2–2, CCN 1/2; outer 1–4, CCN 2; after 5–5 | One CCN-2 arrow | Same unresolved loss |
| `typed_object_return.ts` | Same two branch arrows and following functions | One CCN-2 arrow | Same unresolved loss |
| Supplemental `curried_internal_control.js` | f 2–2, CCN 3; outer 1–4, CCN 1; after 5–5, CCN 1 | Correct | f CCN 2 plus spurious anonymous CCN 2 |

The supplemental internal-colon regression occurs in all four JS/TS/JSX/TSX
variants. Its source is `x => d ? y => y : z => z || x`: the inner `=>` does
not create a second function under the existing curried-arrow contract, yet
raw `_arrow_function` overwrites `_ternaries_before_body` with depth 1. The
internal colon then closes f and moves `||` into an extra reported arrow.
The frozen `nested_arrow_internal` case shows the same failure inside an
enclosing ternary. Preserve the explicit existing outer-only curried contract;
this review requests no new nested-function detection feature.

In typed branches the first return annotation colon reaches `_state_global`
while a ternary is pending but before the arrow starts. It decrements runtime
ternary depth instead of consuming a genuine annotation. The later actual
branch colon then enters type consumption and the alternate is absorbed.
Changing one unconditional colon check does not resolve both ownership paths.

Object-property and computed-key cases retain a known limitation: sibling arrows
after a comma are absorbed on both target and proposal. `object_property` gains
the separated first branch, but still reports the alternate with CCN 3 and no
`next`; computed-key output is unchanged, with no `next`. These are existing
limitations, not newly introduced #534 regressions. Keep them visible when
selecting a coherent correction; they do not justify a new blocking supplier.
Block bodies, plain ternaries, type assertions and following functions are
covered controls; no unrelated parameter or newline fix is claimed.

## Dependency, quality and consumers

The raw head inherits #531's `_expression_body` and `_opens_an_object` heuristic.
It retains the five previously evidenced literal function-type return failures:
TS/TSX string and template variants and the Vue string case. `filter` runs past
its body and arrows can be named `return`. The same high-level snapshots here
reproduce these failures; the prior review explains their already-known cause.

`child` removes only the `_opens_an_object` method and its `{` dispatch branch.
`independent-child-patch.json` is the complete production delta from target.
It retains the reported #534 improvement and passes the new authored test while
restoring all five literal-return cases. It still fails the nested internal
colon and typed examples. This proves independent work can start without #531;
no SEED-013 dependency agreement is asserted. The inherited object improvement
is intentionally absent from this child: its focused suite has exactly that
one authored #531 failure, retained rather than dismissed.

The affected concept stays in JavaScriptFunctionsMixin, TypeScriptStates,
JavaScriptBindingsMixin, JavaScriptObjectsMixin and TypeScriptDeclarationsMixin.
The scalar expression-body/snapshot state needs coherent lifecycle handling;
do not add a parallel parser or metric counter. Two production modules are below
250 lines, but the author's appended testES6.py is larger; a corrected change's
new tests should move to a cohesive small module without changing unrelated tests.

| Consumer boundary | Disposition and observation |
| --- | --- |
| JS/TS shared runtime dispatch | Run: all 92 cases through analyze_file and FileAnalyzer; explicit expected/actual rows |
| JSX/TSX tokenizer and JSX attributes | Run: `.jsx`/`.tsx`, typed returns, grouped arrows and actual Panel attribute cases |
| Vue script reader inheriting TypeScriptReader | Run: six Vue controls including reported, typed and inherited literal cases |
| Existing JS/TS/ES6/TSX/Vue suites | Run in isolated snapshots: target 271 passed/5 skipped; raw 274 passed/5 skipped; child 273 passed/5 skipped/1 inherited object failure |
| Public CLI output | Run: reported, typed, literal-return and supplemental curried sources on all three snapshots; logs retain displayed names/spans/CCN |
| FileAnalyzer and facade integration | Run: matching public metric lists with facade/registry/mixin/state/test-helper import-path assertions |
| Python NS | Reused: prior accepted #527 proof; unused by default path here, with static/dynamic equivalence below; ENS unmeasured |
| Ruby/C++ shared tokenization | Excluded: no token generator, regex or shared comment change in this proposal |

No production change was made in this slice, so there is no accepted correction's
consumer-trace/refactor proof. The follow-up owns that installed trace and final
red/green regression gate. Existing suites passing the raw proposal do not
observe the newly found internal-colon failure; explicit reproducers do.

## Repeated performance and bounded diagnosis

The immutable corpus was frozen before measurement: 92 affected input files and
100 fixed initial source files, no PR533 workload. Hashes are in `manifest.json`.
The unchanged central harness hash is
`3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
Both comparisons use Python 3.9.11 from the same prepared Nix environment,
default extensions/options, warmups, calibrated 1.5-second batches and 20
alternating pairs. All actual paired durations exceed one second. The host
decision is retained separately: 16 CPUs/48 GiB, load 7.33/6.49/6.91,
sequential owned work, other worktree contention unknown.

| Workload | Baseline/candidate median seconds | SD seconds | Median slowdown | Paired geometric slowdown | Paired 95% interval |
| --- | --- | --- | --- | --- | --- |
| API | 1.488780 / 1.519058 | .018110 / .034054 | +2.0337% | +2.3906% | [1.5167%, 3.2720%] |
| Broad CLI | 1.701121 / 1.722747 | .152478 / .112059 | +1.2713% | −0.3131% | [−4.7317%, 4.3104%] |

Both strict below-3% gates are **unproved**, not accepted and not labeled a
proven ≥3% slowdown. Invalid raw code was measured diagnostically once per
workload; no repeat was made to obtain a favorable interval. The future correct
candidate owns sufficient precision, incremental/cumulative gates and current CI.

These default comparisons cover incremental and cumulative effects: static diff
finds only unused NS methods between initial and current analyzer; all 92 actual
API outputs match, and full CLI output bytes match between initial/current/raw.
`comparison-basis.json` retains the output digest. Current trunk's metadata-only
changes do not change these analyzer paths. API timing includes invalid raw
behavior; it is not evidence of a valid correction's performance.

One 5000-iteration profile per snapshot asserts the same six public metrics on
`block_branches.js` before profiling. Totals are 1.8704 s/9,560,001 calls versus
1.9042 s/9,580,001 calls. Raw adds 20,000 object-helper calls (0.0020 s own time),
and state/arrow setup has extra assignments. This is a bounded indication of
overhead on unchanged output, not a causal allocation of the full API interval
or an explanation of CLI variance. Noise and workload-wide causes remain open.

## Proof limits and next ownership

`README.md` retains literal commands and setup/assertion locations. All owned
commands reached terminal results; no owned watcher was created. The supplied
coordinator observer remains untouched. Full suite/make and hosted corrected-head
CI/merge/thanks belong to the coordinator if a satisfactory candidate is later
selected; this deferral makes no final-candidate claim. Initial harness failures,
their diagnosis and corrected observations are retained explicitly.

The queued story owns correction and its proof. This review's durable result is
the real benefit, independently demonstrated non-prerequisite, reproducible new
regression/unresolved typed behavior, two unproved timing gates and linked follow-up.
