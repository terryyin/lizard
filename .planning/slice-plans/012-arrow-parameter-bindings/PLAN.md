# Report consistent JavaScript and TypeScript arrow parameters

**Identity:** SEED-016#arrow-parameter-bindings
**Source:** [Refined story](../../seeds/SEED-016-arrow-parameter-bindings.md#arrow-parameter-bindings)

## Outcome and boundaries

Users analyzing JavaScript, TypeScript, JSX/TSX and Vue script content get
consistent formal parameter counts and established binding names for named and
anonymous arrows. Empty binding patterns occupy formal slots without inventing
public names; callback-type annotations create no phantom functions; type and
initializer expressions keep their own meaning and CCN. CLI, formatters,
warnings, duplicate-parameter lists and the website `parameter_count` consumer
see the same public values.

The source's key examples and
[PR536 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr536/README.md)
are the acceptance input. The frozen
[reproduce.py](../../../docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py)
checks names and counts for the promised rows of `inputs.json` and the
accepted PR532/535 controls in `all-inputs.json`.

Excluded commitments: member-assignment parameter loss
(`module.exports.add = (a, b) => …` remaining 0; later #537); abandoned
expression enclosing CCN (`arrow-abandoned.js` outer CCN 3; later #539);
unrelated boundary repairs; parser redesign; ENS-only performance; a full
JS/TS grammar. The wrapper-call half of `arrow-member.js`
(`api.wrap((x, y) => …)` → count 2) is promised. No sibling is a formal
blocking supplier. This is the technical follow-up after a review that
exceeded its 15-minute correction cutoff; that cutoff is not a time limit for
this plan. No project-specific numeric slice limit was supplied.

Execution authorized by Terry's `dough-execute-plan` invocation on 2026-10-27.
The established start owns the published Take; no second start was made.

## Execution context and existing owners

Detailed setup, observer identity, accepted proof, preparation premises and PFE
rationale are retained in [slice2-execution-context.md](../../../docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md).

Execution: established owned checkout
`/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro`,
branch `codex/report-consistent-javascript-and-typescript-arro`, Story Branch
Mode, publisher `dashboard-territory.local-lizard`, agent `Sola-chan`.
Authorized remote/trunk: `origin` / `master`. Slice 1 accepted branch base:
`33610866db00d28ba9e2dafe5a3fd1942347fb2f`; claim/suffix base:
`7f479101728b5fec8becaf7c784b752ed736719e`. Slice 2 semantic proof is accepted;
its independent refactor preserved product/tests and reorganized evidence only
([refactor proof](../../../docs/reviews/fabio-malpezzi-20261027/pr536/slice2-refactor-consumers.md)).
Slice 2 was accepted at `13c3403fc918209d1279018fd8cb5dad0405f144`, suffix
base `33610866db00d28ba9e2dafe5a3fd1942347fb2f`. Slice 3 source-only reviewer
`slice3_source_review` read only the original seed; comparison accepts the empty,
typed/return/default and consumer journeys in [slice3 proof](../../../docs/reviews/fabio-malpezzi-20261027/pr536/slice3-proof.md):
1944 tests passed, 7 skipped; frozen 63 promised/control rows and actual CLI pass.
Current observer: `sola-seed016`, `/tmp/dough-ci-501/watch-Tz93Df`, PID73990, PTY63716, yielded cell113; old TEDK9c finished unavailable and shut down. Before slice5 record delivery, branch head `1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94` is accepted on origin (suffix `3ba2eb69b228b5103570122c4660b7666d1b797e`); hosted merge `1bc7cbd` and acknowledgement6447350182 are confirmed. The original launch landing comparison is recorded; later trunk publications take no landing context.

Keep parameter extraction in the existing JavaScriptBindings / JavaScriptFunctions
and TypeScriptStates owners, with shared FunctionInfo representation; no parallel
pending-name collector. Formal slots and established binding names are distinct.
Follow Accepted, unsuperseded ADR-0000/0001 linked in the context. No architectural
exception is proposed. No numeric slice limit is configured; preserve this scope.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Anonymous and named arrows share formal counts and binding names | Slice 2: red→green public regressions for reported/authored/named/defaults/binding/alias/pattern/sigils/calls/jsx/tsx/control rows; accepted 532/535 controls stay green |
| Empty slots count; annotation phantoms absent; typed/generic/return/default CCN hold | Slice 3: red→green empty-binding, typed, generic, return-object/function, nested-default, ternary-default; no phantom functions; named empty slots coherent |
| Controls and shared consumers | Slices 2–3: focused consumer suites (CLI, formatters, warnings, duplicates, website parameter_count path); retained reproducer showing only deferred rows failing |
| Strict affected and broad throughput | Slice 4: warmed paired measurements, each upper 95% slowdown bound strictly below 3% versus current target and frozen review baseline |
| Final corrected revision accepted and acknowledged | Slice 5: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Use `lizard-language-support` for reader/test changes. Public `analyze_file` /
`analyze_source_code` results own semantic proof; mock only filesystem
boundaries. Inspect every reached consumer and record run/reused/excluded with
reasons. Independent post-change refactoring runs through the execution
workflow before delivery; keep new tests in a cohesive small module below 250
lines while leaving unrelated large test files byte-identical. Eliminate
unnecessary collector bookkeeping rather than layering fast paths on the
rejected collector.

Focused command after relevant edits:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testJavaScriptParameterNames.py
```

Also rerun the retained reproducer against the candidate checkout (promised
`inputs.json` rows and `all-inputs.json` controls). `AGENTS.md` requires full
`nix develop -c python -m pytest` before completion; the source contract
additionally requires `nix develop -c make` and final hosted CI. Run Nix
commands sequentially because the shell hook changes `.venv`; keep owned heavy
measurement serial. Refresh current CI applicability in execution.

## Ordered slices

### 1. Establish the contributor correction route
Type: Behavior
Status: done
Proof: refreshed PR/head/base, contributor-edit permission and authenticated
publication-route observations; preserve exact refusal if unavailable.

Behavior: given an explicit execution instruction, refresh PR536's patch,
discussion, checks and current target, verify the head still names the
intended contribution, and establish the authorized contributor-head delivery
route (non-mutating authenticated push preview where available) before
dependent implementation. A preview is not accepted publication; slice 5 owns
the real update. If the PR is closed, superseded, changed incompatibly or
cannot receive corrections, stop and return the disposition/destination
decision to Terry. Do not substitute an unrelated trunk patch.

Boundedness: one external feasibility observation; product unchanged. Failure
stops slices 2–5 and revises the plan before edits.

### 2. Capture anonymous arrow formals through existing parameter owners
Type: Behavior
Status: done
Proof: red→green public regressions for `arrow-reported`, `arrow-authored*`,
`arrow-named`, `arrow-defaults`, `arrow-binding-*`, `arrow-alias-*`,
nested/quoted/computed bindings, `arrow-sigils*`, `arrow-calls`,
`arrow-jsx`/`arrow-tsx`, `arrow-control`, and the wrapper-call half of
`arrow-member`; accepted PR532/535 controls stay green; no parallel name
collector remains in the candidate.

Behavior: given a parenthesized binding group or bare identifier that becomes
an arrow (`=>`), analysis records the same formal counts and established
binding names as the equivalent named arrow, including destructuring aliases
(`renamed` not `key`), compound/nested/quoted/computed patterns, rest/defaults
and JSX/TSX/Vue consumers. Implement by routing pending binding candidates
into the existing `FunctionInfo` parameter path used by `_dec`, not by adding
an independent `_read_name` collector. Preserve the public facade and accepted
identifier/receiver behavior.

Boundedness: one ownership rule and one semantic regression loop across the
shared readers; splitting by extension would leave divergent rules. A correct
local result is a safe stopping point, unmerged. Empty-slot count and typed
phantoms remain slice 3.

### 3. Count empty formal slots and skip annotation phantoms
Type: Behavior
Status: done
Proof: red→green `arrow-empty-binding*`, `arrow-typed`, `arrow-generic*`,
`arrow-return-*`, `arrow-nested-default`, `arrow-ternary-default`; named empty
patterns also report count 3; typed case is one function with count 4 and names
`['number','$','cb','tail']`; initializer/default CCN and following functions
hold; the reproducer then fails only deferred rows (`arrow-member` member
half, `arrow-abandoned` CCN).

Behavior: given empty object/array binding patterns among formals, each pattern
occupies a formal slot in `parameter_count` without inventing a public name in
`parameters` (count may exceed `len(parameters)`; trailing-comma empties stay
excluded). Callback-type annotations inside a signature do not create phantom
functions. Extend the shared `FunctionInfo` slot representation and the
annotation-consumption path used by slice 2 rather than adding a parser-local
parallel count. Preserve generics, typed returns, nested default CCN and the
ternary-default CCN improvement already useful in the proposal.

Boundedness: one slot-vs-name representation decision plus annotation
ownership, with its own regression loop on top of slice 2's capture path. Safe
stopping point, unmerged.

### 4. Meet the strict performance budget without weakening semantics
Type: Behavior
Status: done
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, affected API (all 65 frozen
inputs plus relevant corrected-case supplements, hashed before timing) /
names-counts and broader CLI workloads each show an upper paired 95% slowdown
bound strictly below 3% versus current target (incremental) and the frozen
review baseline (cumulative). A broad pass cannot hide an affected failure.
Prior raw/adapted intervals remain diagnosis only; do not repeatedly time the
already-invalid proposal.

Confirm harness identity against the review's intended `paired.py` hash before
timing. Run warmed alternating pairs with batches over one second through Nix;
record load, durations, medians, variability and intervals. Confirm imported
paths and public metric tuples before timing. Profile only when needed and
remove redundant collector bookkeeping rather than adding fast paths; re-run
slices 2–3 proof after optimization. Noise or an upper bound of 3% or more is
failing; fix a prospective quiet retry protocol rather than repeating for a
pass.

### 5. Deliver the qualified correction through PR536
Type: Behavior
Status: done
Proof: final contributor-head SHA and accepted push, green current required CI,
exact-head guarded merge receipt, and one acknowledgement URL.

Behavior: given the corrected/refactored candidate with semantic and
performance proof, publish corrections to the contributor branch without force
or lost history, run full pytest and make on the final revision, verify final
head/base/tree and all required checks, then perform an exact-head guarded
hosted merge. Follow the
[complete-cycle contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md);
its 15-minute triage rule does not re-defer this selected correction. Hosted
merge is not a Dough Land operation. After confirmed merge, with explicit
acknowledgement authority, post one thank-you noting material corrections
(check for a prior receipt). Retain evidence in the review record, then wrap up.

Boundedness: one qualified contribution reaching users and author, with
separate hosted wait time. Pending CI, merge or acknowledgement leaves this
slice unfinished.

## Execution complete

Product advice: no change; existing structural and lexical reviews retain the excluded cases. Outcome and whole-suite review found no correction to plan; process findings extend DD-001 and DD-003.

## Story obligations

### G1. Empty formal slots
Reported: slice 2 — "Named and anonymous ({}, [], tail) report count 1 rather than 3, with names ['tail']."
Story clause: "empty binding patterns still occupy formal slots without inventing public names"
Disposition: proved by slice 3: slice3-proof.md — test_named_and_anonymous_empty_patterns_occupy_slots and test_receiver_and_trailing_commas_do_not_invent_slots; actual count 3/names tail across readers, receiver and trailing empties excluded.

### G2. Function-return annotation continuation
Reported: slice 2 — "arrow-return-function.ts exposes only inner n; the outer anonymous arrow with x,y and count 2 is missing."
Story clause: "Generic parameters, typed returns, rest/defaults, nested default CCN, ternary-default CCN ownership and following functions stay correct."
Disposition: proved by slice 3: slice3-proof.md — test_function_return_annotation_keeps_outer_arrow and typed/generic/default regressions; inner n, outer x/y and following function present, callback annotations create no phantoms.

### G3. Empty-slot output consumers
Reported: slice 2 — "CSV/checkstyle derive counts from len(parameters), and HTML from len(full_parameters); they must follow formal counts when empty slots are represented, while duplicate lists retain identifier-only names."
Story clause: "across CLI, formatters, warnings, duplicate-parameter lists and the current website JSON/`parameter_count` consumer path."
Disposition: proved by slice 3: slice3-proof.md — actual CSV/checkstyle/HTML, CLI warning and Flask JSON count 3; test_empty_slots_do_not_add_duplicate_parameter_keys observes count 7 and sole identifier key a,b,c,d,e; unchanged frontend directly renders fn[field].

### G4. Deferred member assignment
Reported: slice 2 — "arrow-member.js member-assignment count remains 0; its promised wrapper callback now has count 2."
Story clause: "member-assignment parameter loss"
Disposition: excluded "member-assignment parameter loss (`module.exports.add = (a, b) => …` remaining count 0) stays with later review slice #537"

### G5. Deferred enclosing CCN
Reported: slice 2 — "arrow-abandoned.js outer CCN remains 2 instead of 3."
Story clause: "Abandoned-expression enclosing CCN"
Disposition: excluded "Abandoned-expression enclosing CCN (`arrow-abandoned.js` expecting outer CCN 3) stays with #539."

### G6. Strict performance proof remains outstanding
Reported: slice 2 — "No performance experiment has been launched; affected and broad incremental/cumulative bounds remain unproved."
Story clause: "paired 95% upper slowdown bounds strictly below 3% for incremental and cumulative affected and broader workloads."
Disposition: proved by slice 4: slice4-proof.md and corrected-proof/summary.json — all eight warmed paired API/names-counts/default CLI/ENS incremental and cumulative upper bounds are below 3% (maximum 1.51932%); all batches exceed 1s, original 65 plus 36 corrected-case supplements and frozen 100 broad sources are hashed, and public semantics match accepted candidate0.

### G7. Final qualification and delivery remain outstanding
Reported: slice 2 — "Full pytest, make and final hosted CI on the corrected revision remain unrun; PR536 remains unchanged and unmerged."
Story clause: "Run the full pytest suite, `make` and final hosted CI before any exact-head guarded merge."
Disposition: proved by slice 5: slice5-proof.md — exact head 1b8b56e/tree 1317dac0 passed full pytest (1944/7 skipped), make and both applicable 14-build hosted matrices plus change detection; web=false skips justified. Non-force contributor update preserved both histories, guarded merge 1bc7cbd retained the qualified tree, and verified single acknowledgement is issuecomment-6447350182.
