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

## Execution state and accepted proof

- Identity: `SEED-016#arrow-parameter-bindings`; publisher
  `dashboard-territory.local-lizard`; assigned agent `Sola-chan`.
- Execution and originating checkout:
  `/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro`;
  branch `codex/report-consistent-javascript-and-typescript-arro`, Story Branch
  Mode. This is the established owned linked worktree; no integration checkout
  is selected for mutation. Authorized remote/trunk: `origin` / `master`.
- Starting revision: `65c1a10c5a2509188957af6abbc16353442a2dfc`; accepted claim,
  previously published branch base and initial candidate:
  `7f479101728b5fec8becaf7c784b752ed736719e`.
- Checkout setup: checked-in `shell.nix` creates the local venv and installs
  `dev_requirements.txt`; `nix develop -c python -m pytest
  test/test_languages/testJavaScriptParameterNames.py` passed 34 tests here.
  Nix commands remain serial. No selective formatter or commit hook is installed;
  `make` owns the required style checks. Workspace author is `Sola-chan`.
- CI: GitHub Actions `python-app.yml`, verified active ID `38306292`, display
  name `Python application`; push trigger covers the execution branch.
  Codex observer coordinator `sola-seed016`, directory
  `/tmp/dough-ci-501/watch-TEDK9c`, PID `99850`, PTY session `74466`, yielded
  continuation cell `32`. Request root/repository/branch and owner hash were
  checked against this checkout and coordinator after recovering the header.
  It covers ordinary branch increments; the initial trunk claim is unobserved.
- No numeric slice hard limit is configured. Replanning retains the plan's
  existing authority within this story; product promises remain unchanged.
- Source-only acceptance review: fresh non-inheriting `source_commitments`
  agent received the source Goal, Scope, examples, representation, completion
  and explicit execution amendment, no plan/product/return or project-file
  access. Its launch and 72 returned commitments are retained in the execution
  conversation. Compared with the actual source: counts/names, all four-arrow,
  alias, empty-pattern and typed examples, reader/consumer coverage, controls,
  representation/ownership, performance, checks and exclusions remain intact.
  They are future obligations of slices 2–5, not results of this route probe.
- Slice 1 expected route: the intended open contribution can receive a qualified
  correction without losing contributor history. Actual observation: unchanged
  open head, maintainer editing enabled, matching fork ref and authenticated
  receive-pack dry-run exit 0. Commands, observed refs/checks/discussion and
  preview limitations: [contributor-route.md](../../../docs/reviews/fabio-malpezzi-20261027/pr536/contributor-route.md).
  This accepts feasibility only; changed-candidate push acceptance and final
  head/CI/merge/acknowledgement remain slice 5's obligations. No product tests
  are claimed for this evidence-only slice.

## Existing owners and architectural direction

PFE: change the existing anonymous-arrow path in
`lizard_languages/javascript_bindings.py` (`JavaScriptBindingsMixin` pending
binding summaries and `_push_arrow_function`) and
`lizard_languages/typescript_states.py` / `javascript_functions.py`
(`_dec` parameter extraction, `_arrow_function`, binding-candidate
parentheses), together with `lizard_core/function_info.py` (`FunctionInfo`
`parameters` / `parameter_count` / `full_parameters`). TypeScriptReader
composes those owners; JavaScript, JSX/TSX and Vue script reuse them;
`.cjs`/`.mjs` alias the same reader. Do not add a parallel pending-name
collector; keep the public facade.

Rejected shapes from the review: raw/adapted #536 keep a second
`_read_name` collector beside `_dec`, so anonymous aliases expose property
keys instead of the named convention; empty patterns under-count; typed
callback annotations emit phantoms; and paired affected API / names-counts
gates fail. Prefer one coherent rule: when a binding candidate or bare
identifier becomes an arrow, formal slots and binding names enter
`FunctionInfo` through the same representation named signatures already use.

Settled public representation (from the refined story): `parameter_count` is
formal slot count; `parameters` lists established binding names only; empty
patterns add a slot and no name, so count may exceed `len(parameters)`;
trailing-comma empty tokens stay excluded.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for parameter count / language reader / function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no North Star topic or
ADR is warranted. No architectural exception is proposed.

SEED-014 and SEED-015 are Taken and change related expression-body state. At
Take, refresh trunk and rerun the premise observations below on the execution
checkout; reconcile with whichever landed rather than reintroducing their
deferred outcomes. No formal blocking dependency is asserted.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro`,
revision `0ef95abbe278bfd22bbe4de48b9fa4b71f7b2181`, observed 2026-10-27.

- Through `analyze_file.analyze_source_code`, the four reported anonymous
  arrows expose counts `0,0,0,0`. Named `({key: renamed}, tail) =>` reports
  `['renamed','tail']` count 2 while the anonymous twin reports `[]` count 0.
  Named `({}, [], tail) =>` reports count 1 / `['tail']` with
  `full_parameters` `[',', ' tail']`; the anonymous empty form reports
  count 0. Typed callback input on this revision reports one anonymous
  count 0 (review target `d7be8b9` had a phantom zero-parameter sibling as
  well); correction must still yield one function count 4 with names
  `['number','$','cb','tail']` and no phantom. Member assignment remains
  count 0; wrapper-call anonymous also count 0 on target.
- Inspected `JavaScriptBindingsMixin._push_arrow_function` (transfers only
  CCN/defaults from pending summaries, not names) and
  `JavaScriptFunctionsMixin._dec` (named signatures call
  `context.parameter`). Confirms the owners named above. Adapted #536's
  parallel `names` / `_read_name` collector is the rejected shape.
- `FunctionInfo.parameter_count` is `len(self.parameters)`, and `parameters`
  drops tokens that yield no identifier match — so empty formal slots cannot
  be represented without extending that shared model.
- `gh pr view 536 --repo terryyin/lizard` returned `OPEN`, head
  `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f` on FabioMalpezzi's fork, base
  `master`, `maintainerCanModify: true`. Readable metadata is not accepted
  publication; slice 1's early probe owns permission and route.
- Frozen evidence tree exists under
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536` and baseline under
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline`. Cache
  `paired.py` SHA256 is
  `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`; the
  review record's central `paired.py` hash is
  `ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`. Before
  timing, confirm the harness bytes used match the review's intended
  measurement script (restore or pin if the cache copy has drifted). This
  settles replay reachability, not any candidate's timing.

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
Status: planned
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
Status: planned
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
Status: planned
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, affected API (frozen 65
fixed inputs including all relevant corrected cases, hashed before timing) /
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
Status: planned
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

## Plan review

Slice-plan refinement was not needed: slices separate a permitted early
external probe, unified anonymous capture through existing owners, the
slot/phantom extension with its own regression loop, the independent
performance loop and hosted delivery. The cumulative design keeps one
FunctionInfo parameter representation and rejects the parallel collector.
No remaining slice-specific concern was found; contributor publication is
bounded by the early probe. No human decision is currently pending.
