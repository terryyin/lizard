# Preserve arrow boundaries across ternary and type colons

**Identity:** SEED-015#ternary-arrow-boundaries
**Source:** [Refined story](../../seeds/SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries)

## Outcome and boundaries

Users analyzing JavaScript, TypeScript, JSX/TSX and Vue script content get one
reported function per expression-bodied ternary branch arrow, each with its
own CCN, while colons inside an arrow's own body, genuine TypeScript
annotations and property/computed-key colons keep their own syntax. The
enclosing and following functions keep correct names, spans and CCN, and the
existing outer-only curried-arrow contract is preserved.

The source's key examples and
[PR534 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md)
are the acceptance input. The frozen
[reproduce.py](../../../docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py)
checks names, start/end lines and CCN for the promised rows of
`inputs.json` and all of `supplemental-inputs.json`.

Excluded commitments: the existing object-comma sibling absorption (the
`next` rows of `object_property`/`computed_key`), object-literal ternary
bodies (`inherited_*`, `object_inside_ternary`; SEED-013), multiline
continuations (SEED-014), arrow parameter bindings (SEED-016), naming
changes, nested-arrow detection, ENS-only performance, a full JS/TS grammar.
No sibling is a formal blocking supplier. This is the technical follow-up after
a review that exceeded its 15-minute correction cutoff; that cutoff is not a
time limit for this plan. No project-specific numeric slice limit was supplied.

Execution authorized by the selected `dough-execute-plan` instruction on
2026-10-27; the established start owns this work.

## Execution context

- Identity: `SEED-015#ternary-arrow-boundaries`; publisher
  `dashboard-territory.local-lizard`; agent Akiho-chan.
- Mode: story-branch; execution/originating checkout
  `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ`;
  branch `codex/preserve-arrow-boundaries-across-ternary-and-typ`.
  Existing linked worktree, reused through the supplied established start.
- Remote `origin`, trunk `master`; ordinary increments target the execution
  branch. Starting revision `a2d2ab778d03cbb9ea96d243ab7533105d304eb2`;
  claim/published base `6f506711595e0ad832ab5a75f5a4750a2b362415`, confirmed
  on both remote refs at startup. Claim trunk CI is unobserved.
- Default checkout `/Users/terryyin/git/lizard` is not an execution location.
- Setup: project Nix shell prepared this exact checkout; public JS/TS suites
  passed (43 tests). Tooling uses sequential `PIP_QUIET=1 nix develop -c`.
  No configured formatter, generator or commit hook applies to route evidence;
  commits use the assigned workspace agent helper. No new replanning restriction.
- Observer: Codex coordinator `akiho-seed015-a5bbe368`, cell `14`, session
  `75355`, directory `/tmp/dough-ci-501/watch-UAZZGU`, PID `85484`.
  Recovered request/owner hash match the exact checkout, repository
  `terryyin/lizard`, execution mode and execution branch. Selected GitHub
  workflow `python-app.yml` (ID `38306292`, name `Python application`) is
  active and push-triggered for all branches. Retain this observer for delivery.
- Slice 1 route proof: [correction-route.md](../../../docs/reviews/fabio-malpezzi-20261027/pr534/correction-route.md).
  Contributor head unchanged/open and authenticated dry-run route available.
  Fresh target `0ef95abbe278bfd22bbe4de48b9fa4b71f7b2181` differs from claim
  only by an agent profile; analyzer sources are unchanged.

## Existing owners and architectural direction

PFE: change the existing ternary and expression-body colon ownership in
`lizard_languages/typescript_states.py` (`TypeScriptStates._state_global`,
whose scalar `_ternary_depth` today consumes any branch colon without ending a
started arrow) and `lizard_languages/javascript_functions.py`
(`JavaScriptFunctionsMixin._dec` / `_expecting_func_opening_bracket`, which
consume parameter and return-type annotation colons), together with the
binding, object and declaration mixins they compose. TypeScriptReader composes
those owners; JavaScript, JSX/TSX and Vue script reuse them; `.cjs`/`.mjs`
alias the same reader. Do not add a parallel parser, metric counter or
PR-specific colon switch; keep the public facade.

Rejected shapes from the review: raw #534 overwrites the enclosing arrow's
ternary snapshot when a nested arrow creates no function (internal-colon
regression), and changing one unconditional colon check cannot fix both the
runtime-separator and the typed-annotation paths. Prefer one coherent rule:
an arrow started inside a ternary records the depth it belongs to; only that
depth's colon ends it; inner `?`/`:` pairs and genuine annotations are
consumed by their own owners first.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for CCN, language reader and function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no North Star topic or
ADR is warranted. No architectural exception is proposed.

SEED-013 and SEED-014 are Taken and change the same expression-body state.
At Take, refresh trunk and rerun the premise observations below on the
execution checkout; reconcile with whichever landed rather than reintroducing
their deferred outcomes.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ`,
revision `1db5933b66e4602f607c6ae40998984dadd5bab5`, observed 2026-10-27.

- Ran `PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout <checkout>`.
  92 cases; failures include every promised row: `reported.*`, `authored.*`,
  `nested_ternary.*`, `nested_arrows.*`, `nested_arrow_internal.*`,
  `grouped_nested_arrow.*`, `branch_object.*`, `typed_arrows.ts/tsx`,
  `type_assertion.ts/tsx`, `typed_object_return.ts/tsx`, `jsx_attribute.*`
  and `vue_reported`/`vue_nested_ternary`/`vue_nested_arrows`/
  `vue_branch_object`/`vue_typed_arrows`. `internal_ternary`,
  `block_branches`, `plain_ternary` and the literal function-type controls
  pass (preserve them). Deferred rows (`object_property`, `computed_key`,
  `object_inside_ternary`, `inherited_*`) also fail and are not promised.
- Ran the same with `--inputs .../supplemental-inputs.json`: 4 cases, no
  failures. The curried internal-colon control is green on target and must stay
  green; it discriminates the raw #534 regression.
- A scratch script through `analyze_file.analyze_source_code` showed
  `{f: x => x, next: z => z && c}` reports only `f` CCN 2: the comma-sibling
  absorption is independent of ternaries, confirming its exclusion.
- Inspected `TypeScriptStates._state_global` lines 64–70 (`?`/`:` depth) and
  `JavaScriptFunctionsMixin._expecting_func_opening_bracket` (return-type `:`).
  Confirms the owners named above.
- Hash-checked `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py`
  as SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`
  and the cache `manifest.json` as
  `113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5`, equal to
  the checked-in manifest. From the cache `corpus` directory, ran
  `printf '1\n' | python ../paired.py worker <checkout> 0 api` through Nix →
  `{"seconds": …, "files": 92}`; and the CLI workload's literal command
  `python lizard.py <corpus>/lizard.py <corpus>/lizard_core <corpus>/lizard_languages <corpus>/lizard_ext`
  from the checkout → exit 1 (expected threshold warning) with full output.
  Frozen baseline tree exists at `/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline`
  (archive of `2e21e404`, not a Git checkout). This settles replay
  reachability, not any candidate's timing. Reconstruct via the PR534 README
  recipe if cache files disappear.
- `gh pr view 534 --repo terryyin/lizard` returned `OPEN`, head
  `f4074ae860c7999cc37dd081a6f7ebfa777adc83` on
  `FabioMalpezzi:pr/js-arrow-function-in-a-ternary-branch`, base `master`,
  `maintainerCanModify: true`. Readable metadata is not accepted publication;
  slice 1's early probe owns permission and route.

## Proof ownership and gates

Slice 1 acceptance: a fresh no-inheritance source-only reviewer was launched as
`/root/source_review` with the story's Goal, all key examples, Scope and
exclusions, completion and original delivery contract text; it read no files.
Its launch/input and complete returned commitments are retained in this
execution conversation. The return separates each public name/span/CCN,
colon owner, consumer, curried control, typed control, performance comparison,
delivery gate and exclusion; it introduced no narrower rejection rule.

Comparison: given the intended contribution and contributor route, the source
requires full patch/discussion/review/check inspection and eventual
history-preserving contributor delivery. `correction-route.md` observes the
unchanged contribution, empty discussion/reviews, historical checks, current
permission and successful authenticated preview. This proves route feasibility;
actual acceptance remains slice 5. All public behavior examples and their
reader consumers remain slices 2–3, strict performance comparisons slice 4,
and full tests/current CI/final head-base-tree/merge/acknowledgement slice 5.
The source's named exclusions remain excluded. No missing slice-1 observation
was identified. Independent `/root/slice1_refactor` returned
`## REFACTOR COMPLETE`: no edits, 57-line evidence document, whitespace clean;
accepted observation locations unchanged, no redundant tests.

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Untyped branch arrows end at their ternary colon; internal colons and curried contract preserved | Slice 2: new public API regressions red→green in JS/TS/JSX/TSX and Vue; promised untyped frozen rows and supplemental controls pass |
| Typed branch signatures split; genuine function-type bodies preserved | Slice 3: red→green typed regressions in TS/TSX/Vue; typed frozen rows and literal function-type controls pass |
| Controls and shared consumers | Slices 2–3: focused consumer suites, retained reproducer showing only deferred rows failing |
| Strict affected and broad throughput | Slice 4: warmed paired measurements, each upper 95% slowdown bound strictly below 3% versus current target and frozen baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 5: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Use `lizard-language-support` for reader/test changes. Public `analyze_file` /
`analyze_source_code` results own semantic proof; mock only filesystem
boundaries. Inspect every reached consumer and record run/reused/excluded with
reasons. Independent post-change refactoring runs through the execution
workflow before delivery; keep new tests (and any retained author test) in a
cohesive small module below 250 lines while leaving unrelated large test files
byte-identical.

Focused command after relevant edits:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
```

Also rerun the retained reproducer (main and supplemental inputs) against the
candidate checkout. `AGENTS.md` requires full `nix develop -c python -m pytest`
before completion; the source contract additionally requires
`nix develop -c make` and final hosted CI. Run Nix commands sequentially
because the shell hook changes `.venv`; keep owned heavy measurement serial.
Refresh current CI applicability in execution.

## Ordered slices

### 1. Establish the contributor correction route
Type: Behavior
Status: done
Proof: refreshed PR/head/base, contributor-edit permission and authenticated
publication-route observations; preserve exact refusal if unavailable.

Behavior: given an explicit execution instruction, refresh PR534's patch,
discussion, checks and current target, verify the head still names the
intended contribution, and establish the authorized contributor-head delivery
route (non-mutating authenticated push preview where available) before
dependent implementation. A preview is not accepted publication; slice 5 owns
the real update. If the PR is closed, superseded, changed incompatibly or
cannot receive corrections, stop and return the disposition/destination
decision to Terry. Do not substitute an unrelated trunk patch.

Boundedness: one external feasibility observation; product unchanged. Failure
stops slices 2–5 and revises the plan before edits.

### 2. End untyped branch arrows at their own ternary colon
Type: Behavior
Status: planned
Proof: red→green public regressions for the reported, nested-ternary,
nested-arrow-internal, grouped and parenthesized-object branch cases plus a
JSX attribute and a Vue script block; the curried internal-colon control,
`internal_ternary`, `block_branches` and `plain_ternary` stay green; the
object-property case reports `f` 2–2 CCN 1; the reproducer's remaining
failures are only typed and deferred rows.

Behavior: given an expression-bodied arrow that is a ternary branch, analysis
ends it at the colon of the ternary it belongs to, so each branch reports its
own CCN and the enclosing function keeps its CCN. A `?`/`:` pair inside the
arrow's body, a nested arrow that creates no function, and a property or
computed-key colon do not end it. Implement the depth-ownership rule in the
shared state/mixin owners; adopt the raw proposal's useful split without its
snapshot overwrite or #531's object heuristic. Trace JS, TS, JSX/TSX and Vue
consumers.

Boundedness: one ownership rule and one semantic regression loop across the
shared readers; splitting by extension would leave divergent rules. A correct
local result is a safe stopping point, unmerged.

### 3. Consume typed branch signatures before branch separators
Type: Behavior
Status: planned
Proof: red→green TS/TSX/Vue regressions for typed return, object return type
and type-assertion branches; function-type alias, function-type return
(`inherited_function_type_*` rows) and following-function controls stay green;
the reproducer then fails only deferred rows.

Behavior: given typed branch arrows (`c ? (x: number): number => x : …`),
the parameter and return-type annotation colons are consumed as types and do
not decrement the runtime ternary depth, so the later branch colon ends the
first arrow and the alternate is reported separately with its CCN. Genuine
function-type bodies keep their real bodies. Extend slice 2's rule in the
TypeScript declaration/annotation owner rather than adding a second colon
recognizer.

Boundedness: one annotation-versus-separator decision on the TypeScript path
with its own regression loop; it reuses slice 2's ownership model. Safe stopping
point, unmerged.

### 4. Meet the strict performance budget without weakening semantics
Type: Behavior
Status: planned
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, affected API (frozen 92
files plus the four supplemental controls, frozen and hashed as a labelled
addition before timing, leaving the original manifest unchanged) and broader
CLI workloads each show an upper paired 95% slowdown bound strictly below 3%
versus current target (incremental) and the frozen `2e21e404` baseline
(cumulative). A broad pass cannot hide an affected failure. The raw intervals
remain diagnosis only.

Run literal `paired.py --baseline <checkout> --candidate <candidate> --workload api|cli --pairs 20 --seconds 1.5 --out <result>`
through Nix from the cache `corpus` directory, same interpreter, warmed
alternating pairs with batches over one second; record load, durations,
medians, variability and intervals. Confirm imported paths and public metric
tuples before timing. Profile only when needed and simplify the shared rule
rather than adding fast paths; re-run slices 2–3 proof after optimization.
Noise or an upper bound of 3% or more is failing; fix a prospective quiet
retry protocol rather than repeating for a pass.

### 5. Deliver the qualified correction through PR534
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
external probe, the untyped ownership rule, its typed-annotation extension
(separate regression loop on the TypeScript path, reusing one model), the
independent performance loop and hosted delivery. The cumulative design keeps
one ternary-depth ownership model and rejects the raw snapshot overwrite and
single-colon-check shapes. No remaining slice-specific concern was found;
contributor publication is bounded by the early probe. No human decision is
currently pending.
