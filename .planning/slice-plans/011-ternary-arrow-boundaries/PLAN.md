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

Preparation observations are retained in [PROOF.md](PROOF.md#premises-observed-during-preparation).

## Proof ownership and gates

Accepted source comparisons are retained in [PROOF.md](PROOF.md).

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Untyped branch arrows end at their ternary colon; internal colons and curried contract preserved | Slice 2: new public API regressions red→green in JS/TS/JSX/TSX and Vue; promised untyped frozen rows and supplemental controls pass |
| Typed branch signatures split; genuine function-type bodies preserved | Slice 3: red→green typed regressions in TS/TSX/Vue; typed frozen rows and literal function-type controls pass |
| Controls and shared consumers | Slices 2–3: focused consumer suites, retained reproducer showing only deferred rows failing |
| Strict affected and broad throughput | Slice 4: warmed paired measurements, each upper 95% slowdown bound strictly below 3% versus current target and frozen baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 5: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Reusable proof commands and conventions: [PROOF.md](PROOF.md#proof-commands-and-conventions).

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
Status: done
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
Status: done
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

Learning G2 requires metric-safe annotation lookahead with canonical runtime
context; the bounded approach is retained in [PROOF.md](PROOF.md#slice-3-investigation).

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

## Story obligations

### G1. Typed branch return signatures remain combined
Reported: slice 2 — "Candidate main replay: 92 cases, only typed_arrows.ts, typed_arrows.tsx, typed_object_return.ts, typed_object_return.tsx, and vue_typed_arrows.vue still fail."
Story clause: "a genuine signature annotation is consumed as a type before it can be taken as a runtime branch separator."
Disposition: proved by slice 3: corrected-semantics.md typed signatures; all 92 frozen rows pass through public analysis/FileAnalyzer.

### G2. Return annotations with untyped or empty parameters remain combined
Reported: slice 3 — "Ordinary (x): number => x and (): number => 1 work outside a ternary; their ternary equivalents still report one anonymous CCN-2 arrow instead of separate CCN-1/2 arrows."
Story clause: "a genuine signature annotation is consumed as a type before it can be taken as a runtime branch separator."
Disposition: proved by slice 3: test_typed_branch_annotations_are_not_runtime_colons and labelled signature inputs preserve untyped/empty parameters, names/spans/CCN.

### G3. Nested runtime groups steal an outer separator during lookahead
Reported: slice 3 — "All six TS/TSX/Vue nested call/group cases fail on the first lookahead implementation, including the following typed cb initializer."
Story clause: "Colons inside the arrow's own body, genuine TypeScript annotations, and property or computed-key colons keep their own syntax, and the enclosing and following functions keep their correct names, spans and CCN."
Disposition: proved by slice 3: test_nested_runtime_separator_keeps_following_type_annotation and six baseline/candidate full-metric comparisons preserve runtime groups and named cb.
