# Preserve arrow boundaries across ternary and type colons

**Identity:** SEED-015#ternary-arrow-boundaries
**Source:** [Refined story](../../seeds/SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries)

## Outcome and boundaries

JavaScript, TypeScript, JSX/TSX and Vue analysis reports each expression-bodied
ternary branch arrow separately with its own CCN. Internal, genuine annotation
and property/computed-key colons retain their syntax; enclosing/following
names, spans and CCN and the outer-only curried-arrow contract are preserved.

Acceptance uses the source's key examples and
[PR534 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md). The frozen
[reproduce.py](../../../docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py)
checks names, start/end lines and CCN for the promised rows of
`inputs.json` and all of `supplemental-inputs.json`.

Excluded commitments: the existing object-comma sibling absorption (the
`next` rows of `object_property`/`computed_key`), object-literal ternary
bodies (`inherited_*`, `object_inside_ternary`; SEED-013), multiline
continuations (SEED-014), arrow parameter bindings (SEED-016), naming
changes, nested-arrow detection, ENS-only performance, a full JS/TS grammar.
No sibling is a formal blocking supplier. The original review's 15-minute
correction cutoff does not limit this selected follow-up. No numeric slice limit applies.

The 2026-10-27 `dough-execute-plan` instruction and established start authorize execution.

## Execution context

- Identity: `SEED-015#ternary-arrow-boundaries`; publisher
  `dashboard-territory.local-lizard`; agent Akiho-chan.
- Mode: story-branch; execution/originating checkout
  `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ`;
  branch `codex/preserve-arrow-boundaries-across-ternary-and-typ`.
- Remote `origin`, trunk `master`; ordinary increments target the execution
  branch. Starting revision `a2d2ab778d03cbb9ea96d243ab7533105d304eb2`;
  claim/published base `6f506711595e0ad832ab5a75f5a4750a2b362415`, confirmed
  on both remote refs at startup. Claim trunk CI is unobserved.
- Default checkout `/Users/terryyin/git/lizard` is not an execution location.
- Setup: checkout-bound Nix and initial public suites passed (43 tests).
  Use sequential `PIP_QUIET=1 nix develop -c`; no formatter/generator/hook
  applies. Commits credit the assigned agent; no new replanning restriction.
- CI observation: the original stream ended after GitHub acquisition failed;
  its exact identity and terminal evidence are in [PROOF.md](PROOF.md#ci-observation).
  Workflow `python-app.yml` (ID `38306292`) remains push-triggered for all branches.
- Active observer: coordinator `akiho-seed015-continuation-a5bbe368`, cell 5,
  session 21368, `/tmp/dough-ci-501/watch-LmZv82`, PID 80173; same checkout,
  repository and execution-branch target. Original finished stream remains closed.
- Slice 1 route: [correction-route.md](../../../docs/reviews/fabio-malpezzi-20261027/pr534/correction-route.md).
  Head unchanged/open, authenticated preview available; current target evidence is in PROOF.md.

## Existing owners and architectural direction

PFE: use TypeScriptStates' canonical ternary dispatch, JavaScriptFunctionsMixin's
signature/expression lifecycle and the composed binding/object/declaration owners.
PR536 now centralizes formal capture in JavaScriptBindings; reuse that accepted
owner. TypeScriptReader composes these paths for JS/TS/JSX/TSX/Vue and CJS/MJS.
Keep one ternary-depth rule and annotation recognizer; no parallel parser,
metric counter or PR-specific colon switch, and preserve the public facade.

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

SEED-013/014 corrections are already in the execution baseline. Refreshed public
observations preserve them; do not reintroduce their previous state heuristics.

Preparation observations are retained in [PROOF.md](PROOF.md#premises-observed-during-preparation).

## Proof ownership and gates

Accepted source comparisons are retained in [PROOF.md](PROOF.md).

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Untyped branch arrows end at their ternary colon; internal colons and curried contract preserved | Slice 2: new public API regressions red→green in JS/TS/JSX/TSX and Vue; promised untyped frozen rows and supplemental controls pass |
| Typed branch signatures split; genuine function-type bodies preserved | Slice 3: red→green typed regressions in TS/TSX/Vue; typed frozen rows and literal function-type controls pass |
| Controls and shared consumers | Slices 2–3: focused consumer suites, retained reproducer showing only deferred rows failing |
| Preserve semantics while removing overhead and reconciling current owners | Slices 4–6: public regressions/replays; preserve accepted parameter behavior after PR536 integration |
| Strict affected and broad throughput | Slice 7: four original gates plus two augmented-API gates; all upper 95% slowdown bounds strictly below 3% versus current/frozen `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 8: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

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
dependent implementation. A preview is not accepted publication; slice 8 owns
the real update. If the PR is closed, superseded, changed incompatibly or
cannot receive corrections, stop and return the disposition/destination
decision to Terry. Do not substitute an unrelated trunk patch.

### 2. End untyped branch arrows at their own ternary colon
Type: Behavior
Status: done
Proof: red→green public regressions for the reported, nested-ternary,
nested-arrow-internal, grouped and parenthesized-object branch cases plus a
JSX attribute and a Vue script block; the curried internal-colon control,
`internal_ternary`, `block_branches` and `plain_ternary` stay green; the
object-property case reports `f` 2–2 CCN 1; the reproducer's remaining
failures are only typed and deferred rows.

Behavior: a ternary branch arrow ends at its own separator, retaining branch
and enclosing CCN. Internal `?`/`:`, nested arrows creating no function and
property/computed-key colons retain ownership. Use shared depth owners without
raw #534's snapshot overwrite or #531's object heuristic; trace all shared
JS/TS/JSX/TSX/Vue consumers.

### 3. Consume typed branch signatures before branch separators
Type: Behavior
Status: done
Proof: red→green TS/TSX/Vue regressions for typed return, object return type
and type-assertion branches; function-type alias, function-type return
(`inherited_function_type_*` rows) and following-function controls stay green;
the reproducer then fails only deferred rows.

Behavior: typed branch parameter/return colons are consumed as types without
decrementing runtime depth; the actual separator ends the first arrow and the
alternate has its own CCN. Genuine function types retain their real bodies.
Extend the existing declaration/annotation owner, without another recognizer.

Learning G2 requires metric-safe annotation lookahead with canonical runtime
context; the bounded approach is retained in [PROOF.md](PROOF.md#slice-3-investigation).

### 4. Remove whole-stream annotation bookkeeping
Type: Structure
Status: done
Proof: slices 2–3 public regressions and all 128 retained rows stay green;
new owner-context cases and full-metric comparisons preserve existing behavior.

Structure: move group eligibility/emptiness bookkeeping into the existing
binding/signature lifecycle and query it only at the annotation decision.
The initial immutable comparison found +8.480%–9.567% incremental API cost;
an unchanged-output profile implicates preprocessing overhead without allocating
the full slowdown. Simplify the shared rule; add no raw-source fast path.
An independently refactored, semantically qualified commit enables slice 5's
exact-revision measurement. G4 remains open; this slice grants no timing waiver.

### 5. Avoid unnecessary annotation-context traversal
Type: Structure
Status: done
Proof: bounded unchanged-output profile; all 128 public/FileAnalyzer rows and
seven public fields match 8dac7b8; focused shared-consumer regressions pass.

Structure: the committed optimization still has current/API upper +7.623%.
Profile the remaining preprocessing cost, then use existing canonical token/
signature ownership to avoid unnecessary annotation queries. Add no parallel
tracker or raw-source fast path. Preserve runtime groups, trivia and genuine
return annotations. Independent refactoring and semantic delivery precede measurement.

### 6. Reconcile accepted current signature owners
Type: Structure
Status: done
Proof: original 128 names/spans/CCN, current parameter/output regressions and
shared-consumer proof pass; see current-owner-integration.md for PR536 changes.
Structure: integrate current `1bc7cbd` without rewriting published history.
Reuse PR536's canonical binding/formal/type owners; preserve this correction's
ternary/trivia behavior. Independent refactor and delivery freeze code for 7.

### 7. Qualify original and supplemented timing workloads
Type: Behavior
Status: planned
Proof: raw warmed pairs, immutable implementation/corpus/harness hashes,
exact revisions and all six upper 95% slowdown bounds strictly below 3%.

Behavior: 128 affected API inputs and unchanged broad CLI each pass versus
current and frozen `2e21e404`: same Nix, 20 alternating warmed pairs, 1.5-second
calibration, batches over one second. Freeze revisions/harness/corpus/imports/
public tuples; retain every duration, median, variability and interval.
Also freeze all 38 added controls; augmented 166-input API passes both targets.
Preserve prior results. Every upper bound must be strictly below 3%; no broad
averaging, noise waiver or repeat without prospective justification. Further
optimization repeats semantic proof. Literal protocol remains in PROOF.md.

### 8. Deliver the qualified correction through PR534
Type: Behavior
Status: planned
Proof: final contributor-head SHA and accepted push, green current required CI,
exact-head guarded merge receipt, and one acknowledgement URL.

Behavior: given semantic/performance-qualified code, publish contributor
corrections without force or lost history. Run full pytest/make, verify final
head/base/tree and all required checks, then perform an exact-head hosted merge.
Follow the
[complete-cycle contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md);
its 15-minute triage rule does not re-defer this selected correction. Hosted
merge is not a Dough Land operation. After confirmed merge, with explicit
acknowledgement authority, post one thank-you noting material corrections
(check for a prior receipt). Retain evidence in the review record, then wrap up.

Pending CI, merge or acknowledgement leaves this slice unfinished.

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

### G4. Incremental affected API exceeds the strict budget
Reported: slice 5 — "Current/API upper +9.567% initially in slice 4 and +7.623% at committed 8dac7b8; both material failures."
Story clause: "with a paired 95% upper slowdown bound strictly below 3% for both incremental and cumulative affected and broader workloads."
Disposition: receiving slice 7

### G7. Current-owner integration changes a nested runtime-group control
Reported: slice 6 — "Eight new runtime-group controls report an additional anonymous CCN-1 arrow after PR536 integration; 828 other tests pass."
Story clause: "the enclosing and following functions keep their correct names, spans and CCN."
Disposition: proved by slice 6: current-owner-integration.md exact three-version probes and explicit initializer assertions match accepted current1bc; original 128 rows and curried controls pass, surrounding metrics preserved.

### G5. Native CI observation ended after acquisition failed
Reported: slice 4 — "CI_MONITOR_UNAVAILABLE: GitHub workflow-runs acquisition failed; original observer finished and its process ended."
Story clause: "satisfactory performance/CI."
Disposition: receiving slice 8

### G6. Earlier qualification driver terminal receipt unavailable
Reported: slice 5 — "No original numeric exit code is retained; four complete result/log pairs match, and independent postcheck terminated successfully."
Story clause: "with coherent ownership, discriminating regression proof and satisfactory performance/CI."
Disposition: receiving slice 7
