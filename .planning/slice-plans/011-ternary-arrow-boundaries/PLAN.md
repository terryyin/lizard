# Preserve arrow boundaries across ternary and type colons

**Identity:** SEED-015#ternary-arrow-boundaries
**Source:** [Refined story](../../seeds/SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries)

## Outcome and boundaries

JavaScript, TypeScript, JSX/TSX and Vue analysis reports each expression-bodied
ternary branch arrow separately with its own CCN. Internal, genuine annotation
and property/computed-key colons retain their syntax; enclosing/following
names, spans and CCN and the outer-only curried-arrow contract are preserved.

Acceptance uses source examples and [PR534 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md).
The frozen reproducer checks all promised input/supplemental names/spans/CCN; literal proof remains in PROOF.md.

Excluded commitments: the existing object-comma sibling absorption (the
`next` rows of `object_property`/`computed_key`), object-literal ternary
bodies (`inherited_*`, `object_inside_ternary`; SEED-013), multiline
continuations (SEED-014), arrow parameter bindings (SEED-016), naming
changes, nested-arrow detection, ENS-only performance, a full JS/TS grammar.
No sibling blocks this story; review's 15-minute cutoff does not limit selected work. No numeric slice limit applies.

The 2026-10-27 `dough-execute-plan` instruction and established start authorize execution.

## Execution context

- Identity: `SEED-015#ternary-arrow-boundaries`; publisher
  `dashboard-territory.local-lizard`; agent Akiho-chan.
- Mode: story-branch; workspace `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ`; branch `codex/preserve-arrow-boundaries-across-ternary-and-typ`.
- Remote `origin`, trunk `master`; ordinary increments target the execution branch.
  Starting revision `a2d2ab778d03cbb9ea96d243ab7533105d304eb2`; claim `6f506711595e0ad832ab5a75f5a4750a2b362415` confirmed on both remote refs. Claim trunk CI is unobserved.
- Default checkout `/Users/terryyin/git/lizard` is not an execution location.
- Setup: checkout-bound Nix and initial public suites passed (43 tests).
  Use sequential `PIP_QUIET=1 nix develop -c`; no formatter/generator/hook
  applies. Commits credit the assigned agent; no new replanning restriction.
- CI observation: original acquisition failed; exact identity/terminal evidence: [PROOF.md](PROOF.md#ci-observation).
  Workflow `python-app.yml` (ID `38306292`) remains push-triggered for all branches.
- Active observer: coordinator `akiho-seed015-continuation-a5bbe368`, cell 5,
  session 21368, `/tmp/dough-ci-501/watch-LmZv82`, PID 80173; same checkout,
  repository and execution-branch target. Original finished stream remains closed.
- Slice1 route: [correction-route.md](../../../docs/reviews/fabio-malpezzi-20261027/pr534/correction-route.md); open/preview available; current target evidence in PROOF.md.

## Existing owners and architectural direction

PFE: canonical ternary, signature, binding/object/declaration owners remain.
Reuse PR536's JavaScriptBindings formal capture and TypeScriptReader's shared
JS/TS/JSX/TSX/Vue/CJS/MJS composition.
Keep one ternary-depth rule and annotation recognizer; no parallel parser,
metric counter or PR-specific colon switch, and preserve the public facade.

Reject raw #534's overwritten enclosing snapshot and a lone colon heuristic.
Each expression arrow retains its owning depth; only that separator ends it.
Internal pairs and genuine annotations reach their own owners first. Local
single-state reader delivery reuses Go's pattern, preserving generic hooks.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for CCN, language reader and function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both remain Accepted/unsuperseded;
local reader ownership needs no North Star topic, new ADR or exception.

SEED-013/014 corrections are already in the execution baseline. Refreshed public
observations preserve them; do not reintroduce their previous state heuristics.


## Proof ownership and gates

Accepted source comparisons are retained in [PROOF.md](PROOF.md).

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Untyped branch arrows end at their ternary colon; internal colons and curried contract preserved | Slice 2: new public API regressions red→green in JS/TS/JSX/TSX and Vue; promised untyped frozen rows and supplemental controls pass |
| Typed branch signatures split; genuine function-type bodies preserved | Slice 3: red→green typed regressions in TS/TSX/Vue; typed frozen rows and literal function-type controls pass |
| Controls and shared consumers | Slices 2–3: focused consumer suites, retained reproducer showing only deferred rows failing |
| Preserve semantics while removing overhead and reconciling current owners | Slices 4–8: public regressions/replays; retain accepted parameters, binding children and reader lifecycle/hooks |
| Strict affected and broad throughput | Slice 9: four original gates plus two augmented-API gates; all upper 95% slowdown bounds strictly below 3% versus current/frozen `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 10: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

## Ordered slices

### 1. Establish the contributor correction route
Type: Behavior
Status: done
Proof: refreshed PR/head/base, contributor-edit permission and authenticated
publication-route observations; preserve exact refusal if unavailable.

Behavior: refresh PR534's complete patch/discussion/checks/current target and
intended head; verify contributor publication permission before implementation.
Preview is not publication (owned by 10). Closed/superseded/incompatibly changed
or uneditable contribution returns its disposition/destination decision to
Terry; no unrelated trunk substitute.

### 2. End untyped branch arrows at their own ternary colon
Type: Behavior
Status: done
Proof: red→green reported/nested/internal/grouped/object/JSX/Vue regressions;
curried/internal/block/plain controls stay green, property `f` is 2–2 CCN 1.
Only typed and deferred reproducer rows remain; detailed proof in PROOF.md.

Behavior: shared depth owners end branch arrows at their own separator, retaining
branch/enclosing CCN and internal/nested-curried/property/computed-key ownership.
No raw #534 snapshot overwrite or #531 object heuristic; trace all shared consumers.

### 3. Consume typed branch signatures before branch separators
Type: Behavior
Status: done
Proof: red→green TS/TSX/Vue return/object/assertion branches; aliases,
function-type real bodies and following controls stay green. Only deferred
reproducer rows remain; original expectations and detail stay in PROOF.md.

Behavior: annotation owners consume typed parameter/return colons without runtime
depth changes; real separators split branch CCN, genuine function types retain
their real bodies. Extend the existing owner without another recognizer.

Learning G2 requires metric-safe annotation lookahead with canonical runtime
context; the bounded approach is retained in [PROOF.md](PROOF.md#slice-3-investigation).

### 4. Remove whole-stream annotation bookkeeping
Type: Structure
Status: done
Proof: all 128 rows, new owner-context and seven-field comparisons preserve behavior.

Structure: initial +8.480%–9.567% interval/attribution justified moving group
context into binding/signature lifecycle. No raw-source shortcut or waiver;
semantic qualification/refactoring/delivery remain in PROOF.md.

### 5. Avoid unnecessary annotation-context traversal
Type: Structure
Status: done
Proof: bounded unchanged-output profile; all 128 public/FileAnalyzer rows and
seven public fields match 8dac7b8; focused shared-consumer regressions pass.

Structure: after upper +7.623%, canonical token/signature evidence and demand buffering
reduced work, preserving runtime/trivia/types with no parallel tracker/raw shortcut.

### 6. Reconcile accepted current signature owners
Type: Structure
Status: done
Proof: 128 rows/current parameter/output consumers pass; see current-owner-integration.md.
Structure: integrate current `1bc7cbd` without rewriting published history.
Reuse canonical formal/type owners; preserve ternary/trivia behavior, refactor/deliver before 7.

### 7. Deliver sole-state tokens without unused dispatch
Type: Structure
Status: done
Proof: 166 seven-field rows/CLI match ec2; hook/state/finalization and diagnostic proof retained.
Structure: local TypeScriptReader delivery uses its sole state directly when
the generic token hook is unchanged; preserve generic fallback and eof/state
finalization. Reuse Go's delivery pattern, no grammar receipt/counter replay.
Refactor and semantic delivery precede 8; no throughput waiver.

### 8. Dispatch the canonical root without unused binding work
Type: Structure
Status: done
Proof: 166 seven-field rows/CLI match 13c; root/child binding, callback/replay,
configured-root fallback and existing reader lifecycle/consumer contracts pass.
Structure: an exact reader-created root confirmed without a binding candidate
uses canonical CodeStateMachine dispatch. Child binding wrappers remain intact;
retain custom-root/hook/state/container fallbacks and finalization. No new parser,
counter or metadata receipt; refactor/deliver before 9, then measure exact code.

### 9. Qualify original and supplemented timing workloads
Type: Behavior
Status: done
Proof: root-performance.md retains exact737/34cd/frozen six-gate qualification;
all six paired95% upper bounds are strictly below3%, with before/after correspondence.

Behavior: 128 affected API inputs and unchanged broad CLI each pass versus
current and frozen `2e21e404`: same Nix, 20 alternating warmed pairs, 1.5-second
calibration, batches over one second. Freeze revisions/harness/corpus/imports/
public tuples; retain every duration, median, variability and interval.
Also freeze all 38 added controls; augmented 166-input API passes both targets.
Preserve prior results. Every upper bound must be strictly below 3%; no broad
averaging, noise waiver or repeat without prospective justification. Further
optimization repeats semantic proof. Literal protocol remains in PROOF.md.

### 10. Deliver the qualified correction through PR534
Type: Behavior
Status: done
Proof: final contributor-head SHA and accepted push, green current required CI,
exact-head guarded merge receipt, and one acknowledgement URL.

Behavior: publish qualified contributor corrections without force/lost history.
Run full pytest/make; verify final head/base/tree and required checks before
exact-head hosted merge. Follow the [contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md);
its 15-minute triage cutoff does not re-defer selected work. Hosted merge is
not Dough Land. After confirmed merge, use explicit acknowledgement authority
for one thank-you noting material corrections; check prior receipt, retain URL
and review evidence, then wrap up.

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

### G4. Incremental affected API exceeds budget (first reported in slice 4)
Reported: slice 8 — "Current/API upper +9.567% initially, +7.623% at 8dac, ec2 +6.891%/+7.259%; 13c original interval [3.102%,4.156%] fails materially and augmented upper3.556% remains unproved."
Story clause: "with a paired 95% upper slowdown bound strictly below 3% for both incremental and cumulative affected and broader workloads."
Disposition: proved by slice 9: root-performance.md exact737 all six upper bounds1.714/.311/−8.185/.633/2.528/−4.222% pass; complete numeric receipts and immutable inputs/imports/public correspondence retained.

### G7. Current-owner integration changes a nested runtime-group control
Reported: slice 6 — "Eight new runtime-group controls report an additional anonymous CCN-1 arrow after PR536 integration; 828 other tests pass."
Story clause: "the enclosing and following functions keep their correct names, spans and CCN."
Disposition: proved by slice 6: current-owner-integration.md exact three-version probes and explicit initializer assertions match accepted current1bc; original 128 rows and curried controls pass, surrounding metrics preserved.

### G5. Native CI observation ended after acquisition failed
Reported: slice 4 — "CI_MONITOR_UNAVAILABLE: GitHub workflow-runs acquisition failed; original observer finished and its process ended."
Story clause: "satisfactory performance/CI."
Disposition: proved by slice 10: delivery-acceptance.md exactQ native CI success and both14-job matrices green; old observer loss remains historical, final completion/shutdown still required.

### G6. Earlier qualification driver terminal receipt unavailable
Reported: slice 5 — "No original numeric exit code is retained; four complete result/log pairs match, and independent postcheck terminated successfully."
Story clause: "with coherent ownership, discriminating regression proof and satisfactory performance/CI."
Disposition: proved by slice 7: final-performance.md and final-postcheck.json retain all six child/driver numeric exits; historical receipt loss remains explicit.

### G8. Qualification setup reused a path-dependent historical CLI hash
Reported: slice 7 — "First freeze exited 1 before timing; historical CLI hash embeds the prior absolute corpus prefix."
Story clause: "satisfactory performance/CI."
Disposition: proved by slice 7: final-setup-failure.json and final-performance.md preserve failed setup, exact prefix-only cause and setup2 byte-identical three-version outputs.

### G9. Diagnostic baseline imported a candidate-only helper
Reported: slice 7 — "Control setup exited 1 before profiling because current target has no parameter_group helper."
Story clause: "satisfactory performance/CI."
Disposition: proved by slice 7: api-attribution.md preserves setup1 source/exit, conditional-import control2 correction and exact seven-field equality; no product or statistical change.

### G10. Reader protocol fixture omitted existing indexing capability
Reported: slice 7 — "Eight protocol tests passed; the alternate iterable fixture failed at unchanged preprocess's parallel_states[0] before delivery."
Story clause: "satisfactory performance/CI."
Disposition: proved by slice 7: reader-dispatch.md retains failed fixture/receipt and corrected indexed-without-len container; actual grammar/metrics and all eleven protocol observations pass.

### G11. Auxiliary refactor audit counted only Python consumers
Reported: slice 10 — "Audit wrongly counted only located consumer rows mentioning .py;492 covers every consumer kind"
Story clause: "satisfactory performance/CI."
Disposition: proved by slice 10: delivery-local-proof.md retains failed auxiliary filter and corrected audit exit0; all492 located consumers, original correct assertions and accepted product/proof remain unchanged.
