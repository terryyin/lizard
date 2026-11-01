# Preserve binding-reader ownership and deliver PR #525

**Identity:** SEED-023#correct-pr525-binding-boundaries
**Source:** [Correct, qualify and merge PR #525](../../seeds/SEED-023-pr525-binding-boundaries.md#correct-pr525-binding-boundaries)
**Source review:** [Source-only commitments](source-review.md)

## Goal and scope

Integrate PR #525's remaining binding lifecycle correction into current master,
refactor the aggregate concept, qualify every affected workload below the strict
3% upper slowdown bound, and merge the qualified contributor head with one
thank-you message. Preserve contributor history and current metric contracts.
Pre-existing label/deep-object limitations are not new promises; exposed or
introduced regressions require an explicit source-compatible disposition.

## Execution identity and current decisions

- Mode: Story Branch; owned workspace created for this story:
  `/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries`,
  branch `codex/correct-pr525-binding-boundaries`.
- Originating/integration checkout: `/Users/terryyin/git/lizard`.
- Internal publication: `origin`, trunk `master`, retained execution branch.
  Admission `db54a2bab5fe28475982720f043479e99bcafc03` is published on trunk;
  its product bytes equal frozen starting master `6d288d4cdcb85bc50ab4c8a887196177607eeb7d`.
- Contributor: `leonrosenshein/lizard:jsBindingCandidateCrash`, original head
  `63798156566ea4ab100048dc1eb21a7366cde380`, parent `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
  `maintainer_can_modify=true`; divergent fast-forward `git push --dry-run`
  from unpublished probe `ebe05d0ecd09b135039920fc9b3b152f41482dd3` succeeded.
  No probe or correction has been pushed to the contributor branch yet.
- The human explicitly authorizes correction, refactor, <3% qualification,
  contributor publication, exact-head merge and one thank-you. No historical
  Fabio fifteen-minute correction cutoff applies. Existing planning authority
  permits bounded replanning within this outcome.
- PFE: `JavaScriptBindings` owns actual/provisional binding frames, defaults and
  formals; `TypeScriptStates` and its nested syntax readers own runtime group
  dispatch. Existing `parameter_group` distinguishes groups that may be arrow
  formals from proven calls. Attempt 1 showed that giving proven call groups a
  full binding tracker is unnecessary and too costly (G7). Reuse the ordinary
  nested syntax reader for those groups, with an explicit call-group context
  for its enclosing comma if needed; retain binding tracking where actual
  bindings/signatures remain possible. This is a local existing-owner change,
  not another bracket counter, buffering layer, per-token wrapper or PR switch.
- ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) preserves function,
  CCN, NLOC, token and parameter meanings. ADR-0000 governs architectural
  decisions; this local reader correction proposes no exception or new boundary.
- Required project tooling: Nix (Git excepted), high-level analyzer proof;
  full pytest and make before accepting the final candidate. No active hooks
  or project selective formatter/generator apply to these Python reader edits;
  pycodestyle is check-only, run by make. Independent post-change refactor is
  required at each implementation acceptance boundary.
- Freeze workloads before timing. Preserve production/input/harness hashes,
  unfavorable attempts and original-parent comparisons on parent-safe inputs.
  Use twenty alternating warmed pairs calibrated >=1.2 seconds; every measured
  batch >=1 second; paired 95% upper bound strictly <3%. API including public
  parameters, NS consumers and broad CLI are separate required observations.
  Measure incremental versus frozen current master, plus applicable cumulative
  versus original parent. A broad pass does not cancel an affected failure.
- Heavy verification runs serially on this host, with fresh load/memory check
  before timing. Other work is unknown and must not be terminated. Coordinator
  owns final performance runs; delegates must not time concurrently. Reconsider
  on contention, changed production/inputs, failed proof or longer commands.
- Before the first paired measurement, qualification adds the newly traced
  known-call/object argument contexts to a separate complete corpus (171 files)
  and original-parent-safe corpus (85 files). The profiled 131/45/100 sets stay
  immutable. Added sources are the same five ordinary/member/constructor/optional
  callees and object/statement contexts already observed by the semantic tests,
  for JS/TS/TSX/Vue. `qualification-inputs.json` freezes this supplement;
  no timing result motivated its selection. Broad CLI remains the same 100
  original source files. API and NS use 1.2-second calibration; broad CLI uses
  4-second calibration prospectively for process/discovery noise. Every workload
  still uses 20 alternating pairs and the strict 95% upper <3% criterion.

## Decisive premises and proof ownership

- Exact remaining defect reached through public `analyze_file` in this checkout:
  TSX callback containing `e(r[0]); if(x) g(x)` reports `(anonymous), CCN 1,
  lines 1–2` instead of CCN 2 / lines 1–4; successor `after(y)` stays CCN 2.
  Object-array function reports line 2 instead of line 3; successor is retained.
- Prior authored and nearby-control baseline is recoverable at `5f7d58fb`.
  Replay it against the frozen current checkout before changing production;
  retain discriminating red observations and all original authored expectations.
- Contributor write path is settled above. Merge remains gated on fresh remote
  head/base, tree equality to measured/proven candidate and applicable hosted CI.
- Slice 1 owns analyzer/CLI behavior and consumer coverage. Slice 2 owns measured
  runtime and final local checks. Slice 3 owns history-preserving publication,
  hosted head/base checks, guarded merge and acknowledgement.

## CI observation and recovery

On resume, the old code-mode host generation was stale. Exact coordinator/root
record search found no matching observer JSON and `ps` found no matching
`ci-mailbox.mjs` process; the old stream is absent. The newly armed observer is
owned by coordinator `pr525-correct-resume`, GitHub Actions `python-app.yml`
(`Python application`), target `terryyin/lizard:codex/correct-pr525-binding-boundaries`.
Mailbox `/tmp/dough-ci-501/watch-Sae8bZ`, PID `77130`, session `60875`, yielded
cell `g3:15`. Runtime belongs to this exact execution checkout. Reuse it for
managed delivery; recover and close it before replacing or retiring the checkout.
Admission `db54a2b` and preparation `1ba5cb5` were accepted on trunk before this
branch observer and have unobserved trunk coverage; never register them as if
they were this observer's branch publications. Slice 1 was subsequently delivered
on this execution branch at `4a2081598ac8d70debafe64d672d65fd05857944` and registered
on this observer. Exact handles are operational recovery facts, not CI success.

## Ordered slices

### 1. Keep nested expression brackets with their binding reader
Type: Behavior
Status: done
Behavior: Replayed parser tokens do not consume binding brackets twice;
subscripts and template substitutions do not end their enclosing callbacks;
expression-arrow separators preserve subsequent object members and functions.
Retain formal parameters, signature/ternary ownership, NS, Vue and CLI behavior.
Reconcile the complete contributor change in the owned checkout while preserving
current master and authored history. A merge conflict is resolved against the
current semantic owners, not by taking either whole file.
Proof: Add focused high-level regression tests in a cohesive language-test
module for the eleven authored cases plus successor CCN/parameters and template
controls, across relevant suffixes. Show red on frozen current master and green
on the correction. Run the complete JS/TS/Vue affected surface and discovered
CLI/extension/reader-dispatch consumers. Use the installed consumer-trace script;
inspect every listed consumer's purpose and give a run/reused/excluded disposition.
Retain pre-existing limits separately; never weaken an expected result to accept
a new failure. Independent refactor precedes delivery.

### 2. Remove unjustified binding-dispatch cost and qualify the candidate
Type: Structure
Status: done
Structure: Profile the corrected aggregate change and simplify its actual hot
paths within the existing reader/binding owners, preserving slice 1's external
behavior. Avoid the original proposal's repeated ended-property overhead when
the existing frame state can answer the same domain question.
Proof: Reuse unchanged semantic observations and rerun those invalidated by edits;
independent refactor. Freeze complete affected, parent-safe and broad input sets,
then run serial installed paired API/parameter, NS and broad CLI measurements
against frozen current and applicable original parent. Keep all attempts.
Require every applicable upper bound <3%; profile a failing valid candidate
before changing it. Run full `nix develop -c python -m pytest` and
`nix develop -c make`, plus applicable hooks and tests. Required proof cannot be
deferred to a later owner; no merge on an unresolved workload.

### 3. Merge the exact qualified contribution and acknowledge the author
Type: Behavior
Status: planned
Behavior: Preserve the contributor commit and publish current-master integration
and corrections to `jsBindingCandidateCrash` without force. Verify exact remote
head/base and complete aggregate diff, production/test tree against accepted
semantic/performance evidence, and current workflow/matrix applicability. Wait
for applicable hosted checks on the exact head, then merge guarded by that SHA.
After confirmed merge, check for an existing acknowledgement and post one thank-
you explaining material corrections and validation, retaining its URL. Update the
maintained review disposition and close the owner through ordinary wrap-up.
Proof: Regression prerequisites are the accepted slice 1 semantic observations,
slice 2 full pytest/make and all required <3% measurements on matching bytes;
no live contributor publication/merge without their current correspondence.
Retain remote head/tree receipts, hosted run/job conclusions, guarded merge SHA,
merge confirmation, single acknowledgement URL and fresh trunk CI receipt.
The coordinator performs these delivery/hosting actions; no redundant coding
agent is needed for the external administrative action.

## Concern evidence

Plan refinement not needed: three cohesive stopping points preserve one outcome,
with behavior before performance and exact-head delivery last. No remaining
slice-specific planning concern; runtime cost is measured work owned by slice 2,
not an assumed pass. No product-boundary change or independent second outcome.

## Story obligations

### G1. Generic-call comma retains the expression arrow
Reported: slice 1 — "global named expression arrow `const f = (x) => foo<A,\n B>(x)` ends at generic comma after generalizing comma termination."
Story clause: "retain current arrow, formal-parameter, signature, template, extension and CLI behavior."
Disposition: proved by slice 1: `testJavaScriptBindingBoundaries.py::test_global_arrow_generic_call_keeps_continued_expression`, the existing generic continuation test, and implementation/affected-final.txt preserve lines 1–2 on TS/TSX/Vue after the binding/object comma restriction. The coordinator inspected the source and terminal 897-test pass.

### G2. Labeled block arrow remains a pre-existing limitation
Reported: slice 1 — "label-body arrow omission in seven suffixes"
Story clause: "Pre-existing label and deep-object limitations are not new promises"
Disposition: excluded "Pre-existing label and deep-object limitations are not new promises"

### G3. Typed class-field false positive is unchanged
Reported: slice 1 — "typed class-field phantom function `d` in TS/TSX/Vue"
Story clause: "retain current arrow, formal-parameter, signature, template, extension and CLI behavior."
Disposition: proved by slice 1: implementation/current.json and correction.json class-type rows preserve the same pre-existing class-field output; this proves compatibility at that unrelated discovery path, not correctness of class-field function discovery. No new loss is attributed to the binding correction.

### G4. Anonymous generic-call precision is unchanged
Reported: slice 1 — "anonymous ordinary/member/constructor call arrows with multiline generic arguments still end at line 1 in both revisions."
Story clause: "retain current arrow, formal-parameter, signature, template, extension and CLI behavior."
Disposition: proved by slice 1: implementation/generic-controls.json compares frozen-current and corrected TS/TSX results through analyze_file and shows identical pre-existing anonymous-generic spans, while the named/global case keeps lines 1–2. This proves preservation, not a generic-call accuracy improvement; the limitation remains honestly reported and is not used as proof of corrected generic parsing.

### G5. Known call groups capture provisional formals unnecessarily
Reported: slice 2 — "Candidate _read_parameter71,120 vs current67,560 calls"
Story clause: "profile and remove unjustified runtime cost"
Disposition: proved by slice 2: cost/pre-edit-profile.json and post-edit-profile.json retain the first removed-capture experiment. G7 exposed the remaining full-tracker cost. The final cost-return/post-edit-arguments-profile.json and profile-comparison.json instead show ordinary syntax readers removing 35,200 binding-token reads and 3,200 candidates on the immutable forty-source workload; actual/provisional formals retain their existing binding owner. cost-return/affected-final.txt and consumers-final.txt pass on those exact bytes. These profiles establish removed work, not paired qualification.

### G6. Ternary groups retain their formal parameters
Reported: slice 2 — "over-specialized the differing-function-name branch to track_parameters=False; existing ternary arrows also reach it."
Story clause: "retain current arrow, formal-parameter, signature, template, extension and CLI behavior."
Disposition: proved by slice 2: existing parameter_group eligibility routes every dispatch/object group through the shared JavaScriptFunctionsMixin helper; cost-return/affected-final.txt passes 897 tests and consumers-final.txt passes 499. The coordinator inspected the typed/ternary/formal assertions and final-correspondence.json. Rejected specialization patch, profiles and failed assertions remain in cost/ as evidence; no assertion was weakened.

### G7. Proven call arguments exceed the slowdown bound
Reported: slice 2 — "arguments-api slowdown 5.554442857168002%; paired 95% interval [5.0636041944036325, 6.047574631754602]%"
Story clause: "Each affected API/parameter consumer and broad CLI workload has a paired 95% upper slowdown bound strictly below 3% against frozen current master."
Disposition: proved by slice 2: measurements/attempt2/arguments-api.json and arguments-ns.json pass on accepted-candidate.json bytes with upper bounds -0.535% and -0.961%. All eight required comparisons pass; the worst upper bound is +0.526% (parent CLI). The coordinator verified all 107 runtime hashes, stable observations, twenty pairs and minimum measured batch >=1 second. Failed attempt1 is retained unchanged.

## Learnings

None beyond the settled premises above.

## Accepted slice 2 proof

`docs/reviews/pr525-correction-20261101/README.md` records the complete final
qualification. `cost-return/` establishes removed call-tracker work and fresh
897/499/42 semantic/consumer passes. `refactor3/README.md` is the independent
final no-edit refactor return, with `## REFACTOR COMPLETE` and all production
hashes unchanged. `verification2/pytest.json` and `make.json` each exit 0 with
2427 passed / 7 existing skips on the final bytes. The eight attempt2 comparisons
all pass; accepted-candidate.json pins 107 runtime and 174 test/fixture files.
Original frozen corpora, archives, harnesses, earlier profiles, rejected changes
and unfavorable attempt1 remain recoverable. No production edit followed the
accepted profile/refactor/full-check/paired observations. All final Nix entries
and heavy runs were serial; unrelated host work was preserved.
