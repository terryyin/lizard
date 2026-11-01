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
- PFE: `JavaScriptBindings` already owns binding frames/defaults/formals;
  `TypeScriptStates` owns parser dispatch; object/function/signature mixins
  own their syntax. Extend these owners. Do not add another bracket counter,
  whole-stream buffering, per-token wrapper or PR-specific switch.
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

## Ordered slices

### 1. Keep nested expression brackets with their binding reader
Type: Behavior
Status: planned
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
Status: planned
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

## Learnings

None beyond the settled premises above.
