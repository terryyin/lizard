---
name: dough-execute-plan
description: >-
  Executes one selected story or bounded retrospective correction through an
  executable plan, or an explicitly selected simple story as one quick slice
  without a plan, with independent refactoring, delivery, and asynchronous CI
  repair. Use to execute a plan, run slices, or execute a canonical story when
  the caller explicitly skips slice planning. Does not decide story scope or
  quick-path eligibility. `--skip-retro` skips the automatic planned-execution retrospective.
---

# Execute planned or quick story work

Execute the selected work through its existing plan. An explicit current instruction
may instead select an understood canonical feature story as one planless quick slice.
The coordinator owns delivery; implementation agents return uncommitted changes.

## Establish execution context

Identify the execution source before changing project state:

- **Planned:** require an executable plan. For a feature story, read its seed section.
  For a bounded correction, require the complete correction input in the plan under
  [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level);
  do not require or create a seed.
- **Quick:** require an explicit current instruction to skip slice planning and the
  canonical feature story with understood goal, scope, key examples, and no blocking
  decision. The instruction authorizes execution; the story supplies the slice contract.
  A seed alone or apparent story size cannot select this path. Corrections require plans.

Name missing authority or source context and stop before backlog changes, delegation,
or implementation. Successful quick execution keeps scope, decisions, progress, and
proof in the conversation; create no plan, completion note, or substitute record.

Use [planning level](../dough-story-refinement/references/planning.md#choose-the-planning-level)
for source ownership, [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof)
when mapping or accepting proof, and [active-plan refinement](../dough-story-refinement/references/planning.md#refine-the-active-plan)
for plan updates. Retain completed plans, source history, and review evidence for
retrospective and [story wrap-up](../dough-story-wrap-up/SKILL.md); quick work retains
its story and conversation. Use [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
for Behavior/Structure types and [slice sizing](../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices)
for budgets and learning escalation.

At startup, obtain enough authoritative context to select and delegate the first slice.
Reuse instructions while their sources and applicability assumptions remain unchanged.
Resolve project context at the first boundary that needs it:

- execution-source kind, slice target, hard limit, and exceptions; for planned work,
  plan path, status vocabulary, and Story Branch Mode or caller-selected current branch;
- backlog path and selected entry for work selected from **Backlog list**;
- selective formatter and commit hook contract before taking queued work and its claim commit;
- navigation, focused tests, runtime wrapper, and workflow precedence for the selected slice;
- authorized push destination before delivery;
- generation triggers and commands when affected; and
- [refactor context](../dough-post-change-refactor/SKILL.md) before refactor delegation.

Missing context stops its affected boundary, including first-slice delegation when
needed there. Other invoking tools, including GSD, retain this slice delivery contract;
a phase or task cannot replace a slice.

Before first implementation delegation, read [delegation](references/delegation.md)
and the common reassessment/human-judgment rules plus currently triggered sections of
[execution decisions](references/execution-decisions.md). Before accepting a return,
read [proof acceptance](references/wrap-up.md#accept-proof); before delivery, read
[delivery](references/wrap-up.md#deliver-the-change). Before first push, read
[CI observation](references/ci-monitor.md) and only the current host's notification adapter.
Use [targeted retrieval and disposable research](references/disposable-research.md)
for omitted/truncated passages or bounded investigations; another step alone needs no reload.

## Take queued work

If an authorized Git operation reports conflicts, inspect the unmerged paths
for this project's product backlog (often `PRODUCT-BACKLOG.md`). Before editing
or staging its resolution, read and apply
[backlog merge conflicts](../dough-product-backlog/references/merge-conflicts.md),
including its staged-result verification before continuing the Git operation.
If the reference is unavailable, preserve the conflict and report the missing guidance.

After resolving execution source and authority, inspect the backlog before plan-status
changes, observer recovery/startup, delegation, or implementation. Moving a selected
**Backlog list** entry to **Taken** is execution's first project-state change.

Before moving it, resolve selective formatting and the Taken-only commit's hook contract.
An absent or understood check-only hook permits the transition. An unknown, mutating,
failing, or disputed hook stops it with the queue unchanged until safely resolved through
execution decisions. Resolution runs neither delivery formatting nor hook-owned lint;
obtain push/CI context only when another current boundary needs it. Then follow
[take queued work](../dough-product-backlog/SKILL.md#take-queued-work-for-execution).
An ambiguous move stops implementation.

Already **Taken** means resume: preserve its position without duplication. Work absent
from both active lists needs no fabricated entry. Planning/refinement never takes work.
Leave taken work through pauses, failures, completion, and retrospective; wrap-up removes it.

For planned Story Branch Mode, preflight read-only in the originating checkout before
moving the entry: verify branch, backlog path, tracked/staged changes, and ownership of
an isolated claim commit. Ambiguous branch or ownership leaves the queue unchanged;
preserve existing work without stashing, resetting, overwriting, or silently unstaging it.

After moving, stage only the backlog path, inspect the staged diff, and commit the claim
locally on the originating branch. Create the execution branch/worktree from that commit
only after success; do not push the claim separately. Staging/commit failure stops isolated
execution: preserve and report backlog/index state. Later setup failure leaves the committed
entry **Taken** for retry. No-change cases produce no empty claim commit.

## Choose the execution location

Planned work defaults to Story Branch Mode: one execution branch and Git worktree for
the selected story/correction. Explicit caller selection uses the current branch instead.
After committing a claim, create the branch/worktree from it before delegation; when no
claim applies, use verified current HEAD. Resolve names and safe location from project
conventions and ordinary host Git facilities. Missing conventions, unsafe location, or
creation failure stops setup; preserve and report the claim and created resources.
Use no parallel registry, configuration format, or worktree manager.

After successful setup and before delegation, retain one planned-execution identity in
the existing plan and conversation:

- originating checkout and branch, where the claim was recorded;
- execution checkout and branch for implementation and delivery;
- caller/project integration target, defaulting to `main` only when neither supplies one.

Caller-selected current-branch work records that checkout/branch for both locations and
creates no worktree. Quick execution also stays in the current checkout/branch.

On resume, verify retained identity against actual branch, HEAD ancestry, and worktree
state; **Taken** alone supplies no location. Reuse a matching execution checkout. Missing,
ambiguous, contradictory, unsafe, or partial identity/setup requires an exact recovery
decision: preserve resources rather than guessing, nesting worktrees, or switching branches.

Run delegation, refactoring, generation, formatting, staging, commits, pushes, and CI repair
from the selected execution location; Story Branch Mode pushes its execution branch to the
authorized destination. Pass identity/location explicitly to agents and host adapters.
For quick work, pass the current checkout/branch retained in conversation.

Resolve checkout-bound installed runtime from the selected execution checkout and use it
as working directory. Before arming, apply [runtime setup](references/runtime-setup.md)
identity and stop rules; the initially loaded skill's copy is not a fallback.

## Continue or recover at an execution boundary

Planned work uses the existing plan and conversation under
[execution and resume state](../dough-story-refinement/references/planning.md#write-an-executable-plan).
Quick work uses its story/conversation, without a recovery artifact. During uninterrupted
work, reuse decisions, accepted proof, and delivery progress while their sources,
assumptions, and covered boundaries hold. After confirmed push, obtain only newly relevant
next-slice detail; a slice transition alone needs no full recovery read.

After interruption or a changed observation from Git, agents, or observer coverage, verify
execution identity first and reconcile only affected worktree/index, branch/commits,
implementation/refactor return, and exact observer identity. Preserve unrelated or ambiguously
owned work. Reuse proof only while promise, boundary, implementation, setup, and observations match.

Resume at the first delivery obligation not established by evidence: implementation returns
still need proof acceptance/refactoring; completed refactors need remaining delivery;
uncommitted plan edits need staging/commit; local commits absent from the authorized
destination need push. Plan status or a compact report proves none of those later boundaries.
When pushed commit and retained delivery result agree, select the next dependency-ready slice.
Missing/contradictory execution identity requires the recovery decision above.

## Execute the next slice

1. In the execution checkout, use the plan's current statuses, decisions, learnings,
   proof, and selected existing-solution finding/evidence when present. Read retained state
   on initial entry/recovery; reuse valid readings during continuation. For quick work,
   reread the story and conversation's scope, decisions, progress, and proof; it is the only
   slice, with no separate state artifact. Confirm current execution authority. Recover
   an existing CI observer before considering a new one.
2. Select the next unfinished dependency-ready planned slice, or the quick story. Apply
   [execution decisions](references/execution-decisions.md); for behavior/state removal or
   disablement, also run the [destructive later-outcome check](references/destructive-later-outcome-check.md).
3. When planned refinement is needed and learning escalation permits, invoke
   [slice-plan refinement](../dough-slice-plan-refinement/SKILL.md) in place, then restart
   at step 1. If quick work no longer fits one coherent slice, safely stop the attempt under
   [oversized-slice decisions](references/execution-decisions.md#refine-an-oversized-slice),
   use [ordinary slice planning](../dough-slice-planning/SKILL.md) for remaining work,
   and restart as planned execution. Before delegating a change that invalidates a required
   pre-change observation, apply [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof):
   reuse an adequate baseline with known matching revision/environment/selection conditions,
   or obtain it first. Missing/failed prerequisites stop only dependent work. Apply on entry
   and resume without a new startup audit or repeated recovery read. Otherwise delegate
   under [delegation](references/delegation.md).
4. On return, recheck execution decisions; handle incomplete/oversized work there before
   delivery. Otherwise [accept proof](references/wrap-up.md#accept-proof) and confirm
   uncommitted work or an explained empty change.
5. Run [delivery](references/wrap-up.md#deliver-the-change) end to end. After successful
   push, restart for remaining planned slices; a delivered quick slice has no successor.

Planned slices may run concurrently only with disjoint file changes, mutable state, and
plan writes. Quick execution has one slice. Each slice completes coordinator-owned delivery
before a dependent slice starts.

## Finish or stop

On completion, human-judgment stop, or cancellation, close the observer through the current
host adapter: handle delivered failures, then stop observers without waiting for CI. Report pending CI
as unobserved.

After all planned slices satisfy proof/delivery and required observer shutdown succeeds,
report completion, retained evidence, and CI limitations. `--skip-retro` skips only this
execution's automatic retrospective; it changes no preferences or proof/delivery/shutdown
obligations. Explicit omit/defer instructions also take precedence. When skipped, report it
and end with `## PLAN EXECUTION COMPLETE`, retaining plan/evidence for later review and wrap-up.

Otherwise report `## PLAN EXECUTION COMPLETE`, then invoke
[dough-execution-retrospective](../dough-execution-retrospective/SKILL.md) without another
confirmation. Preserve explicit review instructions and project preferences through its
review selection; its authority excludes implementing findings or changing the backlog.

Continue in the recorded execution project/checkout. Supply available references/context:
source contract, original plan and approved changes, attributable commits, decisions, proof,
delivery state, CI limitations, and checkout/branch identity. Include an initial quick attempt
and its planned continuation as one execution. Reuse context without another handoff artifact
or transcript copy; retrospective validates attribution and recovers real gaps.

Execution completion and review completion are distinct. A retrospective context stop leaves
execution complete: report missing input without rerunning implementation or claiming review
completion. On recovery, use retained review state to continue or recognize completed review;
ambiguous state requires recovery rather than duplicate review or guessed completion.

Retain the completed plan, evidence, execution checkout, branch, and worktree for story
wrap-up; do not invoke it here. Wholly planless quick completion retains story/conversation,
reports delivered work and shutdown, and ends with `## QUICK EXECUTION COMPLETE` after
required delivery/shutdown, without automatic retrospective.

For incomplete work, failed delivery/shutdown, cancellation, or a human-judgment stop, report
source, active plan/next slice or story/quick-slice state, preserved work, observer state,
and required decision/recovery action. Emit no completion marker or automatic retrospective.
