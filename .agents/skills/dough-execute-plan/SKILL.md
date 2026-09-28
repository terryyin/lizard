---
name: dough-execute-plan
description: >-
  Executes one selected story or bounded retrospective correction through an
  executable plan, or one authorized planless slice from a selected simple story or
  a contextual instruction, with independent refactoring, delivery, and asynchronous
  CI repair. Use to execute a plan, run slices, execute a canonical story when the
  caller explicitly skips slice planning, or execute a small instruction from
  context without a story or plan. Also admits an accepted mission that no backlog
  list holds, such as a standalone review, investigation, or maintenance request,
  into Taken before its work starts. Does not decide story scope or quick-path
  eligibility. `--trunk` selects Trunk Mode; omitted mode keeps Story Branch Mode.
  `--one-shot` publishes only the result of explicitly selected trivial work.
  `--skip-retro` skips the automatic planned-execution retrospective. `--replan` and
  `--no-replan` choose whether an oversized attempt may continue through planning.
---

# Execute planned or planless work

Execute the selected work through its existing plan. An explicit current instruction
may instead authorize one planless quick slice: an understood canonical feature story,
or a contextual instruction with no story or plan. The coordinator owns delivery;
implementation agents return uncommitted changes.

## Establish execution context

Identify the execution source before changing project state:

- **Planned:** require an executable plan. For a feature story, read its seed section.
  For a bounded correction, require its complete correction input — its story and
  plan, or a plan-homed correction's plan — under [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
- **Quick:** require an explicit current instruction that authorizes planless
  execution. The source is either a canonical feature story with understood goal,
  scope, key examples, and no blocking decision, plus the instruction to skip slice
  planning; or a contextual instruction that supplies the objective, known
  expectations, remaining uncertainty, and authority. A seed alone or apparent story
  size cannot select this path. Corrections require plans. In Story Branch or
  Trunk Mode, first [admit](#admit-accepted-work-that-no-backlog-list-holds) an
  accepted independent mission; otherwise create no story, plan, or queue entry.

Name missing authority or source context and stop before backlog changes, delegation,
or implementation. Successful quick execution keeps its scope in its story, when
admitted, and its decisions, progress, and proof in the conversation; create no plan,
completion note, or substitute record. A contextual instruction may leave expectations
unresolved. Carry its goal and uncertainty, resolve what a behavior change requires,
and allow an evidenced no-change conclusion through the explained-empty-change path.

Use [planning level](../dough-story-refinement/references/planning.md#choose-the-planning-level)
for source ownership, [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof)
when mapping or accepting proof, and [active-plan refinement](../dough-story-refinement/references/planning.md#refine-the-active-plan)
for plan updates. Judge proof during execution; retain completed plans, source
history, and proof decisions for retrospective and
[story wrap-up](../dough-story-wrap-up/SKILL.md). Quick work retains its source,
conversation, and execution identity. Use
[slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
for Behavior/Structure types and [slice sizing](../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices)
for budgets and learning escalation.

At startup, obtain enough authoritative context to select and delegate the first slice.
Reuse instructions while their sources and applicability assumptions remain unchanged.
Resolve project context at the first boundary that needs it:

- execution-source kind, slice target, hard limit, and exceptions;
  [replanning permission](references/execution-decisions.md#choose-replanning-permission);
  integration checkout and branch for a claim, using the project's configured integration branch or `main` when none is supplied;
  execution mode and location: default Story Branch Mode; `--trunk` or a clear
  equivalent selects Trunk Mode; explicit caller selection uses the current
  branch. Resolve contradictions before changing state. Mode never creates
  execution authority; for planned work, plan path and status vocabulary;
- backlog path and selected entry for work selected from **Backlog list**;
- selective formatter and commit hook contract before taking or admitting work and its claim commit;
- navigation, focused tests, runtime wrapper, and workflow precedence for the selected slice;
- authorized push destination before delivery; for Story Branch or Trunk Mode, also
  [trunk publication's Preconditions](references/trunk-publication.md#preconditions) before
  selecting the owned workspace and taking or admitting work, and before publishing a
  claim, validated increment, or owned repair — those preconditions resolve
  publication inputs and defer shared-checkout access and preservation to
  [maintain the default checkout](references/maintain-default-checkout.md);
- generation triggers and commands when affected; and
- [refactor context](../dough-post-change-refactor/SKILL.md) before refactor delegation.

Missing context stops its affected boundary, including first-slice delegation when
needed there. Other invoking tools, including GSD, retain this slice delivery contract;
a phase or task cannot replace a slice.

Before first implementation delegation, read [delegation](references/delegation.md)
and the common reassessment/human-judgment rules, [replanning
permission](references/execution-decisions.md#choose-replanning-permission), plus
currently triggered sections of [execution decisions](references/execution-decisions.md).
Before accepting a return, read [proof acceptance](references/wrap-up.md#accept-proof);
before delivery, read [delivery](references/wrap-up.md#deliver-the-change). Before
arming observation, read [CI observation](references/ci-monitor.md) and only the
current host's notification adapter. Arm from the execution checkout against the
authorized target branch; do not wait for CI. Before creating the execution workspace, read
[execution location](references/execution-location.md). Before a claim,
validated increment, or owned repair publication, read
[trunk publication](references/trunk-publication.md).
Use [targeted retrieval and disposable research](references/disposable-research.md)
for omitted/truncated passages or bounded investigations; another step alone needs no reload.

## Take or admit work

After resolving execution source and authority, inspect the backlog before
plan-status changes, observer startup, delegation, or implementation. Resolve
the selective formatter and claim commit hook contract. An absent or understood
check-only hook permits the transition; an unknown, mutating, failing, or
disputed hook stops with the queue unchanged. Resolve [publication
preconditions](references/trunk-publication.md#preconditions), including the
authorized remote/trunk, before a Story Branch or Trunk Mode startup. Existing
current-branch and host-owned checkout restrictions still apply.

For authorized queued Story Branch or Trunk Mode work, invoke the installed
`scripts/execution-start.mjs start` once with the originating integration
checkout, owned workspace path and branch, selected identity, stable execution
publisher ID, mode (`trunk` or `story-branch`), actual remote and trunk branch,
and the established `--push-authorized --workspace-authorized` flags. Supply
your own `--host` (`claude`, `codex`, or `cursor`) and `--model`; omit either
you cannot state rather than guess. Supply `--plan` as a path relative to the
backlog directory when explicitly selected; the command also resolves the
canonical published plan. Supply `--declared-owner` and matching `--requester`
only when default-checkout access has actually been established. Missing
declarations do not prevent a safe automatic refresh. The command fetches trunk,
checks the published selected source and preparation, selects or reuses the
workspace, names you as an agent, commits an isolated Take that publishes your
agent profile, makes that agent the author of your workspace commits
(`workspaceAuthorship: "not-configured"` means only the Take commit names the
agent), confirms publication on remote trunk, publishes a Story Branch Mode
execution branch at that Take so the branch the profile names exists on the
remote, and reports local refresh separately.

Use its compact one-line result directly; do not filter or fetch it again.
Values you supplied stay in your execution context and are not echoed. An
accepted result (`ok: true`, `status` `published` or `resumed`) carries
`publishedSha`, `startingRevision`, `candidateSha`, `created`, `agent` when the
claim names one, `plan` or `remote` when resolved rather than supplied, and the
default checkout's `maintenance` (`result`, plus `reason` when not refreshed).
Retain them; the first increment's managed delivery uses `publishedSha` as its
previously published base. `existing` (work already Taken under your claim)
writes nothing and returns that claim's `publishedSha`. A deferred or stopped
`maintenance` or an `earlierMaintenance` issue leaves accepted publication
intact. A refusal or unconfirmed result (`ok: false`, non-zero exit) stops
before implementation; report and act on its `status`, `error`, and any
`recovery` or `provenance`, handling `developer-identity-refused` as under
[agent commits](references/agent-commits.md). Inspect current Git state only
when a reported reason needs it; never repeat a mutating command to obtain
diagnostics. If publication is interrupted, invoke the same installed command
with the retained workspace, branch, publisher ID, identity,
`--starting-revision` and `--candidate-sha` from the last result (or its
`recovery`) or confirmed pre-push candidate. Use the latest candidate SHA after
a replay. A `resumed` result confirms current ownership through remote ancestry,
even when trunk has advanced; it may finish eligible local refresh without
another Take or push. A rival or ambiguous provenance stops implementation.
Preserve the stopped candidate and exact recovery fields on an uncertain result.

Every accepted start, new or resumed, then requires this project's
checkout-bound setup and applicable command under [execution
location](references/execution-location.md) before implementation. Setup failure
preserves the accepted claim and workspace. The separate product-backlog Take
tool keeps its local domain purpose; a local Taken entry alone does not satisfy
this startup boundary. Queued current-branch work keeps its existing local Take
contract and gains no publication authority. Leave taken work through pauses,
failures, completion, and retrospective; wrap-up removes it.

### Admit accepted work that no backlog list holds

When the current instruction accepts a mission that no backlog list holds,
follow [admit accepted work](references/admit-accepted-work.md), which admits it
with this start command and `--admit` before its substantive work. Explicitly
selected [one-shot work](references/one-shot.md) starts with `--one-shot` instead.

## Choose the execution location

Follow [execution location](references/execution-location.md) for mode,
workspace creation, project-command readiness, reuse of host-established
preparation for the selected checkout, retained identity, execution resume, push
destination, and checkout-bound runtime. That reference applies the shared
checkout ownership lifecycle for selection, local checkout role, and target
selection.

## Continue or recover at an execution boundary

Planned work uses the existing plan and conversation under
[execution and resume state](../dough-story-refinement/references/planning.md#write-an-executable-plan).
Quick work uses its source and conversation, without a recovery artifact. Retain the
resolved replanning permission with that context. During uninterrupted
work, reuse decisions, accepted proof, and delivery progress while their sources,
assumptions, and covered boundaries hold. After confirmed push, obtain only newly relevant
next-slice detail; a slice transition alone needs no full recovery read.

After interruption or a changed observation from Git, agents, or observer coverage, verify
execution identity first and reconcile only affected worktree/index, branch/commits,
implementation/refactor return, and exact observer identity. Preserve unrelated or ambiguously
owned work. Reuse proof only while promise, boundary, implementation, setup, and observations match.

Resume at the first delivery obligation not established by evidence. An incomplete
or oversized return still needs [oversized-slice handling](references/oversized-slice.md)
before proof acceptance. Otherwise implementation returns still need proof
acceptance/refactoring; completed refactors need remaining delivery;
uncommitted plan edits need staging/commit. Classify an interrupted claim,
increment, or repair with
[interrupted publication](references/trunk-publication.md#resume-an-interrupted-publication)
before any further commit or push. Plan status or a compact report proves none
of those later boundaries. When pushed commit, retained delivery result, and
required registration agree, select the next unfinished slice in plan order.
Missing/contradictory execution identity requires the recovery decision above.

## Execute the next slice

1. In the execution checkout, use the plan's current statuses, decisions, learnings,
   proof, and selected existing-solution finding/evidence when present. Read retained state
   on initial entry/recovery; reuse valid readings during continuation. For quick work,
   reread the source and conversation's scope, decisions, progress, proof, and remaining
   uncertainty; it is the only slice, with no separate state artifact. Confirm current
   execution authority. Recover an existing CI observer before considering a new one.
2. Select the next unfinished planned slice in plan order, or the one quick slice. Apply
   [execution decisions](references/execution-decisions.md); for behavior/state removal or
   disablement, also run the [destructive later-outcome check](references/destructive-later-outcome-check.md).
3. When planned refinement is needed and learning escalation permits, invoke
   [slice-plan refinement](../dough-slice-plan-refinement/SKILL.md) in place, then restart
   at step 1, unless replanning is disabled; then apply
   [oversized-slice decisions](references/oversized-slice.md)
   and stop without retry. If quick work no longer fits one coherent slice, apply
   [oversized-slice decisions](references/oversized-slice.md).
   A no-replan return stops without planning or retry. When replanning is allowed, use
   [ordinary slice planning](../dough-slice-planning/SKILL.md) for remaining work,
   and restart as planned execution. Before
   delegating a change that invalidates a required
   pre-change observation, apply [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof):
   reuse an adequate baseline with known matching revision/environment/selection conditions,
   or obtain it first. Missing/failed prerequisites stop only dependent work. Apply on entry
   and resume without a new startup audit or repeated recovery read. Otherwise delegate
   under [delegation](references/delegation.md).
4. On return, recheck execution decisions; handle incomplete/oversized work there before
   delivery, including a no-replan overrun. Otherwise [accept proof](references/wrap-up.md#accept-proof) and confirm
   uncommitted work or an explained empty change.
5. Run [delivery](references/wrap-up.md#deliver-the-change) end to end. That
   delivery publishes through
   [increment and repair publication](references/trunk-publication.md#publish-an-execution-increment-or-repair).
   After successful delivery, restart for remaining planned slices; a delivered
   quick slice has no successor.

Slices run one at a time in plan order, each finishing its delivery before the next
starts; quick execution has one slice.

## Finish or stop

Follow [finish or stop](references/finish-or-stop.md) for the execution-complete
record, the completion operation, reporting and its markers, automatic
retrospective invocation, and incomplete-work reporting.
