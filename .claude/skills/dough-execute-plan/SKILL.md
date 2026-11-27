---
name: dough-execute-plan
description: >-
  Executes a selected story's plan, a bounded correction, or an authorized
  planless slice, with independent refactoring, delivery, CI repair and
  continuation into wrap-up. Use to execute a plan, run slices, or execute a
  small instruction without a plan.
---

# Execute planned or planless work

The coordinator owns execution and delivery. Implementation agents return
uncommitted changes.

Open a step's references when its prerequisites are met. At startup, establish
source, authority and checkout readiness before loading CI setup. Resolve
applicable observation and select the slice before loading delegation guidance.

Use [targeted retrieval](references/disposable-research.md) for the current
decision. Read applicable headings or bounded ranges that fit the tool output.
Group passages for the same decision, including only the prerequisites they need.
If output cuts off required text, recover that passage with a narrower request
before continuing. Reuse complete passages while their sources and assumptions hold.
If an authoritative input remains unavailable, name it and stop the dependent step.

Use the tools named for the current step when available. When one is missing,
search for the operation and result you need. Inspect plausible tools' descriptions,
inputs and outputs to confirm they provide the required behavior, including any
continuation or notification needed by the workflow.

## Establish execution context

Resolve source and authority before changing project state or delegating.
When there is no Established start, check for an [established session](references/established-session.md).
If the blank-Start handoff names the published story this invocation continues,
use [Continue the published Dashboard admission](references/established-session.md#continue-the-published-dashboard-admission).

- **Planned** work requires an executable plan and the selected story's seed
  section or the [complete correction input](../dough-story-refinement/references/planning.md#choose-the-planning-level).
- **Quick** work requires explicit current authorization to execute one
  planless slice and a source. Use either a canonical story with understood
  goal, scope, key examples and no blocking decision, or a contextual instruction
  with objective, known expectations, remaining uncertainty and authority.
  A seed alone or apparent size cannot select this path. Corrections require plans.
  In Story Branch or Trunk Mode [admit an accepted independent mission](#admit-accepted-work-that-no-backlog-list-holds)
  when no backlog list holds it. Keep the quick scope in its story when
  admitted, and the decisions, progress and proof in the conversation.
  Create no plan, completion note or substitute record. Resolve any remaining
  expectations that a behavior change requires, or return an evidenced no-change result.

Without an established mode, Story Branch Mode is the default. `--trunk` or a
clear equivalent selects Trunk Mode. Explicit caller selection uses the current
branch. Resolve contradictions before changing state. Mode never grants
execution authority.
[Resolve replanning permission](references/execution-decisions.md#choose-replanning-permission)
from `--replan`, `--no-replan` or existing authority. `--skip-retro` skips the
automatic planned-execution retrospective. `--one-shot` keeps selected trivial
work for review, and `--auto-land` selects its automatic landing.

## Take or admit work

When the instruction carries an [established start](references/established-start.md),
retain its identity and accepted publication. Use the named checkout and skip
the start command. Continue at [checkout setup](#choose-the-execution-location).

Otherwise inspect the backlog before changing plan status, starting
observation or delegating. For queued work, resolve its selected entry and
the plan path and status vocabulary. Before taking or admitting work, resolve
the selective formatter and claim commit hook contract. An absent or understood
check-only hook permits the transition. An unknown, mutating, failing or disputed
hook stops with the queue unchanged.

For Story Branch or Trunk Mode, resolve [publication preconditions](references/trunk-publication.md#preconditions),
the originating integration checkout if one exists, the authorized remote and
the configured integration branch (`main` when none is supplied). Resolve
[execution location](references/execution-location.md) for the owned workspace.
If this session holds the story's unlanded Preparing workspace, follow
[the same-session handoff](../dough-story-refinement/references/preparation-journey.md#hand-off-to-execution-in-the-same-session).
Apply the established session's workspace for this invocation as its
[workspace guidance](references/established-session.md#work-in-the-established-workspace) describes.

To take or admit work in Story Branch or Trunk Mode, invoke the installed
`scripts/execution-start.mjs start` once. Supply `--integration` with the
originating integration checkout when it exists. Without one, an existing
owned checkout supplies repository access, or `--repository` supplies an
owned worktree or common Git directory as a read-only source for a new
workspace. Supply the owned workspace and branch, story identity, stable
publisher ID, mode, remote and trunk, and the established
`--push-authorized --workspace-authorized` flags. Give `--host` (`claude`,
`codex` or `cursor`) and `--model` when known. Give `--plan` relative to the
backlog directory when explicitly selected.

The command verifies the published source and preparation, publishes the
isolated Take and agent profile, sets workspace authorship when configured,
and publishes the Story Branch execution branch at the Take. Retain the
[compact start result](references/execution-location.md#read-and-recover-the-start-result)
with publication, local maintenance and recovery coordinates. An uncertain
result stops implementation until that start is recovered.

A local Taken entry alone does not satisfy managed startup. Queued
current-branch work uses its existing local Take contract and gains no
publication authority. Leave taken work through pauses, failures, completion
and retrospective. Wrap-up removes it.

### Admit accepted work that no backlog list holds

Follow [admit accepted work](references/admit-accepted-work.md) and supply
`--admit` to the startup command before substantive work. Explicitly selected
[one-shot work](references/one-shot.md) uses `--one-shot` instead.

## Choose the execution location

Apply [execution location](references/execution-location.md) to select or
verify the checkout and target and run the checkout-bound setup and applicable
project command before implementation. Reuse host preparation only when it
holds for the selected checkout. Setup failure preserves the claim and workspace.

### Arm CI observation

After checkout readiness, follow [runtime setup](references/runtime-setup.md)
to resolve the checkout-bound runtime, CI source and prerequisites. Follow
[observer ownership](references/ci-monitor.md#own-one-observer) to recover or
arm this coordinator's observer and continue without waiting for CI. Observe
the authorized ordinary publication target. In Story Branch Mode this is the
remote execution branch. In Trunk Mode it is the remote integration branch.
Use the adapter for this host at this step.

- **Codex** uses its [yielded observer](references/ci-notify-codex.md).
- **Cursor or Claude Code** uses its [hook-backed observer](references/ci-notify-hosts.md).

At a safe boundary after a notification, [handle it](references/ci-monitor.md#handle-a-notification)
using the current host adapter for ownership, pause and recovery.

## Continue or recover at an execution boundary

Use the existing [plan and execution state](../dough-story-refinement/references/planning.md#write-an-executable-plan)
for planned work. For quick work use its source and conversation. Retain the
resolved replanning permission. Continue with newly relevant detail while
recorded decisions, proof and delivery evidence still apply.

After interruption or changed evidence, verify execution identity first.
Reconcile the affected worktree, index, branch and commits, agent return and
exact observer identity. Preserve unrelated or ambiguously owned work. Reuse
proof while the promise, boundary, implementation, setup and observations match.
On a new coordinator's first slice-clock `start`, pass `--after-ended-session`.
Follow [active-time tracking](references/oversized-slice.md#track-active-time)
for settlement and pause.

Resume at the first unresolved boundary. An incomplete or oversized return
needs [oversized-slice handling](references/oversized-slice.md) before
acceptance. An interrupted claim, increment or repair needs
[publication recovery](references/trunk-publication.md#resume-an-interrupted-publication)
before another commit or push. Use the pushed commit, retained result and
required registration to establish delivery before selecting the next slice.

## Execute the next slice

1. In the execution checkout, read the plan's current status, decisions,
   learnings, proof and selected existing-solution evidence if present, or
   the quick slice's source and retained context. Confirm authority and apply
   [reassessment and limits](references/execution-decisions.md#reassess-before-extending-work)
   and [human-judgment stops](references/execution-decisions.md#stop-for-human-judgment)
   as applicable. Select the next unfinished slice in plan order, or the one
   quick slice. Apply [slice structure and sizing](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
   when reassessing scope.
2. Apply relevant conditions. Before behavior or state removal or disablement,
   run the [destructive later-outcome check](references/destructive-later-outcome-check.md).
   For a runtime failure-path change, apply
   [runtime failure handling](../dough-slice-planning/references/architectural-thinking.md#runtime-failure-handling)
   and carry the intended outcome into the brief. When learning permits,
   [refine the active plan](../dough-slice-plan-refinement/SKILL.md) and reread
   the affected state. Apply [oversized-slice handling](references/oversized-slice.md)
   when the slice no longer fits. Respect a no-replan stop. When authorized,
   use [slice planning](../dough-slice-planning/SKILL.md) for remaining work
   and resume as planned execution.
3. Prepare proof and delegate. Apply [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof)
   before a change invalidates a required baseline. Reuse a matching baseline
   or obtain the necessary observation first. Before choosing test proof or
   authoring tests, use [test entry guidance](references/test-entry-rules.md).
   Apply [expensive-proof selection](../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch)
   before launching it. Resolve project navigation, focused tests, runtime
   wrapper and workflow precedence. List any planned
   [story obligations](references/story-obligations.md#use-the-record-at-execution-boundaries).
   Follow [delegation](references/delegation.md), including
   [proof evidence](references/proof-evidence.md) and the
   [return contract](references/return-report.md). Other invoking tools,
   including GSD, keep this slice contract.
4. On return, recheck execution decisions and handle incomplete or oversized
   work before acceptance. In planned Story Branch Mode
   [settle freshness](references/deliver-the-change.md#settle-freshness-before-dependent-work)
   and [check named siblings](references/deliver-the-change.md#check-named-siblings).
   Then [accept proof](references/wrap-up.md#accept-proof) against the
   return contract and confirm uncommitted work or an explained empty change.
5. Run [delivery](references/deliver-the-change.md#deliver-the-change),
   including independent [refactor context](../dough-post-change-refactor/SKILL.md)
   when required. Resolve triggered generation, selective formatting, hook
   contract, authority and [publication preconditions](references/trunk-publication.md#preconditions)
   when applicable. For planned work
   [update the active plan](../dough-story-refinement/references/planning.md#refine-the-active-plan)
   and check its [story obligations](references/story-obligations.md#use-the-record-at-execution-boundaries)
   at delivery. Use [guarded fresh delivery](references/fresh-increment.md#publish)
   for a validated [increment or owned repair](references/trunk-publication.md#publish-an-execution-increment-or-repair)
   and follow the applicable [candidate publication](references/trunk-publication.md#publish-the-candidate).
   Finish delivery before selecting the next planned slice. Quick work has no successor.

## Finish or stop

Apply [finish or stop](references/finish-or-stop.md) for normal completion,
the automatic retrospective, attention-only reporting and conditional
continuation into [Story Wrap Up](../dough-story-wrap-up/SKILL.md), or
incomplete-work reporting. Retain source, completed plan and accepted proof
for review and wrap-up. Check planned [story obligations](references/story-obligations.md#use-the-record-at-execution-boundaries)
and [Story Branch source freshness](references/source-freshness.md) where
required. When completion is due, use the
[applicable CI revision and shutdown operation](references/ci-completion-wait.md#await-the-applicable-revision-at-completion).
