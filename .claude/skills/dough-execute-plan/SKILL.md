---
name: dough-execute-plan
description: >-
  Executes one selected story or bounded retrospective correction through an
  executable plan, or one authorized planless slice from a selected simple story or
  a contextual instruction, with independent refactoring, delivery, and asynchronous
  CI repair. Continues settled planned work into Story Wrap Up. Use to execute a
  plan, run slices, execute a canonical story when the caller explicitly skips
  slice planning, or execute a small instruction from
  context without a story or plan. Also admits an accepted mission that no backlog
  list holds, such as a standalone review, investigation, or maintenance request,
  into Taken before its work starts. Does not decide story scope or quick-path
  eligibility. `--trunk` selects Trunk Mode; omitted mode keeps Story Branch Mode.
  `--one-shot` keeps selected trivial work's result for review; `--auto-land` lands it.
  `--skip-retro` skips the automatic planned-execution retrospective. `--replan` and
  `--no-replan` choose whether an oversized attempt may continue through planning.
---

# Execute planned or planless work

Execute the selected plan or one explicitly authorized planless quick slice from an
understood canonical feature story or contextual instruction. The coordinator owns
delivery; implementation agents return uncommitted changes.

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
  integration checkout, when one exists, and branch for a claim, using the project's configured integration branch or `main` when none is supplied;
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
  publication inputs and defer shared-checkout preservation to
  [maintain the default checkout](references/maintain-default-checkout.md);
- generation triggers and commands when affected; and
- [refactor context](../dough-post-change-refactor/SKILL.md) before refactor delegation.

Missing context stops its affected boundary, including first-slice delegation when
needed there. Other invoking tools, including GSD, retain this slice delivery contract;
a phase or task cannot replace a slice.

## Read guidance at each boundary

Read each row when its boundary is reached, never earlier, and as its own tool
result, never combined with another row. On a host that reads this skill body as
a tool result, the body is its own read. Each row fits one tool result; a row
already read needs no reload while its sources stay unchanged. When a passage is
truncated or absent, follow [targeted retrieval](references/disposable-research.md)
for that passage alone.

| Boundary | Reads |
| --- | --- |
| Skill body, on a host that reads it as a tool result | [this skill](SKILL.md) |
| An established start your instruction carries | [established start](references/established-start.md) |
| Before creating the execution workspace | [execution location](references/execution-location.md); when CI setup applies, [runtime setup](references/runtime-setup.md) |
| On Codex, arming observation at execution start | [own one observer](references/ci-monitor.md#own-one-observer), [Codex adapter](references/ci-notify-codex.md), [runtime setup](references/runtime-setup.md) |
| Before first implementation delegation | [delegation](references/delegation.md), [reassessment](references/execution-decisions.md#reassess-before-extending-work), [human judgment](references/execution-decisions.md#stop-for-human-judgment), [replanning permission](references/execution-decisions.md#choose-replanning-permission), [targeted retrieval](references/disposable-research.md), [story-obligation inputs](references/story-obligations.md#use-the-record-at-execution-boundaries) |
| Before accepting a return | [proof acceptance](references/wrap-up.md#accept-proof) |
| Before delivery, or a validated increment or owned repair publication | [delivery](references/wrap-up.md#deliver-the-change), [fresh increment](references/fresh-increment.md), [increment publication](references/trunk-publication.md#publish-an-execution-increment-or-repair), [publish the candidate](references/trunk-publication.md#publish-the-candidate), [story-obligation inputs](references/story-obligations.md#use-the-record-at-execution-boundaries) |
| Arming observation on other hosts | [CI observation](references/ci-monitor.md), [Cursor and Claude Code adapters](references/ci-notify-hosts.md) |
| Handling a CI notification | [handle a notification](references/ci-monitor.md#handle-a-notification), with only the current host's adapter: [Codex](references/ci-notify-codex.md) or [Cursor and Claude Code](references/ci-notify-hosts.md) |
| Before finishing or stopping | [finish or stop](references/finish-or-stop.md), [await the applicable revision](references/ci-monitor.md#await-the-applicable-revision-at-completion), [completion mechanics](references/ci-completion-wait.md), [story-obligation inputs](references/story-obligations.md#use-the-record-at-execution-boundaries), in Story Branch Mode [source freshness](references/source-freshness.md) |

Read other [execution decisions](references/execution-decisions.md) sections when
they trigger. Arm from the execution checkout against the authorized target
branch; do not wait for CI.

## Take or admit work

After resolving execution source and authority, inspect the backlog before
plan-status changes, observer startup, delegation, or implementation. Resolve the
selective formatter and claim commit hook contract. An absent or understood
check-only hook permits the transition; an unknown, mutating, failing, or disputed
hook stops with the queue unchanged. Resolve [publication
preconditions](references/trunk-publication.md#preconditions), including the
authorized remote/trunk, before a Story Branch or Trunk Mode startup that publishes
a claim. Existing current-branch and host-owned checkout restrictions still apply.

When your instruction carries an established start, follow [established start](references/established-start.md) instead of the start command below.
When your session holds this story's Preparing workspace with unlanded preparation, first follow
[hand off to execution in the same session](../dough-story-refinement/references/preparation-journey.md#hand-off-to-execution-in-the-same-session) before that command.
When your session's first message carried an established session, give that command the workspace and branch [established session](references/established-session.md) decides.

For authorized queued Story Branch or Trunk Mode work, invoke the installed
`scripts/execution-start.mjs start` once with the originating integration
checkout as `--integration` when one exists. Without one, an existing owned
workspace supplies repository access; to create a new workspace, supply an
owned worktree of the repository or its common Git directory as
`--repository`, which is only read from, never refreshed. Also supply the
owned workspace path and branch, selected identity, stable execution publisher
ID, mode (`trunk` or `story-branch`), actual remote and trunk branch, and the
established `--push-authorized --workspace-authorized` flags. Supply your own `--host` (`claude`, `codex`, or
`cursor`) and `--model`; omit either you cannot state rather than guess. Supply
`--plan` as a path relative to the backlog directory when explicitly selected;
the command also resolves the canonical published plan. The command fetches trunk,
checks the published selected source and preparation, selects the workspace or reuses it under [refresh
eligibility](references/maintain-default-checkout.md#refresh-eligibility), names you as an agent,
commits an isolated Take that publishes your agent profile, makes that agent the
author of your workspace commits (`workspaceAuthorship: "not-configured"` means
only the Take commit names the agent), confirms publication on remote trunk,
publishes a Story Branch Mode execution branch at that Take so the branch the
profile names exists on the remote, and reports local refresh separately.

Use the [compact start result](references/execution-location.md#read-and-recover-the-start-result)
to retain publication, local maintenance, and recovery coordinates. An uncertain
result stops implementation; resume that retained start rather than publishing
another claim.

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

Follow [execution location](references/execution-location.md) for mode, workspace
creation, project-command readiness, reuse of host-established preparation for the
selected checkout, retained identity, execution resume, push destination, and
checkout-bound runtime. That reference applies the shared checkout ownership
lifecycle for selection, local checkout role, and target selection.

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
   and restart as planned execution. Before delegating a change that invalidates a required
   pre-change observation, apply [proof ownership](../dough-story-refinement/references/planning.md#own-executable-proof):
   reuse an adequate baseline with known matching revision/environment/selection conditions,
   or obtain it first. Missing/failed prerequisites stop only dependent work. Apply on entry
   and resume without a new startup audit or repeated recovery read. Before expensive proof, apply
   [choose expensive proof before launch](../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch). Then delegate under [delegation](references/delegation.md).
4. On return, recheck execution decisions; handle incomplete/oversized work there before
   delivery, including a no-replan overrun. Otherwise [accept proof](references/wrap-up.md#accept-proof) and confirm
   uncommitted work or an explained empty change.
5. Run [delivery](references/wrap-up.md#deliver-the-change) end to end. That
   delivery publishes through
   [increment and repair publication](references/trunk-publication.md#publish-an-execution-increment-or-repair).
   After successful delivery, restart for the next planned slice, one at a time:
   each finishes its delivery before the next starts; a quick slice has no successor.

## Finish or stop

Follow [finish or stop](references/finish-or-stop.md) for the execution-complete record,
completion operation, automatic retrospective, conditional planned continuation into
Story Wrap Up, attention-only handoff reporting, and incomplete-work reporting.
