# Prepare records in an owned workspace

Apply this rule in [dough-story-decomposition](../../dough-story-decomposition/SKILL.md),
[dough-story-refinement](../SKILL.md) itself,
[dough-slice-planning](../../dough-slice-planning/SKILL.md), and
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md)
before any of them writes a seed, story, or plan record, including a small,
already-decided correction.
[dough-execution-retrospective](../../dough-execution-retrospective/SKILL.md#write-only-in-an-owned-checkout)
applies it too before writing a process-finding or correction story and plan record when
no invoking execution supplies its checkout. Reading, discussing, answering
questions, or reviewing an existing record needs no workspace at all.

Once a write is done, [decide what happens to the written
result](preparation-disposition.md) is the only disposition for that record.
Decomposition, refinement, planning, and plan refinement all use it for keep,
leaving the result unpublished, or discarding it. None of them publishes by
another path. Then close or retain the workspace below.

## Determine whether a write needs a workspace

Only the first write to a record named above in this preparation session
requires an owned workspace. Continue ordinary discussion, inspection,
and question-answering without one. Once a write is about to happen, establish
or confirm the workspace immediately before making it.

## Select or reuse the workspace

When your instruction carries an established preparation, use its workspace and
skip this selection, as [established preparation](established-preparation.md)
says.

Apply [own a temporary exploration workspace](../../dough-manual-testing/references/exploration-workspace.md)
"Select the checkout", "Record local checkout role and target selection",
and "Use and resume it" as this preparation's Git lifecycle; do not duplicate
its recipe here. First check whether the current story, active plan, session,
or a host-supplied workspace already owns a suitable checkout for this
preparation. Use it, and do not create a nested or per-invocation workspace
merely because a different skill named above is now writing.

When no suitable owned workspace exists, start one using that reference's
create step, in the repository of a suitable existing host workspace when one
is available, otherwise of the checkout this preparation was invoked from. Its
verified base is the authorized remote target, freshly fetched (for example,
`git fetch <remote>`, then `<remote>/<trunk branch>`), so the draft starts from
published history however stale, divergent, or dirty the invoking checkout is.
That checkout's commits, staged content, and edits stay where they are and
enter the draft only when the developer explicitly supplies them as
preparation input. For an existing queued story, the announcement command in
[Announce the preparation assignment](#announce-the-preparation-assignment)
makes this selection: give it the new workspace path and branch instead of
creating the workspace yourself.

Verify a candidate against that reference before writing into it. The suitable
owner is the current story, plan, session, or host. An unverifiable or
ambiguous match is a missing workspace.

Resolve this project's own conventions for the write — seed directory and
ID/filename rules, plan root and layout, required metadata, and installed
skill guidance — from the intended, owned checkout, not from wherever the
invocation started.

Record local checkout role and target selection through that reference, using
the actual established paths. The owned workspace path is the preparation
workspace. The integration checkout path is the checkout this preparation was
invoked from, or a reused host workspace's already-recorded integration
checkout — the project's established checkout for ordinary work, never the
owned preparation workspace itself. When no such checkout exists, record none.
Target selection is the authorized remote target, recorded separately from that
path. A later keep decision publishes
onto this recorded target; see
[Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result).
Preparation's continuation after this selection is the record write and that
disposition. It does not apply execution mode or project-command readiness.

## Announce the preparation assignment

For an existing queued story, announce it as **Preparing** before its first
record write, either after selecting the workspace or as the step that creates
a new one, and keep that assignment through pauses, under
[Publish the preparation assignment](preparation-assignment.md). An explicit
instruction not to publish or commit means announcing nothing. An explicitly
selected [one-shot refinement](one-shot-refinement.md) establishes the
workspace with its own start instead and announces nothing.

## Continue related preparation

Reuse the same workspace across decomposition, refinement, planning, and
plan refinement while they continue the same story, plan, or session,
including across discussion, a pause for a developer's answer, and successive
invocations of any of the four skills above. Do not start a second workspace
for continuation work that could reuse the first.

## Stop only the write that needs it

When ownership cannot be established or stays ambiguous after the checks
above, stop only the pending record write and report the exact gap: what was
checked and what remains unresolved. Reading, discussing, or continuing to
answer questions does not require resolving it first. Do not invent or guess a
workspace to avoid reporting the gap.

## Pause and resume a preparation session

Pausing for a developer's answer, ending a conversation turn, or any other
interruption before a keep or discard decision leaves the draft exactly as it
is in its owned workspace: nothing is committed, published, or discarded
merely by pausing. See
[Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result)
for what only an explicit instruction can trigger.

On resume, before continuing to write into the workspace, apply [own a
temporary exploration workspace](../../dough-manual-testing/references/exploration-workspace.md)
"Use and resume it" — the same verification
[Select or reuse the workspace](#select-or-reuse-the-workspace) above already
requires before any write. Resuming after a pause is one more trigger for it,
not a different check.

If that verification finds partial prior setup, an identity mismatch, or an
otherwise ambiguous match, apply
[Stop only the write that needs it](#stop-only-the-write-that-needs-it)
above: stop only the resumed write and preserve every resource exactly as
found. Do not silently replace the workspace, create a second one alongside
it, or guess which candidate is the right one — the same rule that already
governs an unresolved first-time workspace selection.

This resume verification relies only on the local checkout role already
recorded when the workspace was selected or created — the actual paths and
story, plan, session, or host ownership. Do not add a session registry, log,
or other persistent index to track preparation sessions across time; resume
continues to depend purely on verifying that recorded role against actual
Git state.

## Tiny corrections are included; the Taken transition is not

A small or already-decided drafting correction — for example fixing a typo or
a misordered example while refining a seed or plan — still goes through this
workspace rule. It gains no direct-edit exception on a shared or host checkout
merely because it is small or already decided.

This rule does not apply to [take or admit work](../../dough-execute-plan/SKILL.md#take-or-admit-work)'s
own commit moving a backlog entry to **Taken**. That execution-startup
transition keeps its existing location, timing, and authority; none of the
four preparation skills above route it through this reference or change its
behavior.

## Leave the shared checkout free for other writers

Preparation under this reference never holds an integration turn on a shared
or host checkout, unlike delivery. A second writer may fetch, integrate, and
push their own prepared or published increment onto the shared integration
branch at any time, including while this preparation is mid-question. Nothing
in this reference locks, blocks, or reserves that checkout.

When the write is finished, apply [decide what happens to the written
result](preparation-disposition.md#decide-what-happens-to-the-written-result)
before [closing or retaining the workspace](#close-or-retain-the-workspace)
below.

## Close or retain the workspace

Cleanup runs only after one of these decisions for this preparation's draft
under [Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result)
is actually **confirmed**, never merely attempted or merely because the
session is ending:

- a **keep** whose landed SHA the fetched authorized remote target contains,
  per
  [Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result).
  The default checkout need not match that SHA. No refresh
  [result](../../dough-execute-plan/references/maintain-default-checkout.md#independent-maintenance-outcome)
  withholds this confirmation;
- an explicit **discard** that actually removed the identified draft under
  [Discard an identified draft](preparation-disposition.md#discard-an-identified-draft),
  not one that stopped because the content could not be unambiguously
  isolated; or
- an explicit **no-publish** instruction under [Decide what happens to the
  written result](preparation-disposition.md#decide-what-happens-to-the-written-result)
  given together with the developer's explicit confirmation that this
  preparation session itself is finished, not merely paused for later
  resumption. An ordinary no-publish instruction on its own leaves the
  session resumable under [Pause and resume a preparation
  session](#pause-and-resume-a-preparation-session) above and confirms no
  disposition for cleanup purposes.

Failed or unconfirmed publication never triggers cleanup. A keep whose
landing stopped before the authorized remote contains its SHA is not a
confirmed disposition merely because the session is ending. Treat it as still
unresolved and preserve every resource exactly as found under
[preserve pending local work](../../dough-execute-plan/references/maintain-default-checkout.md#preserve-pending-local-work),
including a pending human edit on the default checkout. Pausing, going quiet,
or ending the conversation before a decision is confirmed is never itself a
trigger, exactly as it is never itself a keep or discard decision.

A workspace whose assignment is still published is retained until the
assignment ends, whatever else applies: it is what identifies that
assignment, so keep it or
[abandon the preparation](preparation-assignment.md#abandon-the-preparation)
before retiring it. If it was lost anyway, the assignment ends only as
[Release a lost workspace's assignment](preparation-lost-workspace.md)
describes.

Once a confirmed disposition applies, retire the workspace under Dough Land's
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree): a keep
already did so as part of its landing, and a confirmed discard or finished
no-publish applies the same rule. Which workspace this preparation removes
follows the shared lifecycle's
[Close or retain it](../../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it),
applied to the identity recorded in [Select or reuse the
workspace](#select-or-reuse-the-workspace). State any retained
workspace's path, branch, and reason alongside, not instead of, any
disposition report already owed to the developer.

Preparation content that never reached a confirmed keep — still
isolated in an owned workspace, discarded, or left unpublished by a
confirmed no-publish/session-finished instruction — has no presence in any
progress view this project derives only from published remote state (an
origin-only dashboard, where one exists): that view reflects what reached
the authorized remote target, such as a published preparation assignment, not
what a preparation session still holds locally, exactly as an unpublished
Taken claim stays invisible to it. Report that gap explicitly rather than
letting local absence from such a view read as lost or completed work.

This composes with, and does not replace or weaken, [own a temporary
exploration workspace](../../dough-manual-testing/references/exploration-workspace.md)'s
own close/retain criteria; manual testing and bug fixing keep relying on
that reference's behavior unchanged.
