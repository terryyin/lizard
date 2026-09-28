# Decide, publish, or discard the written result

Apply this reference once [prepare records in an owned
workspace](preparation-workspace.md) has established this preparation's owned
workspace and recorded its local checkout role and target selection in
that reference's [Select or reuse the
workspace](preparation-workspace.md#select-or-reuse-the-workspace). It covers
only what happens to an already-written seed, story, plan, bug-triage
record, or standalone retrospective record. A bug-triage record is the
authorized canonical story, executable plan, or backlog change left after
disposable reproduction changes are removed. Unrelated exploration content
is not that record. After applying it, the calling skill returns to [Close or
retain the workspace](preparation-workspace.md#close-or-retain-the-workspace). Bug
fixing supplies the explicit keep, leave-unpublished, or discard instruction
and does not choose the backlog home here.

## Decide what happens to the written result

Leave a written seed, story, plan, or bug-triage record in the owned
workspace for the developer's review by default. Do not commit it to a
shared or host checkout or publish it merely because the write finished.
Treat a "quick" or already-decided edit the same way — it is not
authorization to skip this step.
These explicit developer decisions change that default:

- **An explicit instruction to keep this preparation's retained result**
  authorizes landing it from the owned workspace onto the authorized remote
  target through
  [Keep and publish the retained result](#keep-and-publish-the-retained-result)
  below. Only an explicit instruction counts as keep: continuing discussion,
  pausing for more review, or silence is never a keep decision.
- **An explicit instruction to leave the result unpublished** is preserved and
  overrides any default publication. The record stays in the owned workspace
  exactly as the developer left it; this reference performs no additional
  commit or push. Like a pause, it keeps any published preparation assignment:
  others still see the story as Preparing until a keep or an abandonment ends
  it. Say so when reporting.
- **An explicit instruction to discard an identified draft** removes that
  specific session-owned content, under
  [Discard an identified draft](#discard-an-identified-draft) below. The same
  rule that governs keep governs discard: only an explicit instruction that
  identifies what to discard counts. Continuing discussion, pausing, going
  quiet, or the session simply ending is never a discard decision, exactly as
  none of those is ever a keep decision. Discarding a draft does not by itself
  end a published preparation assignment.
- **An explicit instruction to abandon preparing the story** (stop preparing
  it, give it up, or discard this preparation as a whole) ends this session's
  published assignment under
  [Abandon the preparation](preparation-assignment.md#abandon-the-preparation).
  It keeps the story queued and the draft recoverable; discarding the draft
  too needs its own identification, as above.

Absent an explicit instruction, continue leaving the draft isolated: no
commit, integration, publication, or removal happens under this reference.
The draft stays recoverable in the owned workspace, and the result states
that pending disposition.

## Inspect an advanced integration target without deciding

A resumed (or still-active) preparation session may fetch or inspect the
current state of the recorded integration checkout and the authorized remote
target in
[Select or reuse the
workspace](preparation-workspace.md#select-or-reuse-the-workspace) — to see
what changed while paused, or to inform a discussion with the developer —
without that inspection itself becoming an integration or a keep decision.
Looking is not deciding: only an explicit keep instruction, validated under
[Validate a keep instruction before acting](#validate-a-keep-instruction-before-acting)
immediately below, enters
[Keep and publish the retained result](#keep-and-publish-the-retained-result).
"Let me check what's on the integration target now" is inspection for
discussion, not the start of that sequence — do not treat it as one.

## Validate a keep instruction before acting

Before committing or publishing anything, confirm — or require the calling
skill (one of the four preparation skills [prepare records in an owned
workspace](preparation-workspace.md) applies to, bug fixing for a bug-triage
record, per its disposition/report step, or a standalone execution
retrospective for its process-finding or correction story and plan record) to have
already confirmed — that the keep instruction:

- names this session's own retained seed, story, plan, bug-triage record, or
  retrospective record, not implementation, unrelated exploration content,
  disposable reproduction changes, or another session's work;
- applies to an owned workspace that holds nothing else, committed or
  uncommitted, beyond the workspace's recorded starting revision and the
  fetched authorized remote target. This session's own preparation
  announcement and its staged release belong to the result. Landing commits
  and publishes everything in that workspace, so other content there —
  another session's draft, unrelated exploration content, or disposable
  reproduction changes — stops the keep. Name that content; the developer removes it, discards it, or
  confirms it belongs in the landing;
  and
- has a known, unambiguous local checkout role and a separate target
  selection, per the record in
  [Select or reuse the workspace](preparation-workspace.md#select-or-reuse-the-workspace).
  Publish onto the authorized remote target. Do not treat the integration
  checkout path as the publication destination.

A keep instruction that does not clearly identify its own retained result, or
whose destination is unknown or ambiguous, stops before any commit or
publication; report the exact gap. This is a real stop, not permission to
guess a destination or assume "the usual place."

## Keep and publish the retained result

After a validated explicit keep instruction, when this session announced a
preparation assignment, first stage its release in the owned workspace under
[Release it with the kept result](preparation-assignment.md#release-it-with-the-kept-result).
A stop there leaves the result and the assignment unchanged: report it and do
not land. In particular, `story-left-queue` means the story was taken,
completed, or removed on the remote target since this preparation began. The
keep stops there and the decision goes to the developer or coordinator; do not
land, retry, or reinterpret the story's new state.

Then land the owned workspace through
[Dough Land](../../dough-land/SKILL.md).
Reuse the local checkout role and the target
selection recorded in
[Select or reuse the workspace](preparation-workspace.md#select-or-reuse-the-workspace).
The owned workspace is the worktree to land.
The integration checkout path is the default checkout Dough Land refreshes.
The authorized remote target is the publication destination.
The checkout is not a stage the candidate must pass through.
Dough Land owns committing, publishing, refreshing the default checkout,
retiring the workspace, and continuing an interrupted landing from its first
unfinished step. Report its publication,
refresh, and cleanup results as the keep's results.

A keep is **confirmed** once the fetched authorized remote target contains the
landed SHA, whatever the refresh result. A landing that stopped before that
acceptance, or whose push ended without a clear answer, is not confirmed: the
assignment stays published, and it is not reported as released. Rerun `release`
first, under
[Release it with the kept result](preparation-assignment.md#release-it-with-the-kept-result),
then rerun the same landing from the same workspace and recorded target, never
a replacement.

**Conflicting scope change.** If resuming reveals that the human's story or
plan scope changed in a way that conflicts with what was about to be
published — not merely that the remote advanced, which is ordinary
reconciliation — preserve both the retained draft and the remote target, and
name the exact human decision needed. This is
[Stop for human judgment](../../dough-execute-plan/references/execution-decisions.md#stop-for-human-judgment).
When the developer gave an explicit no-push instruction, this section is never
entered: the record stays in the owned workspace, recoverable and unpublished,
and the remote is unchanged.

## What keep does not do

Keep authorizes only landing this session's own retained seed, story, plan,
bug-triage record, or retrospective record. It does not:

- move a backlog entry to **Taken** or perform any part of
  [take or admit work](../../dough-execute-plan/SKILL.md#take-or-admit-work);
- start implementation of the kept record;
- start, arm, or register with a CI/execution observer — that remains
  execution's own concern under
  [Own one observer](../../dough-execute-plan/references/ci-monitor.md#own-one-observer),
  not preparation's;
- establish an execution identity, mode, or claim;
- record or imply readiness: releasing the preparation assignment ends
  Preparing only.

This keeps "keep" a narrower operation than an execute-plan delivery: only the
retained planning record reaches the authorized remote target. Planning-only
execution limits are preserved — this capability never begins implementation
or takes the story, whatever the kept record describes.

## Discard an identified draft

Apply this section only after a validated explicit discard instruction, per
[Decide what happens to the written result](#decide-what-happens-to-the-written-result)
above: one that identifies this session's own retained seed, story, plan, or
bug-triage content, not implementation, unrelated exploration content,
disposable reproduction changes, or another session's work. This is the same
identification requirement
[Validate a keep instruction before acting](#validate-a-keep-instruction-before-acting)
already applies to a keep instruction's own retained-record check. Discard
needs no destination check: it never publishes anywhere, so that section's
local checkout role and target selection requirement does not apply here.

Discard removes only the specific session-owned draft or edit the developer
identified — the seed, story, plan, or bug-triage content this session wrote,
whether it is still uncommitted or already committed-but-unpublished in the
owned workspace. It does not remove:

- **the owned workspace itself.** A reused or host-owned workspace may hold
  other in-progress work — another story, plan, or session's own edits —
  that must survive a discard aimed only at this session's draft. Whether the
  workspace later becomes safe to remove is a separate, independent decision
  governed by [Close or retain the
  workspace](preparation-workspace.md#close-or-retain-the-workspace) (its
  session-created, clean, unambiguous test); discard here is about removing
  the identified draft content, not necessarily the workspace that held it;
- **unrelated edits** already present in that workspace before or alongside
  this session's write; or
- **another session's work**, even one sharing the same workspace.

If the identified content cannot be unambiguously isolated from other work in
the workspace — for example, an uncommitted mix of this session's edits and
another session's uncommitted edits touching the same file — stop and report
the exact conflict rather than guessing which content belongs to which
session. A discard that stops here is not a confirmed disposition: [Close or
retain the workspace](preparation-workspace.md#close-or-retain-the-workspace)
applies cleanup only after discard actually removes the identified content,
never merely because an attempt was made.
