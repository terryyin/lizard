# Publish the preparation assignment

While a queued story is being prepared, others see it as **Preparing** and
which agent holds it, from an assignment published on the remote target. This
reference starts, keeps, and ends that assignment. It applies inside
[prepare records in an owned workspace](preparation-workspace.md), with the
paths and target recorded in its
[Select or reuse the workspace](preparation-workspace.md#select-or-reuse-the-workspace),
and inside [decide, publish, or discard the written
result](preparation-disposition.md). In the commands below, `<installed>` is
this project's installed `dough-story-refinement` skill directory (normally
under `.agents/skills/` or `.claude/skills/`); each command names every
checkout it acts on, so run it as written with that path.

The assignment's announcement and end are commits authored by its agent that
credit the developer configured as Git committer in the checkout committing
them. `developer-identity-refused` from `start` or `abandon` means that
committer is missing, malformed, or the agent's own; nothing was published or
changed. Report its `error`; the developer configures `user.name` and
`user.email` there before you rerun the same command. Never publish around it
with another identity.

## Announce the preparation assignment

Story refinement, slice planning, and plan refinement of an existing queued
story with a stable identity announce that story as **Preparing** before
substantive work. Reading, discussing, answering questions, decomposing a
candidate without a queued identity, bug triage, a standalone retrospective
record, and an explicitly selected
[one-shot refinement](one-shot-refinement.md) announce nothing.

After selecting the workspace, or to select a new one, and before its first
record write, run:

```text
node <installed>/scripts/preparation-assignment.mjs start \
  [--integration <integration checkout>] \
  [--repository <owned worktree or common Git directory>] \
  --workspace <owned workspace> [--branch <new workspace branch>] \
  --identity <queued story identity> --remote <remote> --target <trunk branch> \
  --push-authorized [--host claude|codex|cursor] [--model <model>]
```

Use the recorded paths and target. Supply `--integration` when this project
has an integration checkout; without one, the owned workspace supplies
repository access. When no suitable owned workspace exists yet, name the new
workspace path and supply `--branch` with a new branch name; without an
integration checkout, also supply an owned worktree of the repository or its
common Git directory as `--repository`, which is only read from, never
refreshed. `start` fetches the target and, once it finds the story queued
there, creates the workspace on that branch at fetched trunk before
announcing. Its receipt then carries `selection` with `created: true`, the
branch, and the starting revision, and `start` has written the workspace's
[creation record](../../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it).
An existing path is the owned workspace you already selected or are resuming;
a new announcement uses it under
[refresh eligibility](../../dough-execute-plan/references/maintain-default-checkout.md#refresh-eligibility),
fast-forwarding it when trunk has moved past it. Supply `--push-authorized` only when publishing to that target
is authorized; without it the command stops. Supply your own host and model,
omitting either you cannot state rather than guessing.

Keep the receipt with this session and act on its `status`:

- `announced`: remote trunk accepted a commit that adds only your assignment
  profile, under the next free name of the rotation execution also uses. The
  queue and your draft are unchanged. The command then attempts the same safe
  refresh of the integration checkout as Dough Land's
  [Refresh the default checkout](../../dough-land/SKILL.md#refresh-the-default-checkout)
  and reports it in `refresh`; with no integration checkout supplied, its
  `result` is `not applicable`. No refresh
  [result](../../dough-execute-plan/references/maintain-default-checkout.md#independent-maintenance-outcome)
  undoes the announcement. Begin preparing in the workspace.
- `continued`: this workspace already holds the story's published assignment;
  nothing new is published. Run `start` at each preparation skill's first write
  in the session, and again on resuming after a pause, so slice planning after
  refinement or a resumed session keeps the same assignment instead of taking
  another name.
- Any stop (`ok: false`): do not begin substantive preparation. Report the
  receipt and preserve the workspace. A stop before a new workspace was
  created leaves no workspace or branch behind.
  `workspace-selection-failed`: the new workspace could not be created at
  fetched trunk; report its `error` and whatever path or branch partly exists,
  without retrying or removing it. `unpublished`: remote trunk did not
  accept an announcement, so nothing is assigned; when the receipt carries a
  `candidateSha`, acceptance could not be checked, so rerun `start` in the same
  workspace, which settles it from the remote instead of announcing twice.
  `workspace-not-isolated`: the workspace is not eligible, for the reason in
  `error`; the announcement comes before the first write and never carries a
  draft. `workspace-assigned-elsewhere`: this workspace still holds a published
  assignment the request does not name, such as another story's; keep or
  abandon that preparation first.
  `agent-unavailable`: every name is held; report its `occupied` list (each
  profile's agent, story, activity, and allocation, or why a file is
  unrecognized) so developers can see who holds what. Do not remove,
  reclaim, or wait out a profile on your own judgment: its age, its silence,
  no process running for it on this machine, or a missing workspace does not
  free it. Only a developer's confirmation that one exact preparation
  assignment is abandoned releases it, under
  [Release a lost workspace's assignment](preparation-lost-workspace.md).
  `not-queued`: the story is not queued on the fetched target.

The workspace remembers which announcement is its own. Later `start`,
`release`, and `abandon` commands identify the assignment from it, never from
the agent name, story, or tool and model alone, so keep every later command
for this assignment in the same workspace. If that workspace is lost before
the assignment ends, see
[Release a lost workspace's assignment](preparation-lost-workspace.md).

An explicit developer instruction not to publish or commit means: do not run
`start`; report that no Preparing assignment was published, so others cannot
see this preparation; continue only as that instruction allows.

A pause keeps the published assignment, however long it lasts. It ends only
when the kept result lands, under
[Release it with the kept result](#release-it-with-the-kept-result), or on an
explicit abandonment, under [Abandon the preparation](#abandon-the-preparation).

## Release it with the kept result

After a validated explicit keep instruction, per
[Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result),
stage the release in the owned workspace before landing:

```text
node <installed>/scripts/preparation-assignment.mjs release \
  --workspace <owned workspace> --identity <queued story identity> \
  --remote <remote> --target <trunk branch>
```

`release-staged` means the workspace now stages removal of exactly the profile
its own announcement added, verified against the fetched remote target, beside
the retained result. The landing then publishes result and release in one
snapshot, so no reader sees the result landed while the assignment stays
active. Any stop leaves both unchanged: report it and do not land.
`already-released` means trunk already ended this assignment (its `endedBy`
names the commit); there is nothing to stage, and the landing proceeds without
it. The story stays queued, and its refinement, approach, and readiness are
what the
[preparation recorder](../../dough-product-backlog/references/record-preparation.md)
wrote, never something the release implies.

`story-left-queue` means another developer took, completed, or removed the
story on the fetched target since this preparation began. Nothing was staged;
the draft, the workspace's commits, and the assignment stay as they were.
`story` says where it is now: `place: "taken"` with `owners`, the execution
assignments naming it (agent, mode, branch), or `place: "absent"`. Do not
land, retry until it passes, or reinterpret the story's new state, for example
by re-queuing it, folding the draft into the new owner's work, or treating a
completed story as kept. Report the receipt and hand the decision to the
developer or coordinator; its `choices` make none of them:

- `abandon`: end this assignment under
  [Abandon the preparation](#abandon-the-preparation); the draft stays;
- `discard`: discard the draft under
  [Discard an identified draft](preparation-disposition.md#discard-an-identified-draft);
- `separate-work`: take the draft's content up as separate work with the
  story's current owner.

Rerunning `release` without that decision gives the same stop. One limit
remains: the story can still leave the queue after `release` succeeds and
before the landing's push, because the landing reconciles with trunk without
rechecking the queue. Report that plainly if it happens rather than undoing
the landing yourself.

When the landing stopped before the remote target accepted it, or its push
ended without a clear answer, rerun `release`, which reads trunk again, before
rerunning that landing:

- `release-staged` with `staged: "already-committed"`: the release waits in
  the unpublished landing commit; rerun the landing.
- `already-released`: trunk already contains the release, normally because
  the interrupted landing was accepted. Rerun the landing, which pushes
  nothing already accepted and continues with refresh and retirement.
- `release-conflict` with a `successor`: this assignment already ended and
  trunk now holds a later allocation of the same name, possibly for the same
  story. Landing the removal would end that other assignment. Do not land;
  report the conflict. The removal must come out of the workspace's
  unpublished changes and commits first, which is the developer's decision.

## Abandon the preparation

Apply this section only after an explicit instruction to abandon preparing
the story, per
[Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result).
Pausing, going quiet, a finished or failed session, a discarded draft, a
leave-unpublished instruction, or an assignment's age is never an
abandonment. When publishing to the recorded target is not authorized, or the
developer said not to publish, do not abandon: report that the assignment
stays published and visible as Preparing.

From the owned workspace that announced it, run:

```text
node <installed>/scripts/preparation-assignment.mjs abandon \
  [--integration <integration checkout>] --workspace <owned workspace> \
  --identity <queued story identity> --remote <remote> --target <trunk branch> \
  --push-authorized
```

It publishes a commit on the fetched target that only removes this
workspace's own assignment profile, and leaves the workspace's files, index,
and commits exactly as they were. The story stays queued. Act on `status`:

- `abandoned`: remote trunk accepted the end at `publishedSha`. `refresh`
  reports the integration checkout separately, as for the announcement.
- `already-released`: trunk had already ended this assignment (`endedBy`),
  for example on a repeated request; nothing was published. A `successor`
  is a later allocation of the same name, which is someone else's and stays.
- `unpublished`: the remote did not accept the end; the assignment is still
  published. `unconfirmed`: whether it was accepted is unknown. Report either
  as such, and rerun the same command when the remote is reachable; it rereads
  trunk and never ends anything twice.
- `no-assignment`: this workspace holds no published assignment for the story.
  When the workspace that announced it is gone, use
  [Release a lost workspace's assignment](preparation-lost-workspace.md).

Afterwards, the draft stays in the workspace for a later keep or discard
decision. A keep then lands without an assignment: `release` reports
`already-released`. Preparing the story again announces anew from a clean
workspace. The workspace may be retired under
[Close or retain the workspace](preparation-workspace.md#close-or-retain-the-workspace)
once a disposition for its draft is confirmed.
