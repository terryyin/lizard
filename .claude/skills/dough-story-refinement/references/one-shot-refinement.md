# One-shot refinement

One-shot refinement refines one story queued in the **Backlog list** without
publishing a Preparing assignment. It works in an owned workspace at fetched
trunk, or in the default checkout when that is selected, records the story's
preparation facts there, and stops with the committed result retained for
review, or lands it when automatic landing is also selected. Use it only when the developer or parent
instruction explicitly selects one-shot (`--one-shot` or a clear equivalent)
for refining that story; never infer it from apparent smallness. Without that
selection, refine under
[Announce the preparation assignment](preparation-assignment.md#announce-the-preparation-assignment).

An instruction that carries an established preparation continues it under
[established preparation](established-preparation.md), even when it also
names one-shot.

## Start without an assignment

Resolve the paths and target under
[Select or reuse the workspace](preparation-workspace.md#select-or-reuse-the-workspace),
then, before the first record write, run:

```text
node <installed>/scripts/preparation-assignment.mjs start --one-shot \
  [--integration <integration checkout>] \
  [--repository <owned worktree or common Git directory>] \
  --workspace <owned workspace> [--branch <new workspace branch>] \
  --identity <queued story identity> --remote <remote> --target <trunk branch>
```

`<installed>` is this project's installed `dough-story-refinement` skill
directory. Supply `--integration`, `--repository`, and `--branch` as
[Announce the preparation assignment](preparation-assignment.md#announce-the-preparation-assignment)
describes. The start needs no publication authority: it fetches the target,
checks how fetched trunk holds the story, and selects or creates the owned
workspace at fetched trunk. It publishes nothing: no commit, push, agent
profile, or backlog change.

Act on the receipt's `status`:

- `prepared`: the workspace is ready at `startingRevision`; retain it with
  `workspace`, `branch`, and `created`. `refresh` reports the integration
  checkout's safe refresh, as for an announcement. Begin refining.
- `source-refused`: fetched trunk shows the story Taken or held by an agent
  profile (execution or preparation). Report its `error` and leave the story
  to that holder.
- `workspace-assigned`: the workspace holds a published preparation
  assignment. For this story, continue that assignment by running `start`
  without `--one-shot`; otherwise end the other assignment first.
- `not-queued`, `workspace-selection-failed`, and `invalid-request` stop as
  they do for an announcement.

A recorded `not-ready` assessment does not stop the start: refinement may be
what repairs it.

## Refine in the default checkout

When the developer or parent instruction also selects the default checkout
(`--default-main` or a clear equivalent), refine directly in this project's
established default checkout on its trunk branch. Resolve that checkout's
actual path and current branch; a configured project folder alone does not
establish them. Run the start above with `--default-main` and `--workspace` set
to the default checkout; omit `--branch` and `--integration`, or name the trunk
branch and that same checkout.

The start checks the story on fetched trunk as above, then takes the checkout
exactly as it is: uncommitted changes and local commits stay, and nothing is
reset, refreshed, created, or published. Its `prepared` receipt carries
`role: "default-checkout"`, `workspace`, `branch`, its actual HEAD as
`startingRevision`, the `fetched` trunk, and `created: false`, with no
`refresh`. `workspace-selection-failed` refuses a checkout on a branch other
than trunk or with an ongoing Git operation; report its `error` and leave
switching branches or finishing the operation to the developer.
`invalid-request` names a `--branch` or checkout path that does not match, and
`--default-main` without `--one-shot`.

Existing changes in the checkout need no clean checkout or confirmation: they
become part of this session's result. When committing below, stage everything
(`git add -A`) so all checkout content is committed together; never discard,
stash, or leave out existing content.

## Refine, record, and commit

Refine under [planning scope and lifecycle](planning.md) in the workspace.
After the seed records goal, scope, and key examples, apply
[record preparation facts](../../dough-product-backlog/references/record-preparation.md),
including its readiness assessment once you have reviewed the story. Record
only what is true: refined alone is not ready, and an unselected approach is
a blocking reason.

Commit the seed and its recorded facts with plain `git commit` in the
workspace: the start named no agent.

## Stop for review

Unless automatic landing was selected, stop here; with it, continue under
[Land automatically when selected](#land-automatically-when-selected).
Report the workspace path and branch, the retained `startingRevision`, the
result commit, the recorded refinement, approach, and assessment, and any
unresolved decisions. Say that remote trunk still lists the story in the
**Backlog list** with no Preparing assignment, so others cannot see this
refinement until it lands. Leave the workspace and its branch in place:
nothing is pushed or retired, and the story is neither Taken nor completed.

A later session that names the retained workspace continues there without
running `start` again. The default checkout always stays in place.

## Land automatically when selected

When the developer or parent instruction also selects automatic landing
(`--auto-land` or a clear equivalent), that selection is the developer's
advance keep instruction for this refinement's result, so it does not wait for
review. It is independent of the workspace choice. A confirmation that
existing default-checkout changes may join the result lets them be committed
with it; it never selects automatic landing. Add `--auto-land
--push-authorized` to the start: its `prepared` receipt then also carries
`landing: "auto-land"`, and `authority-required` names missing publication
authority before any workspace is selected.

Land only once the seed records the story's goal, scope, and key examples, its
recorded facts and assessment are true, and no decision you need from the
developer remains open. Then land the committed result as
[Land or discard on request](#land-or-discard-on-request) describes for a keep,
ownership recheck included. The story stays queued with the recorded facts:
landing neither Takes nor completes it, and starts no CI observer.

Stop instead, keeping the committed result and its workspace as they are, and
report what stopped it, when a decision remains open (it goes to the
developer), the recheck reports `ownership-changed`, or the landing stops on a
conflict or a second rejection. Push nothing more after such a stop.

## Land or discard on request

The retained result follows
[Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result).
An explicit keep lands it under
[Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result)
with no assignment release to stage. The story stays queued with the facts
the recorder wrote. Then close the workspace under
[Close or retain the workspace](preparation-workspace.md#close-or-retain-the-workspace).

A default-checkout result holds everything committed there, so an explicit
keep lands it with [Dough Land](../../dough-land/SKILL.md) from that checkout,
which lands all of its content and keeps the checkout in place.

Every landing of this result rechecks the story on fetched trunk. Supply this
command as the candidate check Dough Land's publication runs, so it runs
before the push and again before the retry after a rejected push:

```text
node <installed>/scripts/preparation-assignment.mjs recheck \
  --workspace <workspace> --identity <queued story identity> \
  --remote <remote> --target <trunk branch>
```

`queued` lets the push go ahead; a recorded `not-ready` assessment does not
stop it. `ownership-changed` means another owner now holds the story (a Taken
entry or an agent profile) or it has left the **Backlog list**: push nothing,
keep the committed result and its workspace, report the `ownership` and
`error`, and leave the story to that owner and the developer. A landing whose
push ended without a clear answer continues as Dough Land's rerun describes,
from the same commit.
