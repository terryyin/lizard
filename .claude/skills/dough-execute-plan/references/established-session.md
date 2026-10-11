# Continue in an established session

Your session's first message may carry an **Established session** block: the
session was started with no workflow, its place to work already exists, and the
developer chose in advance what happens to results it verifies.

```text
Established session:
- workspace: <checkout path this session works in>
- workspace role: <isolated | default-checkout>
- branch: <the checkout's branch>
- remote: <remote>
- target: <trunk branch>
- landing: <review | auto-land>
- startingRevision, fetched: only when known
```

- `workspace` and `branch` are where every workflow invoked in this session
  works. `workspace role: isolated` names an owned worktree created for this
  session at fetched trunk; `default-checkout` names this project's default
  checkout on its trunk branch, taken as it was, existing changes included.
- `remote` and `target` name the authorized remote trunk.
- `landing` is the developer's choice for results this session lands, under
  [Land under the session's landing intent](#land-under-the-sessions-landing-intent).
- `startingRevision` is the revision the session's work started from. `fetched`
  is the trunk fetched at that moment, carried for a `default-checkout`
  workspace; an `isolated` block has no `fetched` line, because its
  `startingRevision` is that trunk.

The block names no tracking, identity, mode, or agent, and grants no
execution, admission, or Take. Tracking comes from the invocation itself: the
skill and flags the developer types in this session decide whether work is
one-shot, admitted, or Taken, and that workflow owns its verification and
completion. Until one is invoked, nothing in the session is claimed, tracked,
or landed.

This reference applies in a session that received the block; it stays in force
for every workflow invoked there.

## Work in the established workspace

One-shot, planless, and contextual paths work in the block's `workspace` on its
`branch` in place of selecting or creating a workspace of their own, and retain
the block's `startingRevision` as the revision this session's result started
from.

A [one-shot](one-shot.md) start is given the established workspace and branch
and creates no second workspace:

- `workspace role: isolated`: run the start in
  [Start in an owned workspace](one-shot.md#start-in-an-owned-workspace) with
  the block's `workspace` as the owned workspace and its `branch`, `remote`, and
  `target`. Its `prepared` receipt carries `created: false`.
- `workspace role: default-checkout`: run it with `--default-main` and the
  block's `workspace`, as
  [Work in the default checkout](one-shot.md#work-in-the-default-checkout)
  describes.

[One-shot refinement](../../dough-story-refinement/references/one-shot-refinement.md)
gives its own start the same workspace, branch, and `--default-main` choice.
The start still checks how fetched trunk holds a named story, so supply the
identity the invocation names. A start that refuses the workspace over the
conversation's own edits or commits is answered as
[below](#hand-the-workspace-to-a-tracked-start).

## Hand the workspace to a tracked start

A standard tracked invocation, whether it Takes queued work,
[admits](admit-accepted-work.md) an accepted mission, or announces a
preparation, hands the established workspace and branch to its own start
command and reports that command's answer.

In an `isolated` session, supply the block's `workspace` and `branch` as the
owned workspace and branch of the start in
[Take or admit work](../SKILL.md#take-or-admit-work), or of the preparation
announcement. Its `--integration` stays this project's default checkout, which
is also where an admission's story draft is written. Then act on the answer:

- `published` with `created: false` (or `prepared` for a one-shot start):
  continue in the established workspace and branch, now at the start's result.
- `setup-failed` whose `recovery.error` names `pending-edit` or
  `unpublished-commits` (a preparation start reports the same error under
  `workspace-selection-failed`): the conversation's own uncommitted edits or
  commits are in the workspace, and the start changed nothing. Report the
  answer, then commit the edits or set them aside, and land the commits or
  set them aside as the developer decides, before rerunning the same start
  with the same workspace and branch. Never discard them, and never answer
  the refusal by creating another worktree.
- Any other answer is handled as the invoked workflow's own reference
  describes.

In a `default-checkout` session, a tracked start selects its own owned
workspace exactly as a direct invocation from that checkout does, with the
block's `workspace` as its integration checkout: the default checkout is taken
as it is only by a one-shot start with `--default-main`.

## Land under the session's landing intent

The block's `landing` covers results this session lands through one-shot
delivery or [Dough Land](../../dough-land/SKILL.md). It is independent of the
workspace role.

- `landing: auto-land` is the developer's advance keep for those results: a
  result the invoked workflow verified lands without a landing request. Give a
  one-shot start `--auto-land --push-authorized` and continue under
  [Land automatically when selected](one-shot.md#land-automatically-when-selected),
  with its conditions and stops unchanged. Where that workflow's keep runs
  through Dough Land, the block is its keep instruction for the established
  checkout.
- `landing: review` waits: report the verified result with its workspace and
  branch, and land it when the developer asks, as
  [Land the retained result](one-shot.md#land-the-retained-result) or an
  explicit Dough Land invocation describes.

Dough Land invoked in the session lands the established checkout: the block's
`workspace` and `branch` on its `remote` and `target`, with `startingRevision`
as its starting revision. An `isolated` workspace is an owned worktree, retired
once trunk contains its work; the default checkout stays in place.

Tracked work publishes through its own lifecycle as it always does, with the
authority its own invocation carries, and ignores the block's `landing`.

A landing made in the session supplies the landing context that the session's
reporting context names, when it names one, as that reporting context and
[the original launch's landing handoff](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing)
instruct.
