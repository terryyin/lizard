# Continue in an established session

A first message's **Established session** block supplies the checkout and the
landing intent. Retain it across workflow invocations.

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

`isolated` is an owned worktree created for this session at fetched trunk.
`default-checkout` is the project's checkout on trunk as it was, including its
existing changes. `startingRevision` identifies where the work started. For
`default-checkout`, `fetched` records trunk fetched at that moment. For
`isolated`, the starting revision already records fetched trunk.

The block records the selected workspace and landing intent. The invocation
selects the workflow, tracking, identity and execution authority. Only the
published-admission handoff below already names an assignment.

Without a block, check with `scripts/session-landing-handoff.mjs` from the
native session's working checkout. In Cursor, run the exact shell command
`node .agents/skills/dough-execute-plan/scripts/session-landing-handoff.mjs`
and read the native post-tool context. Claude Code uses the script under
`.claude/skills/dough-execute-plan/scripts/`. A matching blank Start with
Automatically land returns the block, its Taken story, and exact landing and
reporting context. Empty output grants no inherited landing authority.

## Continue the published Dashboard admission

Use this handoff only for the published story the invocation continues. Keep
its identity, native checkout, branch and target. Continue under its scope,
setup and proof in Trunk Mode without starting or claiming work again. Resolve
a conflicting mode or assignment before changing state.

- `default-checkout`: verify that the native checkout can reach the accepted
  `publishedSha` from the [owned claim](wrap-up-closure-publication.md#finish-a-dashboard-admission-from-its-owned-claim-checkout)
  while preserving existing work. Use the separate claim checkout for wrap-up.
- `isolated`: `startingRevision` is the accepted `publishedSha`. The native
  worktree also owns ordinary [Trunk Mode closure](wrap-up-closure-publication.md#finish-trunk-mode-closure).

Use that SHA as the first increment's published base. Automatically land
selects [managed delivery](trunk-publication.md#publish-an-execution-increment-or-repair)
without another keep request. Explicit review, hold or local-only instructions
still govern. Carry the story, accepted publication and
[launch context](../../dough-land/references/dashboard-completion.md) into
Story Wrap Up when its completion gates permit.

## Work in the established workspace

One-shot, planless and contextual paths use the block's workspace, branch and
starting revision. Give a [one-shot](one-shot.md#start-in-an-owned-workspace)
or [one-shot refinement](../../dough-story-refinement/references/one-shot-refinement.md)
start the block's workspace and branch. Give the isolated start its remote and
target too. Its receipt has `created: false`. For the default checkout select
[its one-shot form](one-shot.md#work-in-the-default-checkout) with
`--default-main`. A named workflow still verifies its own story against trunk.

## Hand the workspace to a tracked start

For separate queued work, contextual admission or preparation, follow the
invoked workflow's start. An isolated session supplies its workspace and
branch as the owned checkout. The integration checkout, also where an
admission draft is written, stays the project's default checkout. A tracked
start from a `default-checkout` session uses it as integration and selects a
separate owned workspace as usual.

Continue a `published` or `prepared` receipt that reuses the owned workspace.
A refusal for `pending-edit` or `unpublished-commits` leaves the conversation's
work in place. Report it, settle the edits and commits as the developer decides,
and rerun the same start there. Handle other refusals under the invoked
workflow's own guidance.

## Land under the session's landing intent

For an admitted blank Start use [its managed delivery](#continue-the-published-dashboard-admission).
Other tracked work uses its invocation's publication authority. Dough Land
lands the block's checkout and branch on its remote target using the retained
starting revision. For results covered by the block:

- `auto-land` is advance keep. Use `--auto-land --push-authorized` for a
  one-shot start and [its delivery gates](one-shot.md#land-automatically-when-selected).
  A workflow that keeps through Dough Land uses the block as its keep instruction.
- `review` retains and reports the verified result with its workspace and
  branch for [later landing](one-shot.md#land-the-retained-result) or explicit
  Dough Land.

Give the first trunk publication the supplied [launch landing context](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).

## Retire the isolated workspace

An `isolated` block records creation for this session even if a later start
reuses it with `created: false`. After landing and required completion, use
[the normal retirement checks](../../dough-land/references/worktree-retirement.md)
with `--created-for-work` and the landed revision as `--contained`.
`default-checkout` sessions keep their workspaces.
