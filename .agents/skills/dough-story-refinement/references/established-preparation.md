# Continue from an established preparation

Your instruction may carry an **Established preparation** block: this story's
Preparing assignment was already published for you, and its workspace exists.

```text
Established preparation:
- identity: <story identity>
- workspace: <owned workspace path>
- branch: <workspace branch>
- remote: <remote>
- target: <trunk branch>
- agent: <agent named on the assignment>
- publishedSha: <published assignment revision>, only when known
- integration checkout: <checkout path>, only when known
```

When the block is present:

- Skip the workspace selection in
  [Select or reuse the workspace](preparation-workspace.md#select-or-reuse-the-workspace)
  and the `start` call in
  [Announce the preparation assignment](preparation-assignment.md#announce-the-preparation-assignment).
  Make no second announcement or workspace; the start already did.
- Work in the named workspace and branch, not the directory you were opened in.
- Keep the fields as this preparation's recorded identity, workspace, target, and
  integration checkout for the later `release` or `abandon` commands and the
  keep decision.
- Continue with refinement and the record write.

A later skill in the same session that runs `start` for this story in this
workspace gets `continued`, not a second assignment.

## Continue an established one-shot preparation

A block whose first field is `tracking: one-shot` names a
[one-shot refinement](one-shot-refinement.md) start that already ran: it
published no assignment, so it names no agent or `publishedSha`. It names the
`workspace`, its `workspace role` (`isolated` for an owned workspace,
`default-checkout` for the default checkout taken as it is), `branch`,
`remote`, `target`, the selected `landing` (`review` or `auto-land`), and,
when known, `startingRevision` and the integration checkout.

- Skip the workspace selection and the `start --one-shot` call; make no
  announcement or second start.
- Work in the named workspace and branch, retaining `startingRevision`. In the
  default checkout, its existing changes are part of this session's result,
  as [Refine in the default checkout](one-shot-refinement.md#refine-in-the-default-checkout)
  describes.
- Continue at
  [Refine, record, and commit](one-shot-refinement.md#refine-record-and-commit).
  `landing: auto-land` is the selected automatic landing; `landing: review`
  stops for review.

Without the block, prepare as
[preparation workspace](preparation-workspace.md) describes.
