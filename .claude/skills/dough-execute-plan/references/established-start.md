# Continue from an established start

Your instruction may carry an **Established start** block: the claim on the
work was already taken and published for you, and its workspace exists.

```text
Established start:
- identity: <story identity>
- publisher ID: <stable execution publisher ID>
- workspace: <owned workspace path>
- branch: <execution branch>
- mode: <trunk | story-branch>
- remote: <remote>
- target: <trunk branch>
- publishedSha: <published claim revision>
- agent, plan, startingRevision, candidateSha: only when known
- readiness: Changed since readiness review (only when reported)
```

When the block is present:

- Skip the `execution-start.mjs start` call in
  [Take or admit work](../SKILL.md#take-or-admit-work). Make no second claim,
  profile, or branch publication; the start already did.
- Retain `publishedSha`: the first increment's managed delivery uses it as its
  previously published base. Keep the other fields as your execution identity.
- Retain any "Changed since readiness review" indication through continuation
  and recovery. It informs you of changes after the recorded review; it does not
  withdraw Ready or require another confirmation. Follow
  [execution and resume](../../dough-product-backlog/references/record-preparation.md#execution-and-resume)
  for scope alignment and readiness review; Take, resume and delivery do not
  renew the reviewed basis.
- Work in the named workspace and branch, not the directory you were opened in.
- Continue at the checkout-bound setup and project command under
  [execution location](execution-location.md), then the first slice.

## Continue an established one-shot start

A block whose first field is `tracking: one-shot` names a
[one-shot](one-shot.md) start that already ran: it published nothing, so it
names no publisher ID, `publishedSha`, or agent. It names the `workspace`,
its `workspace role` (`isolated` for an owned workspace, `default-checkout`
for the default checkout taken as it is), `branch`, `mode`, `remote`,
`target`, the selected `landing` (`review` or `auto-land`), and, when known,
`startingRevision` and the `fetched` trunk.

- Skip the `execution-start.mjs start` call in
  [one-shot work](one-shot.md); make no claim, profile, or second start.
- Retain `startingRevision` as that section's start would have, and work in
  the named workspace and branch. In the default checkout, its existing
  changes are part of this session's result, as
  [Work in the default checkout](one-shot.md#work-in-the-default-checkout)
  describes.
- Continue at the checkout-bound setup under
  [execution location](execution-location.md), then
  [Verify and retain the result](one-shot.md#verify-and-retain-the-result).
  `landing: auto-land` is the selected automatic landing; `landing: review`
  stops for review.

Without the block, take or admit the work as
[Take or admit work](../SKILL.md#take-or-admit-work) describes.
