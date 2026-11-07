# Lifecycle commitments at closure

A planned execution's source review can allocate a commitment to a
[lifecycle owner](../../dough-execute-plan/references/story-obligations.md#record-the-source-review)
instead of a slice: `landing`, `target CI` or `reporting`. Execution checks
leave those rows unobserved for closure, and wrap-up deletes the plan and
source review before their owners act. Each row stays mandatory and unobserved
until its owner's existing receipt below holds.

## Read them at completion

At [Establish execution completion](../SKILL.md#establish-execution-completion),
run with this project's selected plan:

```sh
node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --planned-completion
```

A refusal leaves the work unfinished as that section says. Take every
`pendingLifecycle` entry (`id`, `text`, `owner`) from the result as returned,
in its order. No entry, or no source review, means no lifecycle commitment.

## Carry them past deletion

Retain the entries with the rest of the execution context kept before the plan
is deleted, in the coordinator's available execution context and closure
report, not in a new file or registry. Do not mark a row observed in the plan or
source review before deleting them; a lifecycle row never carries `observed:`.
If the entries cannot be retained, stop before deleting the plan.

## Observe each owner's receipt

Judge each retained entry after its owner's operation settles, against the
receipt wrap-up already requires:

- `landing`: the accepted integration or landing receipt for this closure on the
  authorized target, meaning Story Branch integration's accepted receipt, or in
  Trunk Mode the accepted publication of the final-closure commit through
  `finish`. Committed pending publication, a superseded candidate, or a refused
  or preserved integration is not that receipt.
- `target CI`: the `success` completion receipt for the final accepted SHA with
  confirmed shutdown. Branch CI, an `integrated_state` success of another
  candidate, a pending or retained observation, and the check that enforces the
  gate are not that receipt.
- `reporting`: the matching receipt of the
  [final dashboard operation](../../dough-land/references/dashboard-completion.md)
  run with supplied reporting context.

Never treat the deleted records, the plan's execution-complete record, a
pending gate or the enforcing check itself as an owner's observation.

## Report the outcome

Judge the `landing` and `target CI` entries before final wording settles. When
any of them lacks its owner's receipt, closure is not successful: its attention
response, and an `--outcome unfinished` final dashboard report when reporting
context was supplied, names each unobserved commitment by id and text with its
owner, in the owner's operation name: Story Branch integration or Trunk Mode
`finish` for `landing`, final target CI for `target CI`, and the final dashboard
operation for `reporting`. Never submit `--outcome completed` while such an
entry is unobserved.

When every `landing` and `target CI` entry's receipt holds and no other gate
prevents success, a `reporting` entry is observed by the final operation
itself: with supplied reporting context, submit the
[final dashboard operation](../../dough-land/references/dashboard-completion.md)
with `--outcome completed` and wait for its matching receipt, which is that
entry's observation. When delivery fails or stays unacknowledged, report
unacknowledged delivery locally as that operation says, retain its message and
`Retry reporting only` command, and do not claim successful closure: name each
`reporting` entry and the final dashboard operation as its owner. Without
supplied reporting context, each `reporting` entry stays unobserved; name it and
its owner in the native response and make no dashboard contact.

Keep every other gate as it is: an unobserved commitment changes neither
publication, CI observation, shutdown nor retirement, and resources stay
wherever those gates leave them.

For example, when Story Branch integration is refused at wrap-up, its
[refusal](story-branch-integration.md) preserves the execution resources and
blocks completion; the closure report also names each `landing` commitment, and
each `target CI` commitment still lacking its receipt, with its owner, and does
not report success.
