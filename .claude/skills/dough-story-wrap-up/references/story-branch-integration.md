# Story Branch integration

Save the final-closure tip already published on the remote execution branch.
Follow [Story Branch integration observation](../../dough-execute-plan/references/wrap-up-closure-publication.md#observe-story-branch-integration)
for the target transition, publication, and shared completion on the final
accepted SHA: the integrated SHA, or the last consumer publication. Its
publication uses [Preserve published history](../../dough-execute-plan/references/preserve-published-history.md)
from the owned execution workspace, excluding any default checkout's
unrelated commits and pending human edit.

When the merge touches the product backlog or its done records, use the owned
workspace's installed merge adapter as that procedure requires, preserving
[follow-up priority](follow-up-disposition.md#preserve-follow-up-priority-during-reconciliation).
A stopped result stays as Git left it.
Resolve it through [a real conflict](../../dough-product-backlog/references/merge-conflicts.md#a-real-conflict-resolve-by-hand-then-continue-through-the-same-adapter).
If the adapter and that reference are unavailable, report the gap and leave the
conflict. Stop when no coherent resolution is justified. Selected-work cleanup
alone does not prove a sibling backlog change survived.

A `preserved` integration result with a `conflict` or `uncommitted-merge`
reason takes two steps: resolve and commit the stopped merge in the owned
workspace, then run the same integration command again, which publishes that
commit and returns the receipt.

Require that procedure's accepted receipt before resource cleanup. The receipt
is the accepted candidate SHA and the remote trunk ref. A superseded candidate
is not the receipt. Unresolved integration preserves the execution resources
and blocks completion. Do not force-push.

Pass the selected identity to that integration as `--supplier-identity` under
the [visit rule](../../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit),
even when its branch shows no consumer. After its accepted receipt, apply the
[current-consumer visit](../../dough-product-backlog/references/supplier-dependencies.md#visit-current-consumers-after-integration)
to the printed `visit` from the same owned workspace, judge each consumer, and
publish owned consumer changes to the authorized trunk through
[wrap-up closure publication](../../dough-execute-plan/references/wrap-up-closure-publication.md)
on the same trunk observer. Those later publications take no dashboard landing
context. Finish the visit, or explicitly retain unresolved dependency work,
evidence and gaps in existing active context, before the final completion and
retirement that procedure orders; report gaps.

## Retire Story Branch resources

Retire the worktree and branch under Dough Land's
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree) with
`--remote-branch <execution branch>` and `--contained <integrated SHA>`, adding
`--contained` for each accepted consumer publication, with wrap-up's gate: a
`success` completion receipt for the final accepted SHA whose shutdown is
confirmed, and no active checkout-bound observer still hosting the worktree
under [preserve pending local work](../../dough-execute-plan/references/maintain-default-checkout.md#preserve-pending-local-work).
