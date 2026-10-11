# Story Branch integration

Save the final-closure tip already published on the remote execution branch.
Follow [Story Branch integration observation](../../dough-execute-plan/references/wrap-up-closure-publication.md#observe-story-branch-integration)
for the target transition, publication, and shared completion on the accepted
integrated SHA. Its publication uses [Preserve published history](../../dough-execute-plan/references/preserve-published-history.md)
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

After accepted Story Branch supplier integration, [resolve and publish consumers](../../dough-product-backlog/references/supplier-dependencies.md#apply-and-publish-a-direct-resolution)
through the same closure publication/completion procedure before retirement, using
the resulting accepted SHA and shutdown receipt. That later publication takes no
dashboard landing context. Finish or explicitly retain
unresolved dependency work and evidence in existing active context; report gaps.
