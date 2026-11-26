# Publish the candidate

Use the installed publisher named by the calling workflow. It owns the Git
sequence below. Follow the same sequence in the owned workspace when the
caller directs publication by hand. An unpublished suffix can be replayed.
A published prefix uses [history-preserving integration](preserve-published-history.md).

## Preconditions

Resolve the owned workspace, the authorized remote and its
`refs/heads/<target-branch>`, and the unique owned suffix or reviewed tip.
Resolve the caller's proof and registration duties before publication. Stop
if any required input is missing or contradictory.

The suffix is the caller's consecutive unpublished commits on the owned
branch, above the previously published base. Reconcile and push from the
workspace that owns it. Preserve published history and other writers' commits.
Use an ordinary push to the authorized ref.

Leave a separate default checkout alone even when it has pending work. Apply
[default-checkout preservation](maintain-default-checkout.md#preserve-pending-local-work)
before a publication or maintenance step mutates that checkout. Remote
acceptance stands independently of local refresh.

## Publish the candidate

For the first trunk publication of a Dashboard launch, supply its original
[landing context](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).
That handoff retains the pair before each push and records the accepted pair
before retirement. Follow its reporting-only retry for failed capture.

1. Fetch the authorized remote from the owned workspace.
2. Apply the caller's fetched-target checks before preparing a candidate.
   Use the fetched target, including when the default checkout is elsewhere.
3. If the target is unchanged from the published base or does not exist,
   keep the suffix. If it advanced, replay only the owned suffix with
   `git -C <owned-workspace> rebase --onto <fetched-target> <previously-published-base> <owned-branch>`.
   For the backlog or its done records, use the installed
   `product-backlog-git-rebase.mjs rebase --onto <fetched-target> --ref <previously-published-base> --branch <owned-branch> --cwd <owned-workspace>`.
   A nonzero adapter result, including `catalog-uncommitted`, stops with Git
   state preserved. Follow [publication rebase conflict](publication-rebase-conflict.md)
   to resolve it. A reviewed published prefix instead follows
   [Preserve published history](preserve-published-history.md).
4. Apply the caller's proof to the candidate. Reuse proof that still applies
   and recheck what the combined changes affect before any push. A failed
   recheck holds the candidate. Successful reconciliation alone supplies no
   behavioral proof.
5. Retain the full candidate SHA and its actual `suffixBase` immediately
   before each push. Use the previously published base for an unchanged
   suffix or the fetched target it was replayed onto after a rewrite. Keep
   both values from `beforePush` and the accepted receipt. This comparison
   covers the whole suffix, including multiple commits. Push the exact SHA
   with `git -C <owned-workspace> push <remote> <candidate>:refs/heads/<target-branch>`.
6. Confirm the accepted SHA on the authorized target. After an answered
   push, fresh increments and reviewed integration require exact equality
   with the live target tip.
   When the caller accepts containment, fetch and check ancestry. Record
   the exact accepted SHA and target. When observation applies, register
   that SHA on the caller's bound observer for that target or report a
   coverage gap. Leave any wait to the caller. Record
   [maintenance separately](maintain-default-checkout.md#independent-maintenance-outcome)
   and apply [refresh eligibility](maintain-default-checkout.md#refresh-eligibility)
   only after acceptance when the caller requires refresh.

## Recover a rejected push

For a non-fast-forward rejection, use the retained candidate and base from
step 5. Recheck [preconditions](#preconditions), including default-checkout
preservation when that checkout owns the suffix. Fetch the new target. Apply
steps 2–6 once more using the retained `suffixBase` as the replay cutoff,
including when the first attempt already rewrote the suffix. This keeps
another writer's commits outside the replay. A history-preserving integration
recomputes its merge on the new target. Keep the renewed proof and comparison
with the rewritten candidate.

On a second rejection or persistent failure, stop with the remaining Git
state and report the contention. An unanswered push is uncertain acceptance.
Use [interrupted publication recovery](#resume-an-interrupted-publication)
to settle it.

## Resume an interrupted publication

Use the workspace, remote and target of the interrupted attempt. Recover the
full candidate SHA and its retained `suffixBase` together before running
resume. The base must be an ancestor of the candidate, including equality for
an empty comparison. Validate both commit IDs before transport. Keep the
original pair even if the target advanced. Use the latest retained rewrite
when one was prepared. Keep superseded SHAs out of publication.

Fetch the authorized target. For direct inspection use
`git merge-base --is-ancestor <candidate> <fetched-remote-target>`.
Then follow the caller's recovery entry point and the observed state.

| State | Action |
| --- | --- |
| Target contains the candidate | Keep the accepted SHA and original comparison. Continue the caller's outstanding duties without another push. |
| Target lacks the retained candidate | Apply the caller's fetched-target holds before a push. Managed resume tries that exact SHA at most once and leaves it unchanged. If it is rejected, retain the state. Use the fresh publisher's reconciliation and proof path when authorized to prepare a replacement. For a direct caller, use the retained SHA when it still fast-forwards or follow the applicable [rejected-push recovery](#recover-a-rejected-push) to prepare a new candidate. |
| Candidate is local only in the default checkout | If it is the authorized owned checkout, the same exact-SHA rule applies. Preserve pending edits. |
| Acceptance lacks registration | Reuse the caller's receipt or register the accepted SHA on its live observer for the same target. Report any unresolved coverage gap. |

Missing or contradictory recovery inputs stop the path. Keep the existing
checkout and Git state until the caller can recover its exact coordinates.

## Preserve remaining state

On setup or publication failure, keep the refs, worktree, commits and index
as Git left them. Apply
[default-checkout preservation](maintain-default-checkout.md#preserve-pending-local-work)
if that checkout is involved. Accepted remote work stays accepted.
