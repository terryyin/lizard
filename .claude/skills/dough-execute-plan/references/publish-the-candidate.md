# Publish the candidate

Reusable remote-publication mechanics for one owned unpublished suffix onto a
shared authorized target: fetch, reconcile in the owned workspace, rebase only
that suffix, validate, push that exact candidate, and recover a rejected push.
Local default-checkout preservation and refresh are owned by
[maintain the default checkout](maintain-default-checkout.md). Remote
acceptance does not require that checkout to move, and a later maintenance
result does not erase an accepted publication.

A calling procedure supplies the owned workspace, the owned unpublished
suffix, the authorized remote target as the fully qualified branch ref
`refs/heads/<target-branch>`, and how it registers or validates the pushed
result. Nothing below requires execution's mode selection, Taken-claim
semantics, or CI-observer policy. [Trunk publication](trunk-publication.md) is
`dough-execute-plan`'s caller for claims, increments, and owned repairs: it
names the suffix, workspace, and authorized remote target, then uses the
sequence below.

## Preconditions

Resolve the owned workspace, the authorized remote target, and the owned
unpublished suffix. The suffix is that workspace's commits not yet on the
authorized remote target. For a claim, that suffix is the Taken commit
in the execution workspace. For an increment, it is the caller's consecutive
unpublished commits in the workspace that already holds them. Its parent is the
previously published base when this caller has recorded one. Rewrite only that
suffix.
Never rewrite published revisions or another writer's commits, force-push, or
publish a branch other than the authorized target.

The owned workspace is where the suffix is reconciled and from where it is
pushed. A preparation keep uses the preparation workspace. An execution
increment uses the execution worktree. A claim is committed and pushed
from the execution workspace selected before that claim.

A pending human edit on the default checkout does not block publication from
a different owned workspace. Do not stage, unstage, reset, stash, or otherwise
change that edit in order to publish. Before mutating the default checkout —
only when the owned workspace is that checkout, or when a maintenance step
actually refreshes it — apply
[preserve pending local work](maintain-default-checkout.md#preserve-pending-local-work).
Those checks do not gate a remote push from a separate owned workspace. A
maintenance stop does not register a publication the remote has not accepted,
and does not undo one it has.

## Preserve published history

The sequence below rebases an unpublished suffix. A caller integrating an
already-published tip supplies that tip instead of a suffix. In the owned
workspace, merge it onto the fetched authorized target. Fast-forward when
that tip already contains the fetched target; otherwise create a merge
commit so both published histories remain. Do not rebase those published
commits. When [agent commits](agent-commits.md) apply to that workspace, the
merge commit is one: for example, run
`git merge --no-ff --no-commit <published-tip>`, then commit the in-progress
merge through `agent-commit.mjs`; `-F "$(git rev-parse --git-path MERGE_MSG)"`
keeps Git's prepared message. When the merge
touches the product backlog or the done records beside it, run
`product-backlog-git-merge.mjs merge --ref <published-tip> --cwd <owned-workspace>`
rather than a raw `git merge`, following
[reconcile product backlog Git operations](../../dough-product-backlog/references/merge-conflicts.md);
it credits its merge commit the same way.
Push, confirmation, and the receipt stay the candidate SHA and its target.
A rejected push recomputes this merge once onto the newly fetched target
and pushes once. A superseded candidate is not the receipt. Resume treats
an ancestor of the fetched target as already accepted and does not merge
or push again.

## Publish the candidate

Apply [Preconditions](#preconditions) before this sequence. For an established
one-shot launch with supplied dashboard landing context, wire that context into
the installed publisher before step 5 under the shared
[landing handoff](../../dough-land/references/dashboard-completion.md#retain-the-one-shot-landing).
It retains each final pair before push and captures accepted evidence at step 6;
a later explicit landing uses the original launch's context.

1. Fetch the authorized remote for the target branch from the owned workspace.
2. Reconcile in that workspace from the fetched remote target, not from the
   default checkout's tip. Push the caller's candidate SHA later, never the
   default checkout's branch tip, so unrelated commits and a pending human
   edit there stay unpublished. Leave that checkout's commits, index, and
   working tree untouched when the owned workspace is separate. When the
   caller cannot name a unique suffix, stop under
   [preserve pending local work](maintain-default-checkout.md#preserve-pending-local-work)
   instead of guessing which commit to publish.
3. An unpublished suffix follows this step. An already-published tip follows
   [Preserve published history](#preserve-published-history) instead of this
   rebase. When the fetched authorized remote target is still the previously published
   base, or that target ref does not exist yet, leave the suffix unchanged.
   When that target advanced, rebase only that suffix onto the fetched target
   in the owned workspace. The range is commits after the previously published
   base on the owned branch:
   `git -C <owned-workspace> rebase --onto <fetched-remote-target> <previously-published-base> <owned-branch>`.
   When that replay touches the product backlog or the done records beside
   it, run the same range through
   the installed rebase adapter instead of that raw `git rebase`:
   `product-backlog-git-rebase.mjs rebase --onto <fetched-remote-target> --ref <previously-published-base> --branch <owned-branch> --cwd <owned-workspace>`,
   following
   [publication rebase conflicts](publication-rebase-conflict.md). After a
   rewrite, the pre-rebase SHA is not the candidate. Every adapter result
   that exits non-zero, including `catalog-uncommitted`, stops before the
   push and preserves the state Git left; resume it as
   [publication rebase conflicts](publication-rebase-conflict.md) describes.
   Do not rebase the default checkout's branch unless it is the owned branch.
4. Validate the candidate using the check the caller supplied for this
   suffix. An unchanged base does not invalidate accepted proof. A rebase
   onto a newer target invalidates only proof the combined changes affect;
   reverify that behavior and reuse the rest. Do not treat rebase success
   as behavioral proof, rerun unrelated checks, or wait for CI.
5. Immediately before pushing, retain the full candidate SHA and the base
   the owned suffix extends. When this candidate has not been rewritten,
   that base is the previously published base. When step 3 replayed the
   suffix, that base is the fetched target it was replayed onto, not the
   older revision and not the candidate tip. The execution publisher's
   `beforePush` callback exposes `candidate` and `suffixBase` for each
   attempted push, including after a retry rewrites the candidate and its base.
   Retain the pair together
   before allowing that push. The accepted result carries the same base and
   its receipt's SHA; a multi-commit suffix is not based at the tip's parent.
   Push that exact candidate from the owned workspace:
   `git -C <owned-workspace> push <remote> <candidate>:refs/heads/<target-branch>`.
   Do not fast-forward the default checkout, force-push, or move that
   checkout's branch with `merge`, `update-ref`, or `branch -f`.
6. After the push, fetch again and confirm the authorized remote contains
   that exact candidate. That confirmation is publication acceptance. Do
   not require the default checkout's `HEAD`, index, or working tree to
   match. When this caller keeps published revisions, append that SHA; do
   not add a pre-rebase SHA or treat a later moving `HEAD` as the
   publication. The receipt is that accepted SHA and the target it was
   accepted on. When an observer is already bound to that target, register
   that SHA. Do not register a pre-rebase SHA, and do not register the SHA
   with an observer bound to a different target.
   Registration failure is lost coverage: report it and do not claim the
   revision was observed. Do not wait for CI. Then record the separate
   [maintenance outcome](maintain-default-checkout.md#independent-maintenance-outcome)
   by inspecting the default checkout and not refreshing it in this
   sequence. A caller attempts refresh only after this sequence, under
   [Refresh eligibility](maintain-default-checkout.md#refresh-eligibility).
   Acceptance here is independent of that attempt.

## Recover a rejected push

A non-fast-forward rejection leaves the owned suffix unpublished. Retain the
rejected candidate SHA and the base that suffix extends, the base retained
in step 5. After a rewrite, that base is the fetched target the suffix was
replayed onto. Do not use the rejected tip as the cutoff: that range is
empty and drops the suffix. Do not use an older published revision the
rewritten suffix no longer extends directly: that range includes another
writer's commits. Recheck
[Preconditions](#preconditions). If the owned workspace is the default
checkout, recheck
[default-checkout preservation](maintain-default-checkout.md#preserve-pending-local-work)
before rebasing that checkout. A separate owned workspace does not wait on
that checkout. When the applicable checks hold, reconcile only the suffix and
retry one ordinary push:

1. Fetch the authorized remote. The current target is that fetched remote target.
2. In the owned workspace, replay the same owned-suffix range as
   [candidate step 3](#publish-the-candidate), using the base retained in
   step 5 as the cutoff: only commits after that base, onto that fetched target.
   A history-preserving merge recomputes that merge onto the fetched target
   instead of rebasing, through the merge adapter when the backlog or its done
   records are touched. When an unpublished suffix touches the product backlog
   or its done records, that replay is the rebase adapter, not a raw
   `git rebase`. Do not rebase from the rejected
   candidate, and do not rebase the default checkout unless it is the owned
   branch. Either mistake can drop the suffix or rewrite another writer's
   commits. Every adapter result that exits non-zero, including
   `catalog-uncommitted`, stops here. Preserve the refs, worktree, and index
   Git left, report that result, and do not push; resume it as
   [publication rebase conflicts](publication-rebase-conflict.md) describes.
3. The rewritten owned-branch tip is the candidate. The rejected SHA is not.
   Do not move the default checkout onto it.
4. Revalidate as in candidate step 4. The rebase invalidates only proof the
   combined changes affect. A failed recheck stops before the retry push and
   preserves the rewritten candidate.
5. Push the rewritten candidate once, as in candidate step 5. After confirmed
   remote acceptance, record that SHA and the separate maintenance outcome as
   in candidate step 6.

A second rejection or other persistent failure stops. Preserve the rewritten
candidate and the remote as they are. Report the persistent contention. Do
not rebase or push again.

## Resume an interrupted publication

After an interruption, classify the owned suffix from fetched remote history
and the candidate SHA retained immediately before the push. A rewrite updates
that retained SHA before the push, so a lost response does not put a
pre-rebase SHA back in its place. Fetch the authorized remote, then test
ancestry — not tip equality:

`git merge-base --is-ancestor <candidate> <fetched-remote-target>`

The candidate is published when that command succeeds, including when another
writer has since added commits on top of it. Do not rebase that candidate,
push it again, or push a superseded pre-rebase SHA. A lost or unknown push
response is this published case whenever the retained candidate is an
ancestor. It is not a rejected push.

When the caller retained the delivery comparison before push, supply its
`suffixBase` with that candidate. Resume validates both full commit IDs and
base ancestry before any fetch, push, or registration, and returns that base
unchanged even after the target advances. Equal ends are a valid empty
comparison. Without an explicitly retained base, ordinary publication recovery
still applies, but the result has no historical comparison. Do not substitute
the candidate's parent, today's target, or an earlier starting revision after
a rewrite.

Continue only the first unfinished obligation below. Do not duplicate the
commit or replace the caller's workspace. A pending human edit on the default
checkout does not change this classification and is not destroyed while
resuming. Checkout maintenance and resource cleanup are not obligations of
this step: record maintenance by inspection only, and leave cleanup to the
caller that already owns it.

| Boundary | Actual state | Continue with |
| --- | --- | --- |
| Not on the remote | The retained candidate is not an ancestor of fetched remote history. The owned workspace still has that commit. | [Publish the candidate](#publish-the-candidate) from step 1. When that SHA still fast-forwards onto the fetched target, push it and do not commit again. When it does not fast-forward, use [rejected-push recovery](#recover-a-rejected-push); do not treat the candidate as published. Do not fast-forward the default checkout. When an observer is already bound to that target, step 6's registration is part of finishing the publication that this push accepts. |
| Candidate only on the default checkout | That checkout's target tip is the owned candidate, and the remote does not contain it. The owned workspace for this suffix is that checkout. | Push that exact SHA ([Publish the candidate](#publish-the-candidate) step 5). Do not rebase or commit again unless a newer remote requires [rejected-push recovery](#recover-a-rejected-push). Preserve any pending human edit; the SHA push does not include it. |
| Already published | The retained candidate — the rewritten SHA, when a rewrite was retained — is an ancestor of fetched remote history. The remote tip may be a later writer's commit. | Append that SHA to retained published revisions when identity omitted it. Do not push. This recognition is not registration, maintenance, or cleanup. |
| Missing registration | The candidate is already an ancestor and its SHA is retained, but an observer already bound to this caller has no receipt for it. A caller that binds no observer has nothing to register. | Register that SHA with the existing observer. Do not push, and do not start a replacement observer. Leave maintenance and cleanup unperformed. |

If the owned workspace, target, or candidate SHA is missing, contradictory, or
matches no unique owned suffix, preserve every existing worktree, branch, and
index under
[preserve pending local work](maintain-default-checkout.md#preserve-pending-local-work).
Report the gap. Do not create a replacement worktree, switch branches, or
guess which commit to publish.

## Preserve remaining state

Setup or publication failure preserves remaining state exactly as found:
existing worktrees, branches, commits, and index content stay untouched under
[preserve pending local work](maintain-default-checkout.md#preserve-pending-local-work).
An accepted remote candidate stays accepted. Preserved state is not permission
to start unauthorized work, continue from an unpublished result, or substitute
a different destination than the one already recorded for this caller.
