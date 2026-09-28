---
name: dough-land
description: >-
  Dough Land lands everything in one reviewed, owned worktree on the authorized
  remote trunk: commits all of its changes, reconciles and publishes them without
  force, refreshes the default checkout when safe, and retires the clean worktree
  and its branch once trunk contains them. Use only on explicit invocation, such
  as "/dough-land", "use Dough Land", or "land this worktree", or when another
  skill's validated keep instruction links here. A casual "keep", "looks good",
  or approval does not invoke it. Hosted merges and pull requests are out of scope.
---

# Dough Land

Land one reviewed worktree: commit everything in it, publish that onto the
authorized remote trunk, refresh the default checkout when safe, and retire the
worktree. Publication, refresh, and cleanup are separate results. A later step
that stops or is deferred never undoes an earlier one.

Run only when the developer explicitly invokes Dough Land, or when a calling
skill's own validated keep instruction links here. Reviewing, approving,
pausing, or saying "keep" in passing does not start a landing. Do not open a
pull request, request a hosted merge, or discard anything; when the developer
wants one of those, stop and say that Dough Land does not do it.

## Resolve the worktree and target

Resolve both before any commit:

- **Worktree.** The worktree named by the invocation or its context: normally
  the current one, or the owned workspace a calling skill recorded. Record its
  path and branch, whether this work created it or reused one another
  workflow owns, and its starting revision when known. Ownership comes from
  that context; do not infer it from a clean directory.
- **Default checkout.** The project's established checkout for ordinary work,
  which the refresh step inspects. It is never the worktree being landed.
- **Target.** The authorized remote target as `refs/heads/<branch>` on a named
  remote: the target selection already recorded for this worktree, or the one
  the project or developer authorizes. Default the branch to `main` only when
  neither the caller nor the project supplies one.

Stop before committing, and name the gap, when:

- no worktree is in context, or more than one candidate fits;
- the named worktree is the default checkout itself — landing never commits
  or pushes from it;
- the target is missing, contradictory, or not a branch on an authorized
  remote; or
- the worktree has an unfinished merge, rebase, cherry-pick, or revert. Name
  it; the developer resolves it, then reruns.

## Commit everything in the worktree

Everything in the worktree is the reviewed change. Stage all tracked,
untracked, and deleted paths (`git -C <worktree> add -A`) and commit them in
the worktree with a message describing the reviewed change, as an
[agent commit](../dough-execute-plan/references/agent-commits.md) when that
reference applies to the worktree; its refusal stops the landing before
publication. When there is nothing to commit, create no commit; that is the
normal rerun case.

The owned unpublished suffix is every commit on the worktree branch that the
fetched target does not contain. Its previously published base is the last
SHA this landing recorded as accepted, otherwise the worktree's recorded
starting revision, otherwise the merge base of the branch and the fetched
target.

A calling skill that must land only its own record checks, before linking
here, that the worktree holds nothing else. Dough Land does not pick paths out
of a worktree. A preparation keep stages its assignment's release in the
worktree before linking here, so it lands with the result; Dough Land itself
never adds or removes an assignment profile.

## Publish

Apply [publish the candidate](../dough-execute-plan/references/publish-the-candidate.md)
from the worktree, for that suffix and target; do not invent a second
sequence. The check this caller supplies is that the candidate's own changes
are the reviewed worktree content. A rewrite onto a newer target rechecks only
proof the combined change affects. Nothing is registered with a CI observer.

A conflict, refusal, failed recheck, or second rejection stops here. Preserve
the worktree, branch, index, and whatever state Git left. Name the conflict or
contention, and report publication, refresh, and cleanup as not done. Do not
loop.

## Refresh the default checkout

After acceptance, attempt
[Refresh eligibility](../dough-execute-plan/references/maintain-default-checkout.md#refresh-eligibility)
on the default checkout, without acquiring exclusive access to it. Report its
result and reason separately from publication. A deferred or stopped refresh,
such as a pending human edit in that checkout, is not a failed landing and
does not block retirement.

Another skill may apply this section on its own after its own accepted
publication.

## Retire the worktree

Retirement follows
[own a temporary exploration workspace](../dough-manual-testing/references/exploration-workspace.md)
"Close or retain it", with containment as the safety test:

1. Fetch the target. Continue only when the fetched target contains the
   worktree branch tip (`git merge-base --is-ancestor <tip> <remote>/<branch>`).
   Remote acceptance is what makes retirement safe. The default checkout
   being behind the target does not make the branch unmerged.
2. Remove the worktree (`git worktree remove <worktree>`) only as that
   section allows: clean and created by this work. Otherwise retain it and
   report its path, branch, and reason.
3. Delete the local branch with a safe, non-force delete once the target
   contains its tip (for example, point its upstream at the fetched target,
   then `git branch -d`).
4. Delete a separately published remote branch for the worktree only after
   the target contains its tip, and never the target branch itself.

Accept already-absent resources on a rerun. Never force-remove, force-delete,
or reset to make cleanup possible. A calling skill may add its own gate, such
as a completion receipt that must arrive first; retirement then waits for that
gate as well as containment, and keeps every resource while either is missing.

Another skill may apply this section on its own after its own accepted
publication, with its own gate.

## Stop, rerun, and report

A stop keeps every resource recoverable (worktree, branch, commits, index, the
default checkout, and the remote as it is) and names the exact unfinished
step. A rerun reads real Git and remote state and continues from the first
unfinished step:

| State found | Continue with |
| --- | --- |
| Unfinished Git operation in the worktree | Stop and name it |
| Uncommitted changes | [Commit everything](#commit-everything-in-the-worktree) |
| Branch tip not contained in the fetched target | [Publish](#publish), through the publisher's [resume](../dough-execute-plan/references/publish-the-candidate.md#resume-an-interrupted-publication) |
| Tip already contained | Record it as accepted; push nothing. Then refresh and retire |
| Worktree or branch already absent | Report it as already retired |

Never commit the same change twice, push an already accepted candidate again,
or create a replacement worktree.

Report, as separate results:

- **Publication:** accepted SHA and target, or the step that stopped and why.
- **Refresh:** the refresh result and reason, or not attempted.
- **Cleanup:** worktree and local branch removed, already absent, or retained
  with path and reason; a remote branch deleted only when verified absent.
