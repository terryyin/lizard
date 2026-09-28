# Maintain the default checkout

Owns default-checkout access, local preservation, and the independent
maintenance outcome for that checkout. Remote candidate acceptance is a
separate fact owned by [publish the candidate](publish-the-candidate.md).
Callers supply which checkout is the default or integration checkout and
when they need access, preservation, or a refresh attempt. This reference
does not publish, invent a merge queue or lock service, or replace a
caller's disposition, cleanup, or CI policy.

## Independent maintenance outcome

Successful remote publication and default-checkout maintenance are
independently reportable. A deferred, stopped, or unfinished refresh does
not erase an accepted remote publication. A publication that never reached
remote acceptance leaves any outstanding maintenance obligation unchanged
and does not authorize treating the remote as updated.

When reporting, name the maintenance result separately from publication
acceptance:

- **advanced** — this attempt fast-forwarded an eligible clean checkout to fetched trunk.
- **already current** — the checkout is clean and its `HEAD` is that fetched revision.
- **deferred** — a pending human edit, staged or unstaged change, unpublished commit, ongoing operation, another writer's ownership, or ambiguous ownership. Name the preserved `HEAD`, index, and working tree.
- **stopped** — the checked-out branch is not the integration branch, or local history has diverged from fetched trunk. Preserve that state and name it.

Owned-workspace publication records an inspection result and does not
fast-forward. Inspection reports **already current** only when the checkout
is clean and `HEAD` is the accepted remote revision. Every other inspected
state is **deferred**. That inspection is not the refresh decision below.
A caller attempts refresh only by following
[Refresh eligibility](#refresh-eligibility).

## Establish access before local mutation

For a direct edit or publication that mutates the default checkout's working
tree, index, or checked-out branch, acquire exclusive local access through
available coordinator context and inspect the checkout. A clean directory or Git lock file does not establish exclusivity;
coordinate with a declared owner or stop. Do not invent a merge queue, lock,
or extra claim.

Automatic refresh follows [Refresh eligibility](#refresh-eligibility) instead.
It does not require an ownership declaration when none is available and does
not acquire exclusive access. Preserve a known competing writer's checkout.

These access checks gate direct edits and publication from the default checkout. They do not gate
commits, proof, formatting, or a remote push from a separate owned
execution or preparation workspace. Publishing that workspace does not
acquire this checkout's access.

## Preserve pending local work

Dirty or ambiguous checkout state, or unrelated unpublished local commits,
stop local mutation of that checkout. Unknown ownership also stops direct
edits; a missing declaration alone does not stop automatic refresh. Preserve
exact refs, worktrees, index, and staged/unstaged content; do not stash,
reset, unstage, revert, or silently include unrelated work in a refresh or
in a publication that mutates this checkout.

Report the competing writer or inspectable state. When the unpublished
suffix lives on this checkout, leave it recoverable and do not treat a
remote that lacks it as published. When the suffix lives in a separate
owned workspace, this preservation does not block that workspace's push.

Apply the same preservation after a rejected push that left an owned
suffix unpublished on this checkout: do not undo that locally integrated
suffix to clear the tree.

## Refresh eligibility

This reference owns whether and how the default checkout may be advanced
toward fetched trunk. Callers invoke a refresh only when their own
procedure requests one: after an accepted trunk publication, or before
using this checkout's commit as a new task base. A new owned workspace may
start from fetched remote trunk without advancing the default checkout.
Publication success does not decide eligibility.

Inspect current checkout state on every refresh attempt and again after any
handoff. These checks preserve local work; they do not provide mutual exclusion
between simultaneous writers.

1. Honor any declared owner from coordinator context. Another declared owner
   is **deferred** (`another-writer`); a declared owner with no identified
   requester is **deferred** (`unclear-ownership`). Without a declared owner,
   continue with the Git checks below. Do not require or invent an ownership
   declaration merely to refresh.
2. Re-read the working tree, index, `HEAD`, checked-out branch, and any
   in-progress operation. An `index.lock`, or an in-progress merge, rebase,
   cherry-pick, or revert, is **deferred** (`ongoing-operation`). Leave the
   lock and the operation in place.
3. Fetch the authorized remote. Fetch updates remote-tracking refs only.
4. When the checked-out branch is not the integration branch, **stop**
   (`unexpected-branch`). When `HEAD` and fetched trunk have diverged,
   **stop** (`diverged`), including when the index or working tree also
   has a pending edit. Preserve that state. Do not merge, rebase, or reset.
5. When status is not clean — staged, unstaged, or untracked — **defer**
   (`pending-edit`). Preserve the exact index and working tree.
6. When `HEAD` contains commits fetched trunk does not, **defer**
   (`unpublished-commits`). Leave those commits in place.
7. When `HEAD` is the fetched trunk revision, report **already current**.
8. When the checkout is clean and `HEAD` is a strict ancestor of fetched
   trunk, fast-forward with a checkout-aware update:
   `git merge --ff-only <fetched-trunk>`.
   Do not use `update-ref`, `branch -f`, or a merge that creates a commit.
   Report **advanced** to that fetched revision.

Before using this checkout's commit as a new task base, run this attempt
and use the commit only when the result is **advanced** or **already
current**. When the result is **deferred** or **stopped**, do not treat
that commit as a fresh base. Start the new workspace from fetched remote
trunk unless the caller explicitly selected this checkout's unpublished
work, and do not advance the checkout to make that start possible.

## Direct edit

A bounded direct edit holds the same exclusive access for its whole
interval. The declared owner is this edit from the first read through
commit, publication, and release.

1. Establish access under
   [Establish access before local mutation](#establish-access-before-local-mutation).
   Re-read current state after that access. A pending edit, staged change,
   unpublished commit, ongoing operation, unexpected branch, diverged
   history, or ambiguous ownership stops the edit. Preserve that state.
   Do not stash or reset it to make room.
2. Change and commit only the authorized content.
3. Publish only that authorized commit through
   [publish the candidate](publish-the-candidate.md). The owned workspace
   is this checkout. The suffix is that commit. When any other unpublished
   commit would be reachable from the pushed SHA, stop under
   [Preserve pending local work](#preserve-pending-local-work) and do not
   push. Do not fast-forward the checkout as part of the push.
4. Release access after the commit and its publication attempt have
   finished or stopped with state preserved. Release does not discard the
   commit or an unpublished suffix left by a rejected push.
5. A later refresh re-reads the checkout after the release. It does not
   reuse the snapshot from the start of the edit.
