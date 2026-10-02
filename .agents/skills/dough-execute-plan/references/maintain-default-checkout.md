# Maintain the default checkout

Owns default-checkout local preservation and the independent maintenance
outcome for that checkout. Remote candidate acceptance is a separate fact
owned by [publish the candidate](publish-the-candidate.md). Callers supply
which checkout is the default or integration checkout and when they need
preservation or a refresh attempt. This reference does not publish or
replace a caller's disposition, cleanup, or CI policy.

## Independent maintenance outcome

Successful remote publication and default-checkout maintenance are
independently reportable. No maintenance result below erases an accepted
remote publication, leaves it unfinished, or authorizes another push. A
publication that never reached remote acceptance leaves any outstanding
maintenance obligation unchanged and does not authorize treating the remote
as updated.

When reporting, name the maintenance result separately from publication
acceptance:

- **advanced** — this attempt fast-forwarded an eligible clean checkout to fetched trunk.
- **already current** — the checkout is clean and its `HEAD` is that fetched revision.
- **deferred** — a pending human edit, staged or unstaged change, unpublished commit, or ongoing operation. Name the preserved `HEAD`, index, and working tree. A supplied checkout that could not be read or refreshed, such as a missing path or a failing Git command there, is **deferred** (`refresh-failed`) with the error.
- **stopped** — the checked-out branch is not the integration branch, or local history has diverged from fetched trunk. Preserve that state and name it.
- **not applicable** — no default checkout was supplied; there is nothing to refresh or preserve locally.

Owned-workspace publication records an inspection result and does not
fast-forward. Inspection reports **already current** only when the checkout
is clean and `HEAD` is the accepted remote revision, and **not applicable**
when no checkout was supplied. Every other state, including a checkout that
cannot be read, is **deferred**. That inspection is not the refresh decision below.
A caller attempts refresh only by following
[Refresh eligibility](#refresh-eligibility).

## Preserve pending local work

Dirty or ambiguous checkout state, or unrelated unpublished local commits,
stop a refresh of that checkout. A [direct edit](#direct-edit) stops for the
states it names. Preserve exact refs, worktrees, index, and staged/unstaged
content; do not stash, reset, unstage, revert, or silently include unrelated
work in a refresh or in a publication that mutates this checkout. These
checks do not gate commits, proof, or formatting in a separate owned
execution or preparation workspace.

Report the inspectable state. When the unpublished suffix lives on this
checkout, leave it recoverable and do not treat a remote that lacks it as
published. When the suffix lives in a separate
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
Publication success does not decide eligibility. Startup and preparation
apply steps 1 and 3–7 to a reused owned workspace after their own fetch;
startup checks its owned branch in place of the integration branch.

Inspect current checkout state on every refresh attempt. With no default
checkout supplied, report **not applicable** and stop. When the supplied path
is missing or a Git command there fails, report **deferred**
(`refresh-failed`) with the error, change nothing further, and leave the
caller's accepted publication and its later steps, such as retirement,
unaffected.

1. Re-read the working tree, index, `HEAD`, checked-out branch, and any
   in-progress operation. An `index.lock`, or an in-progress merge, rebase,
   cherry-pick, or revert, is **deferred** (`ongoing-operation`). Leave the
   lock and the operation in place.
2. Fetch the authorized remote. Fetch updates remote-tracking refs only.
3. When the checked-out branch is not the integration branch, **stop**
   (`unexpected-branch`). When `HEAD` and fetched trunk have diverged,
   **stop** (`diverged`), including when the index or working tree also
   has a pending edit. Preserve that state. Do not merge, rebase, or reset.
4. When status is not clean — staged, unstaged, or untracked — **defer**
   (`pending-edit`). Preserve the exact index and working tree.
5. When `HEAD` contains commits fetched trunk does not, **defer**
   (`unpublished-commits`). Leave those commits in place.
6. When `HEAD` is the fetched trunk revision, report **already current**.
7. When the checkout is clean and `HEAD` is a strict ancestor of fetched
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

A developer may explicitly select this checkout for a bounded edit. That
selection and the task's established authority are the edit's authority;
publication needs its separately established publication authority.

1. Re-read the working tree, index, `HEAD`, checked-out branch, and any
   in-progress operation. Unrelated staged content, an unpublished unrelated
   commit, an ongoing operation, or a branch other than the selected one
   stops the edit. Preserve that state. Do not stash or reset it to make room.
2. Change and commit only the authorized content. Unrelated unstaged edits
   stay out of the commit, byte for byte.
3. With publication authority, publish that commit from the default checkout
   through the installed `deliver` entry point, as
   [current-branch publication](trunk-publication.md#publish-an-execution-increment-or-repair)
   does, with the `HEAD` from before the edit's commit as
   `--previously-published-base`. Handle an `unpublished-base` stop as that
   section does, preserving the developer's work under
   [Preserve pending local work](#preserve-pending-local-work). Without
   publication authority, report the commit as pending publication.
4. Keep the selected checkout and branch throughout. A rejected push leaves
   the commit and any unpublished suffix in place.
5. A later refresh re-reads the checkout. It does not reuse the snapshot from
   the start of the edit.
