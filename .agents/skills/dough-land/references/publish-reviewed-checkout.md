# Publish the reviewed checkout

For a wholly unpublished suffix, apply
[publish the candidate](../../dough-execute-plan/references/publish-the-candidate.md)
from the checkout, for that suffix and target. If the story branch has an
already-published prefix, use
[preserve published history](../../dough-execute-plan/references/preserve-published-history.md)
instead: establish that the recorded published tip is an ancestor of the
complete reviewed checkout tip and pass that **complete tip** as the API's `publishedTip`.
This includes any local unpublished suffix above the published prefix; passing
only the older published tip would omit that work or mistake it for an accepted
rerun. Preserve all published SHAs and return to the same execution branch.
The default checkout is the owned workspace when it is the one landing; do not invent a second
sequence. The candidate's own changes must be the reviewed checkout content.
On each fetched target, before rewriting the candidate and before the retry
push after a rejection, run this candidate check from the landing checkout:

```sh
node <installed>/dough-land/scripts/queued-closure-check.mjs check --checkout <checkout> --remote <remote> --target-ref refs/heads/<branch>
```

The command fetches the target and checks every queued story the candidate
closes against that tip. It identifies closures from the candidate's merge
base with the fetched target: entries in that base's **Backlog list** that
are in neither list at `HEAD`. It needs no retained one-shot context.

| Result | Action |
| --- | --- |
| `clear` | Continue publication. An empty `closes` means no queued story was closed. |
| `ownership-changed` | Push nothing; keep the checkout, branch, index, and commit. Report `ownership` and `error`, with publication, refresh, and cleanup not done. Leave the story to that owner and the developer. |
| Other failure | Stop before pushing and report the error; preserve the checkout and Git state. |

For a history-preserving landing, use the installed
`publishHistoryPreservingCandidate` API from `history-preserving-publication.mjs`
with this same check in its `onFetchedTarget` callback before each reconciliation,
including a retry. The installed `checkQueuedClosures`
API in `queued-closure-check.mjs` accepts `{ checkout, remote, targetRef }`;
on a non-clear result return `{ status, fields: { ownership, error } }` so the
publisher preserves the candidate and reports those details before rewriting.
Supply the complete reviewed tip, original branch and authorized target to that publisher, with the original
launch's `landingContext` only for its first trunk publication. Keep affected
proof validation in `affectedCheck`, which runs on each prepared candidate,
as required by the shared publication contract.

Do not resolve a backlog reconciliation conflict by removing the other side's
entry. A rewrite onto a newer target rechecks only
proof the combined change affects. Direct landing starts no CI observer.
Preserve the caller's required CI duties and register only on an observer already
bound to that target; never retarget the execution observer for this landing.

A conflict, refusal, failed recheck, or second rejection stops here. Preserve
the checkout, branch, index, and whatever state Git left. Name the conflict or
contention, and report publication, refresh, and cleanup as not done. Do not
loop.

