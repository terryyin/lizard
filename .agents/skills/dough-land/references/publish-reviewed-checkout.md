# Publish the reviewed checkout

Choose the publisher by the first row that fits the checkout:

| Checkout | Publisher |
| --- | --- |
| Completed selected work | Its [visit forms](#land-completed-selected-work) |
| Published prefix | [Preserve published history](../../dough-execute-plan/references/preserve-published-history.md) |
| A story-linked Dashboard launch or a story preparation | [Session close](#publish-a-session-close) |
| Any other wholly unpublished suffix | [Publish the candidate](../../dough-execute-plan/references/publish-the-candidate.md) for that suffix and target |

For a published prefix, establish that the recorded published tip is an
ancestor of the complete reviewed checkout tip, including any local unpublished
suffix above it, and pass that **complete tip** as the API's `publishedTip`.
Preserve all published SHAs and return to the same execution branch.
When the default checkout is the one landing, it is the owned workspace. The
candidate's own changes are the reviewed checkout content.

On each fetched target, before rewriting the candidate and before the retry
push after a rejection, run this candidate check from the landing checkout:

```sh
node <installed>/dough-land/scripts/queued-closure-check.mjs check --checkout <checkout> --remote <remote> --target-ref refs/heads/<branch>
```

It checks every queued story the candidate closes against the fetched target.

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

## Publish a session close

Call the installed `publishSessionClose` API from
`dough-execute-plan/scripts/session-record-publication.mjs` with the
`publishExecutionIncrement` request, the project's backlog path as
`backlogPath`, and `session: { workflow, story }`. `workflow` is
`dough-story-refinement` for a story preparation's landing and `ad-hoc`
otherwise. `story` is the story identity bound to the session, such as a
follow-up's Done story. The API publishes the work, then the session's record
as its own commit along the same route. Report `work` as the publication
result and `record.status`; accepted work stays in place whatever the record's
status. Other publishers write no session record; for ordinary reviewed work,
report that none was written.

A `published` record settles the close. Otherwise, including after an
interruption or an unanswered push such as `transport-timeout`, call
`resumeSessionClose` from the same module with the same `workspace`, `branch`,
`targetRef`, `backlogPath` and `session`, a Dashboard launch's own
`landingContext`, and the result's `record.record` as `record` when it holds
one. Otherwise the close resumes from the state it saved in the workspace for
the session its landing context or `session.sessionId` names; only when that
state is absent, pass the stopped
work's `candidate` as `candidateSha` with its `suffixBase`. It never publishes
accepted work again. Report its `record.status`, and `work` when present, and
act on the first row that fits:

| Result | Action |
| --- | --- |
| `record.publication.status` is `needs-validation` | Rerun with its `candidate` as `validatedCandidate` |
| `record.status` is `session-record-conflict` | Report it: another record is already published for this session; the accepted work stays |
| `record.status` is `stopped`, or `work` is not accepted | Report `work` and the error; resume again once it is resolved |

## Land completed selected work

When retained context establishes completed selected work with a recorded
identity, it takes the visit under the shared
[visit rule](../../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit),
even when its checkout shows no consumer; a generic landing or an unfinished
increment takes none. Publish with the installed `supplier-integration.mjs`
APIs, adding `supplierIdentity`: `publishSelectedSupplier` takes the
`publishExecutionIncrement` request for a wholly unpublished suffix, and
`integrateSelectedSupplier` takes the `publishHistoryPreservingCandidate`
request for a published prefix. Keep the same candidate check, proof check and
first-publication `landingContext`. Once the candidate is accepted, the result
also holds `visit`, the current-consumer visit from the landing checkout
against the accepted SHA; its receipt stands even when that visit stopped.
Reconciliation may rewrite the preserved outcome commit: cite its accepted
rewrite, not the pre-publication SHA, as supplier evidence. Publish consumer
changes afterwards with the plain publisher and no landing context. A rerun
of either API after acceptance pushes nothing and visits again.
