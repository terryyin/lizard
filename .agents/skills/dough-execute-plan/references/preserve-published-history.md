# Preserve published history

Apply the [shared publication preconditions](publish-the-candidate.md#preconditions)
to the complete reviewed tip, including any owned suffix above its published
prefix. In the owned workspace, fast-forward when that tip contains the fetched
target. Otherwise merge to preserve both histories. Apply
[agent commits](agent-commits.md) and the
[backlog merge adapter](../../dough-product-backlog/references/merge-conflicts.md)
when required. The shared [rejection recovery](publish-the-candidate.md#recover-a-rejected-push)
recomputes the merge once on the new target.

## Run the integration

Resolve `<installed>` to this project's installed skills directory and run
the integration from the owned workspace.

```sh
node <installed>/dough-execute-plan/scripts/history-preserving-publication.mjs integrate \
  --workspace <owned-workspace> --published-tip <published-tip> \
  --branch <owned-branch> --target-ref refs/heads/<target-branch>
```

Add `--remote <remote>` for a remote other than `origin`. Read `classification`.
`published` gives the exact accepted receipt. `already-accepted` means the
fetched target already contains the reviewed tip and the operation pushed
nothing. Both return the workspace to its branch once accepted. `preserved`
names its `reason` and leaves Git's remaining state in place. With a supplied
Dashboard landing context for its first trunk publication, add
`--landing-context <supplied absolute file>` under the shared
[landing handoff](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).

For completed selected work, add `--supplier-identity <identity>` under the
[visit rule](../../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit).
After acceptance, read `visit` for the
[current-consumer visit](../../dough-product-backlog/references/supplier-dependencies.md#visit-current-consumers-after-integration)
against the accepted SHA. A stopped visit leaves the acceptance intact.

A caller with a fetched-target ownership guard uses the shared
`publishHistoryPreservingCandidate` API in that installed module. Its
`onFetchedTarget({ attempt, candidate, remoteTip })` runs before reconciliation
on each fetched target, including the retry. Return `{ status, fields }` to
preserve the state and report that reason and details before rewriting or
pushing. Supply `affectedCheck(candidateSha)` for proof after preparation and
before each push, including after a recomputed merge.

## Resolve a conflicted integration

A `preserved` result whose `reason` is `conflict` names its `conflictedPaths`
and leaves the stopped merge in the workspace. First resolve and commit that
merge there: through
`agent-commit.mjs -F "$(git rev-parse --git-path MERGE_MSG)"` where agent
commits apply, or the merge adapter's `continue` for the backlog or its done
records. Then run the same `integrate` command again: it publishes that commit,
a merge of the fetched target's tip and the published tip, as the candidate.
When the target moved meanwhile, it sets that committed resolution aside and
computes the merge on the new tip: resolve the conflict again on that merge.

A `conflict` result with empty `conflictedPaths` and an entry in
`adapterStatuses` is the merge adapter's `blocked` or `refused` answer, where
no merge may be stopped: follow that status in place of those two steps.

A `preserved` result whose `reason` is `uncommitted-merge` found a merge in
progress with nothing left unmerged, and left it untouched. Commit that merge
the same way, then run the same command.

When that run fails before its push, the resolved merge stays checked out in
the workspace. Leave it there and run the same command again once the failure
is cleared: it takes that merge again.
