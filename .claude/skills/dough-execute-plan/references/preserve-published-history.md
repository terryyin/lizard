# Preserve published history

[Publish the candidate](publish-the-candidate.md#publish-the-candidate) rebases
an unpublished suffix. A caller integrating an already-published tip supplies
that tip instead of a suffix, merged onto the fetched authorized target in the
owned workspace: a fast-forward when that tip already contains the fetched
target, otherwise a merge commit so both published histories remain. Do not
rebase those published commits. Where [agent commits](agent-commits.md) apply,
the merge commit is one. A merge that touches the product backlog or its done
records goes through the merge adapter of
[reconcile product backlog Git operations](../../dough-product-backlog/references/merge-conflicts.md).
Push, confirmation, and the receipt stay the candidate SHA and its target. A
rejected push recomputes this merge once onto the newly fetched target and
pushes once. A superseded candidate is not the receipt. Resume treats an
ancestor of the fetched target as already accepted and does not merge or push.

## Run the integration

Run this integration with the installed `history-preserving-publication.mjs`:

```sh
history-preserving-publication.mjs integrate \
  --workspace <owned-workspace> --published-tip <published-tip> \
  --branch <owned-branch> --target-ref refs/heads/<target-branch>
```

Add `--remote <remote>` for a remote other than `origin`. It performs the
fetch, the merge and its credit or the adapter merge, the exact push and the one
recomputed retry, then returns the workspace to its branch once accepted. Its
printed result names `classification`: `published` with the `receipt`,
`already-accepted` with nothing pushed, or `preserved` with its `reason` and the
state Git left. With a supplied dashboard landing context, add
`--landing-context <supplied absolute file>` under the shared
[landing handoff](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).

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
