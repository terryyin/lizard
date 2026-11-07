# Resolve a publication rebase conflict

A conflict while rebasing the owned unpublished suffix is not permission to
take `--ours` or `--theirs`, skip the commit, or continue Git blindly.

The ordinary rebase in [Publish the candidate](publish-the-candidate.md#publish-the-candidate) step 3 is run through this project's
installed product backlog rebase adapter, not a raw `git rebase`, whenever it touches the product backlog
(often `PRODUCT-BACKLOG.md`) or the done records beside it; see [reconcile product backlog Git operations](../../dough-product-backlog/references/merge-conflicts.md)
for how to resolve and run it. Its own `conflict`/`refused`/`blocked` result already identifies the real
replayed commit, its parent, and the current destination from Git's own rebase state, never from
ours/theirs labels. Resolve every unmerged path it leaves — the backlog or a done record following that
reference, another product path as below — `git add` it, then run the adapter's own `continue` for this
same rebase — never a raw `git rebase --continue`. A clean
replay the adapter reports as `disputed` is not a Git conflict and has nothing staged to resolve the usual
way: repair the backlog by hand, or decide the current result should stand as is, then run the adapter's
own `validate` before this section's own revalidation below and before publishing. A `catalog-uncommitted`
result, whose report says the rebuilt done catalog is staged but not committed, is not a conflict: the
replay finished. Settle and commit that
catalog as that reference describes, then take the owned-branch tip as the candidate and revalidate as in
candidate step 4. If neither the adapter nor that reference is available, preserve the conflict and report
the missing guidance.

[Recover a rejected push](publish-the-candidate.md#recover-a-rejected-push) uses that same adapter for the owned-suffix range named there, not a raw `git rebase` and not a rebase of whatever branch happens to be checked out. The cutoff is the base that section retains for the push. Every adapter result that exits non-zero stops before the retry push and leaves the refs, worktree, and index as Git left them. Resolve or settle it the same way as the ordinary rebase above, resuming any conflict through this adapter's `continue`. If the adapter is unavailable, use [the fallback domain knowledge](../../dough-product-backlog/references/merge-conflicts.md#fallback-domain-knowledge) by hand exactly as below.

For other product or code paths, read the three Git versions (ancestor,
current side, and incoming side; index stages 1, 2, and 3). Identify the
fetched authorized remote target versus the unpublished suffix from the actual commits; Git's
ours/theirs labels during rebase do not name intent. Compare each side with
the ancestor and retain a brief account of what each contributor changed.

When both sides' intent is understood and compatible, combine those changes.
Apply identical edits once. An unchanged region does not override the other
side. Do not choose an entire side. Continue the rebase, through the
adapter's own `continue` when the adapter ran it, only after the
combined working tree matches that account, then revalidate as in candidate
step 4: the combined change invalidates only the affected proof. Rebase
success is not behavioral proof. Do not publish until that recheck succeeds.

If identity, incompatible product intent, or a competing restriction remains
unresolved, preserve the exact refs, worktree, and index, including conflict
markers. Do not discard either side. Stop publication and report the specific
missing decision:

- Unclear value, domain meaning, architecture, or ambiguity that could
  waste a commit uses
  [human judgment](execution-decisions.md#stop-for-human-judgment).
- A change that would drop or weaken a required rejection or other
  contractual product constraint uses
  [a disputed plan restriction](execution-decisions.md#resolve-a-disputed-plan-restriction).

Do not register publication or continue as delivered. After a human
decision, resume from the preserved conflict state rather than inventing a
side.
