# Reconcile product backlog Git operations

Use the installed product backlog Git adapters from the start of an authorized
merge or rebase that combines changes to the backlog or to the records in
`done/` beside it. A clean Git result can still lose a concurrent change.
For adding, taking, placing, or completing an entry on the current branch,
follow the ordinary backlog-maintenance guidance.

## Resolve and run the installed adapters

Resolve this project's installed skill directory in the checkout performing the
operation. Use `.agents/skills/dough-product-backlog` for Codex and Cursor or
`.claude/skills/dough-product-backlog` for Claude Code. Run the matching script
under that directory's `scripts/`. Supply `--file` for a nondefault backlog path
and `--cwd` to select the checkout.

- Merge. `product-backlog-git-merge.mjs merge --ref <ref> [--file <path>]`.
  A ref already contained in the current branch stops as `blocked` when the
  current branch is ahead. There is no merge to continue.
- Rebase. `product-backlog-git-rebase.mjs rebase --ref <upstream> [--onto <newbase>] [--branch <branch>] [--file <path>]`.
  Git rebases the current branch or the one named by `--branch`. With
  `--onto`, `--ref` is the cutoff before the commits to replay and
  `<newbase>` is their destination. Without `--onto`, `--ref` names the
  destination.

The commands share backlog and done catalog drivers. Success prints only its receipt.
A stop also reports Git, driver and hook diagnostics. If the installed scripts
are unavailable or a conflict falls outside their scope, use the
[manual reconciliation rules](#fallback-domain-knowledge).

## A real conflict: resolve by hand, then continue through the same adapter

A `conflict`, `refused`, `refused-before-commit`, or `blocked` result leaves
the affected Git state — refs, index, and worktree, including any unrelated
conflicted path — exactly as Git left it, fully recoverable. Resolve the
backlog's own unmerged path by hand using
[the domain knowledge below](#fallback-domain-knowledge), `git add` the
resolved file, then run the same adapter's own `continue` verb (for example
`product-backlog-git-merge.mjs continue [--file <path>]`).
The adapter's `continue` validates what is actually staged for the backlog
against its own invariants before it lets Git proceed; it never re-runs
reconciliation over a decision a human's resolution may deliberately differ
from. An unresolved path, or a staged candidate that fails validation, stays
stopped exactly where it was.

## A clean but disputed result: validate, do not re-decide

After a clean replay, the command compares the result with reconciliation of
the branch's original tip and original destination. A `disputed` result leaves
the replayed commits on the local branch. Repair the backlog or accept the
current bytes, then run `product-backlog-git-rebase.mjs validate [--file <path>]`
before continuing or publishing. This read-only check verifies that the current
backlog can be read. A merge checks its candidate before committing.

## Done records and the done catalog beside the backlog

Every adapter keeps the done catalog `done/.catalog.json` current and never
stops on the catalog alone. When an operation changes `done/`, the merge
commit includes the rebuilt catalog. An accepted rebase adds a catalog-only
tip commit when the rebuilt catalog differs from the replayed one. That
commit credits the developer in an agent's owned workspace. A stopped operation
rebuilds the catalog when `continue` accepts the resolution. A disputed replay
leaves the catalog as replayed.

Done records under `done/` beside the backlog are one file per identity, so
combine them as ordinary files: keep each side's added, replaced, or removed
record, and preserve a real conflict in one record for the human who owns that
work. Resolve it, `git add` the record, and run `continue` through the same
adapter, which then rebuilds the catalog from the resolved records. The catalog
is derived from those records and is never combined by hand.

Agent profiles stay in `agents/` beside the backlog; none belongs in `done/`.
When a resolution leaves an agent profile staged under `done/` — for example
by accepting Git's suggestion to move a live profile there after the other
side released the last profile in `agents/` — the merge adapter's `continue`
answers `refused` and leaves the merge uncommitted, naming each misplaced path
and the `agents/` path it belongs at. Move each profile back to that path,
`git add` both paths, and run `continue` again. Publication holds a candidate
that commits a profile under `done/` the same way, naming the same paths and
pushing nothing.

A rebase whose catalog commit cannot be made, such as in an
agent's owned workspace whose developer credit is refused, reports
`catalog-uncommitted`: the operation has finished and the rebuilt catalog is
staged but not committed. Settle what the report names, then commit that
catalog alone, crediting the developer as any other agent commit does.

### Catalog representation and repair

The done catalog lists each done record's file name, identity, completion time,
and Git blob hash, ordered newest first by completion time. Unreadable record
files have their hash and no completion time, so a reader can order the records
and read only the ones it needs. Titles, developers, agents, hosts, and models
stay in the records. The catalog is derived from the record files: never edit or merge it
by hand. `complete` and the installed merge and rebase adapters
keep it current, rebuilding it from the resulting records whenever they change.
Rebuild with the installed `scripts/product-backlog.mjs catalog-done
[--file <backlog>]` from the same installed skill directory and commit its result
when:

- done records exist beside the backlog with no catalog because they predate it;
- record files changed outside `complete` and those adapters, such as by raw Git,
  a hand edit, or an older installed copy of these scripts; or
- raw Git reports a conflict in `done/.catalog.json`: settle the record files
  first, then rebuild and stage the catalog.

`catalog-done` changes no record and no backlog entry, prunes nothing, and
writes no completion; with no record left it removes the catalog. Its report
names unreadable record files, which stay for a person to read; the catalog
never makes up completion times for them.

## Fallback domain knowledge

Use this section by hand only when the installed adapters are genuinely
unavailable or do not cover the conflict, and when resolving a real conflict
the adapters already stopped (above). It states the same reconciliation rules
those adapters apply automatically; it is not a different or looser standard.

1. Before replacing conflict markers or staging the backlog, read its three Git
   versions: ancestor, current side, and incoming side (available in unmerged
   index stages 1, 2, and 3). Identify the actual branches/commits; during Story
   Branch integration, distinguish the integration target from the execution tip;
   during a Trunk Mode publication rebase, distinguish fetched trunk from the
   unpublished suffix. Git ours/theirs labels during rebase do not name intent.
   Compare each side with the ancestor and retain a brief per-identity account of
   changed membership, links, or ordering in the working context. Include sibling
   work, not just the story being wrapped up. Match work items by their recorded
   identity under [work item identity](identity.md), including known
   story-to-plan links; a shared seed ID is insufficient, and a side that only
   relinked a renamed or moved canonical home changed its navigation, not its
   identity. A shared link is insufficient too: two different recorded
   identities do not become one work item because their links have come to name
   the same canonical home, and which work such an entry is belongs to step 5.
   Consult affected history or canonical homes only if identity or intent is
   unclear. Use history when cleanup deleted a needed canonical home; missing
   artifacts alone do not prove completion.
2. Combine compatible changes from both sides. Apply identical changes once.
   An unchanged entry does not override the other side's take or removal.
   For example, taking A and removing completed B yields A in **Taken** and B
   absent. Different changes to the same work require compatible intentions;
   neither removal nor a later lifecycle state automatically wins.
   For concurrent closures, ancestor **Taken** = [A, B], target = [B], and
   execution tip = [A] must resolve to an empty **Taken**: each side removed one
   item and left the other unchanged. Preserving unrelated sibling work during
   branch-local cleanup does not authorize restoring a sibling removed on the
   target. Do not choose an entire side or union the surviving entries.
3. Preserve unrelated titles, links, direction text, and queue order. Retain
   compatible explicit reprioritization; taking or removing work does not
   reprioritize the remaining queue. An entry that only shifts position behind
   work a side took or removed keeps its priority; a reprioritization is a
   change to an entry's place among the entries around it.
   When reconciling a retrospective follow-up queued by wrap-up, apply
   [wrap-up's follow-up priority](../../dough-story-wrap-up/references/follow-up-disposition.md#preserve-follow-up-priority-during-reconciliation)
   to resolve competition with another newly added top item. Identify that
   follow-up from the available wrap-up context, not from branch labels or a
   generic assumption that every new item takes priority.
4. In **Taken**, retain surviving existing entries in order, then append new
   entries while preserving each side's addition order. Unless the project
   supplies a convention, interleave concurrent additions by repeatedly choosing
   the lexically smallest established identity among the next entries from each
   side, emitting each identity once. Do not use this rule for queue priority.
5. If identity, incompatible changes, or competing order remains unresolved,
   preserve the conflict and ask the human for the specific missing decision.
   For example, one side removing B while the other side gives B a different
   place in the queue requires a decision: removing work and reprioritizing it
   are different intentions, and neither wins because available context does
   not establish which one applies.
6. Before completing the merge or continuing the rebase, inspect the
   staged backlog against the per-identity changes from step 1. Verify both sides'
   intended changes are represented, each active identity appears once across
   both lists, every compatible removal stays absent, and references remain
   coherent. Correct a failed check before continuing. Retain both section headings
   even when empty. Report both sides' resolved transitions briefly, including sibling removals, and
   continue the calling workflow; backlog-only resolution needs no implementation
   test run.
