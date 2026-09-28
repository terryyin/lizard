# Reconcile product backlog Git operations

An authorized merge, rebase, or cherry-pick that combines two sides of this
project's product backlog (often `PRODUCT-BACKLOG.md`) across branches or
commits is not an ordinary same-branch edit. Run it through this project's
installed product backlog Git adapters; do not run a raw `git merge`,
`git rebase`, or `git cherry-pick` that touches the backlog path and then
decide what to do only if Git reports a conflict. A clean Git result can
still combine the backlog wrongly — an earlier step can silently absorb a
concurrent change that a later step then changes again, unopposed at every
single step — so the adapters must be in the invocation path itself, not
consulted only after the fact.

An ordinary same-branch edit to the backlog (adding, taking, placing, or
completing an entry on the branch already checked out, with nothing to
combine from another ref) is not one of these operations and needs none of
this file's guidance; follow this project's ordinary backlog-maintenance
guidance instead.

## Resolve and run the installed adapters

Resolve this project's installed skill directory inside the checkout actually
performing the Git operation, the same way other checkout-bound installed
skill tooling is resolved: normally `.agents/skills/dough-product-backlog`
for Codex/Cursor or `.claude/skills/dough-product-backlog` for Claude Code.
Do not reuse an installed directory resolved for another checkout. Each
adapter below lives under that directory's `scripts/` module — for example
`.../dough-product-backlog/scripts/product-backlog-git-merge.mjs`. If no
installed skill directory can be resolved there, or the resolved adapter
refuses for a reason this file does not cover (for example, a conflict in a
file that adapter does not own), use
[the fallback domain knowledge](#fallback-domain-knowledge) below instead.

Otherwise, run the operation actually being performed, supplying the
backlog's real path with `--file` when it is not this project's default:

- Merge: `product-backlog-git-merge.mjs merge --ref <ref> [--file <path>]`
- Rebase: `product-backlog-git-rebase.mjs rebase --ref <upstream> [--onto <newbase>] [--branch <branch>] [--file <path>]`
  Without `--onto`, this rebases the current branch onto `--ref`, or the
  named `--branch` when that branch is not the one checked out. With
  `--onto`, `--ref` is the cutoff and Git replays only commits after it onto
  `<newbase>`, including on the named `--branch` when one is given. A caller
  replaying an owned unpublished suffix passes that suffix's cutoff as
  `--ref`; which revision that cutoff is belongs to the caller.
  `--pre-rebase-tip` and `--destination-at-start` override only the
  whole-rebase aggregate endpoints. They do not change which commits are
  replayed. They default to that branch's tip and the onto (or `--ref`)
  destination.
- Cherry-pick: `product-backlog-git-cherry-pick.mjs pick --ref <rev[ rev...]>
  [--mainline <n>] [--file <path>]`

Each of these self-registers Git's own merge driver for the backlog path in
this checkout on first use, then performs the actual Git operation; it never
selects a side, aborts, resets, or repairs a conflict on its own.

On success an adapter prints only its receipt on stdout and nothing on stderr,
even when Git or this project's hooks printed messages along the way. Any
other outcome also passes on everything Git, the backlog merge driver, and the
hooks wrote to stderr during that run, so their diagnostics stay available when
the operation stops, refuses, or fails.

## A real conflict: resolve by hand, then continue through the same adapter

A `conflict`, `refused`, `refused-before-commit`, or `blocked` result leaves
the affected Git state — refs, index, and worktree, including any unrelated
conflicted path — exactly as Git left it, fully recoverable. Resolve the
backlog's own unmerged path by hand using
[the domain knowledge below](#fallback-domain-knowledge), `git add` the
resolved file, then run the same adapter's own `continue` verb (for example
`product-backlog-git-merge.mjs continue [--file <path>]`) — never a raw
`git merge --continue`/`git rebase --continue`/`git cherry-pick --continue`.
The adapter's `continue` validates what is actually staged for the backlog
against its own invariants before it lets Git proceed; it never re-runs
reconciliation over a decision a human's resolution may deliberately differ
from. An unresolved path, or a staged candidate that fails validation, stays
stopped exactly where it was.

Cherry-pick has two additional stop shapes with no merge/rebase analogue: a
picked merge commit missing `--mainline`, and Git's own "this step is now
empty" stop. Resolve an empty step only through this same adapter's `continue`
(supplying whatever the sequence needs), never a raw `git cherry-pick --skip`
or `--allow-empty`: a raw skip can silently cascade through every remaining
clean step of the same invocation, bypassing this gate for the rest of the
sequence with no further point to intervene.

## A clean but disputed result: validate, do not re-decide

A rebase or cherry-pick sequence that finishes with no Git conflict anywhere
on the backlog path is still checked once more, as a whole operation, before
being reported as accepted. A `disputed` result means that check found the
true pre-operation tip and the true destination-at-start disagree with the
actual result. This is not a Git conflict: there is nothing staged to
`git add` in the usual way, and the operation's own local commits are left
exactly where the operation finished them — on the branch, unpublished, and
fully recoverable.

Repair the backlog by hand to the intended state, or decide the current
result should stand as is; either way, run the same adapter's `validate` verb
(for example `product-backlog-git-rebase.mjs validate [--file <path>]`)
before the calling workflow continues or publishes. `validate` writes
nothing and never re-runs the disputed reconciliation; it only confirms the
branch's current bytes are a backlog this tooling can read. A merge has no
`validate` verb: a single merge's own result is already checked by its own
gate before it is ever offered for commit, so there is no separate
whole-operation aggregate to revisit afterward.

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
6. Before completing the merge or continuing the rebase/cherry-pick, inspect the
   staged backlog against the per-identity changes from step 1. Verify both sides'
   intended changes are represented, each active identity appears once across
   both lists, every compatible removal stays absent, and references remain
   coherent. Correct a failed check before continuing. Retain both section headings
   even when empty. Report both sides' resolved transitions briefly, including sibling removals, and
   continue the calling workflow; backlog-only resolution needs no implementation
   test run.
