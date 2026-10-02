# Choose the execution location

Planned and planless work default to Story Branch Mode: one execution branch and
Git worktree for the selected work. Explicit `--trunk` uses Trunk Mode: still
one retained local execution branch and worktree, with claim startup through
[Take or admit work](../SKILL.md#take-or-admit-work) and increment publication as
in [trunk publication](trunk-publication.md). Explicit caller selection uses the
current branch instead. Story Branch and Trunk Mode select or reuse the owned
workspace and publish the claim through that installed startup operation.
Caller-selected current-branch
work commits its claim on the current checkout, which is never published.
An accepted independent mission, including contextual planless work, is
[admitted](admit-accepted-work.md) through that same startup operation.
Explicitly selected [one-shot work](one-shot.md), the only Story Branch or
Trunk Mode start without a claim, prepares its workspace there too.

Select or create that workspace through
[own a temporary exploration workspace](../../dough-manual-testing/references/exploration-workspace.md)
"Select the checkout" and
"Record local checkout role and target selection". Do not duplicate that
recipe here. This execution chooses when selection runs and which verified
base it supplies:

- Queued or admitted Story Branch and Trunk Mode claims supply the owned
  workspace path and authority to the installed startup operation.
  It uses fetched remote trunk as the base, confirms the Taken claim there, and
  returns a compact result with the accepted claim SHA and recovery coordinates
  for the workspace you supplied. Without an integration checkout, supply an
  existing owned worktree, or the repository context a new one is created
  from, as [Take or admit work](../SKILL.md#take-or-admit-work) describes.
  A conflicting or ambiguous claim stops implementation; identical **Taken**
  text alone proves no ownership.
- [One-shot work](one-shot.md) supplies the same path with `--one-shot` and
  workspace authority alone; the operation bases the workspace on fetched
  remote trunk and publishes nothing. With `--default-main` it instead takes
  the default checkout on trunk exactly as it is, creating no worktree or
  branch, as [work in the default checkout](one-shot.md#work-in-the-default-checkout)
  describes.
- Caller-selected current-branch work records that checkout and creates no
  worktree. An already-supported host-owned execution stays in that same
  recorded checkout and does not switch branches. Publication follows the
  caller's established authority in
  [slice wrap-up](wrap-up.md#deliver-the-change). Codex, Cursor, and Claude
  keep the checkout and authorized target their adapters already record.

Queued, admitted, and isolated one-shot starts require a linked Git worktree
when selecting an existing owned workspace. The repository's main worktree is
refused before fetch or parking carried edits. Explicit `--default-main` and
established continuations retain their own contracts.

If selection stops, preserve and report any partial workspace. Do not publish
a claim from it and do not start implementation. The shared lifecycle does not
publish the claim or refresh the default checkout; those stay with
[trunk publication](trunk-publication.md) and
[maintain the default checkout](maintain-default-checkout.md).

Record the local checkout role and target selection through that reference,
using the actual established paths. The originating checkout path, the
execution checkout path and branch, and the integration checkout path, when
one exists, are separate local roles. The authorized remote target is target
selection and is not one of those paths.

After the selected checkout exists, and after a claim's SHA is confirmed
on the authorized remote when this execution publishes one, prepare the checkout
as part of the same setup lifecycle so this project's ordinary commands are
usable there before implementation delegation. The same readiness rule applies
when caller-selected current-branch work newly supplies an unprepared checkout,
and when one-shot work has no claim to publish first.

Resolve the required setup from this project's checked-in conventions and
locked dependency metadata, not from an Open Dough configuration key. When
those sources establish a deterministic locked install, perform it in the
selected checkout without rewriting lockfiles. A committed lockfile with
contributor or CI convention for `npm ci` is one such case; do not require
npm, or treat a lockfile's presence as an Open Dough recognizer, for a
project that uses different tooling. Then run an applicable project command
from that checkout. Do not infer availability from the presence or absence
of `node_modules` or a similar local directory. Do not copy or symlink
mutable installation from another checkout, and do not treat parent-directory
resolution as the contract. Keep mutable installed dependencies, generated
output, and project-local caches in the selected checkout. Supported
package-manager download or artifact caches may remain machine-level.

Creation, this preparation, and the command check are one setup lifecycle.
Execution may cross the implementation boundary only when any required command
succeeds from the selected checkout and, for a Story Branch or Trunk Mode
claim, that claim's SHA is on the authorized remote. A missing, ambiguous,
or failed required preparation stops before implementation delegation, formatting,
proof commands, or CI-readiness claims. A remote claim that already succeeded
stays published. Preserve the checkout and report its path, the published claim
SHA when one exists, the command selected or the missing convention, and the
failure needed for recovery. Host facilities may establish or invoke the same
project-owned outcome; they do not define a separate preparation policy.

Reuse that host-established outcome only when its evidence names this exact
selected checkout and the checkout's current locked dependency state, and an
applicable project command then succeeds there. Keep the evidence in the
current execution context and the command result; write no registry, stamp
file, or host-specific reuse policy. A host callback may supply evidence,
but it cannot redefine what prepared means. Preparation that names a
different checkout, a changed dependency state, or an unusable command is
not reused; parent-directory resolution, a copied installation, or a
symlink is not reuse evidence. In those cases perform this project's setup
in the selected checkout and run the command as above. Verified reuse is
the same readiness gate, not a second preparation path.

[Runtime setup](runtime-setup.md) remains the owner of checkout-bound CI
observer runtime only. Do not arm observation as part of this gate, and do
not make CI setup the owner of development dependencies.

After that setup succeeds and before delegation, retain execution resume
context in the existing plan when one exists, and in the conversation, in
addition to the shared local checkout role and target selection:

- selected mode;
- retained published revisions — the accepted SHA and the target it was
  accepted on — when this execution has published any. This is the execution's
  review attribution in the existing plan or conversation, not a second ledger.
  Also retain the unpublished candidate SHA after a rewrite.
  [Trunk publication](trunk-publication.md) updates those fields; do not invent
  another ledger.

Caller-selected current-branch work records that checkout path as both the
execution checkout and the integration checkout and creates no worktree.
Host-owned execution uses that recorded checkout too. It is incompatible
with Trunk Mode; contradictory selection stops before setup.

On resume, verify the shared lifecycle's recorded worktree path, branch,
starting revision, and created-versus-reused ownership when that fact was
recorded, then verify mode, HEAD ancestry, retained published revisions, and
the unpublished candidate SHA against actual worktree state.
**Taken** alone supplies no location. Reuse a matching execution checkout
only when the claim rule above agrees this execution owns it.
Rewritten unpublished identities follow
[interrupted publication](trunk-publication.md#resume-an-interrupted-publication).
Missing, ambiguous, contradictory, unsafe, or partial identity/setup requires
an exact recovery decision: preserve resources rather than guessing, nesting
worktrees, or switching branches.

Run delegation, refactoring, generation, formatting, staging, commits, and
CI repair from the selected execution location. A validated increment and an
owned repair both publish through
[increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair),
which also names where that candidate is pushed.
[Maintain the default checkout](maintain-default-checkout.md) applies when
that checkout is mutated, including when the selected execution checkout is
that default checkout. Before using its commit as a new task base, verify
freshness with
[Refresh eligibility](maintain-default-checkout.md#refresh-eligibility)
and use that commit only when the result is advanced or already current.
An explicit contextual selection of this checkout's unpublished work keeps
that local base when refresh is deferred or stopped. That selection does not
grant publication authority.
Pass identity/location explicitly to agents and
host adapters.

Resolve checkout-bound installed runtime from the selected execution checkout
and use it as working directory. Arm only after the project-command readiness
gate above has passed. Before arming, apply
[runtime setup](runtime-setup.md) identity and stop rules; the initially loaded
skill's copy is not a fallback. CI source is the authorized target branch;
edits and repair stay in this checkout.
