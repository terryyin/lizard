---
name: dough-land
description: >-
  Dough Land lands everything in one reviewed checkout on the authorized remote
  trunk: commits all of its changes, reconciles and publishes them without
  force, and refreshes a supplied default checkout when safe. An owned worktree
  is retired with its branch once trunk contains its work; the default checkout
  stays in place.
  Use only on explicit invocation, such as "/dough-land", "use Dough Land",
  "land this worktree", or "land" for changes made on the default checkout, or
  when another skill's validated keep instruction links here. A casual
  "keep", "looks good", or approval does not invoke it.
  Hosted merges and pull requests are out of scope. With
  `--process-retrospective`, it also reviews the landing's own process and
  records supported findings in `DearDough.md`.
---

# Dough Land

Land one reviewed checkout, an owned worktree or the default checkout on the
target branch: commit everything in it, publish that onto the authorized remote
trunk, refresh a supplied default checkout when safe, and retire the worktree
when it is one. Apply [completion attention](#completion-attention) to the final
response. Publication, refresh, and cleanup are separate results.
A later step that stops or is deferred never undoes an earlier one.
With `--process-retrospective`, also
[review this landing's process](#review-this-landings-process).

Run only when the developer explicitly invokes Dough Land, or when a calling
skill's own validated keep instruction links here. Reviewing, approving,
pausing, or saying "keep" in passing does not start a landing. Do not open a
pull request, request a hosted merge, or discard anything; when the developer
wants one of those, stop and say that Dough Land does not do it.

## Resolve the checkout and target

Resolve both before any commit:

- **Landing checkout.** The checkout named by the invocation or its context:
  normally the current one, or the owned workspace a calling skill recorded.
  When the context says the change sits on the default checkout, that checkout
  is the one to land. Record its path and branch, whether this work created it
  or reused one another workflow owns, and its starting revision when known.
  Ownership comes from
  that context or the worktree's
  [creation record](../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it);
  do not infer it from a clean directory.
- **Default checkout.** The project's established checkout for ordinary work,
  when the context supplies one; the refresh step inspects it. Landing needs no
  default checkout. When the landing checkout is a worktree, the default
  checkout is a different one; when the change sits on the default checkout,
  it is the landing checkout and needs no separate refresh source.
- **Target.** The authorized remote target as `refs/heads/<branch>` on a named
  remote: the target selection already recorded for this worktree, or the one
  the project or developer authorizes. Default the branch to `main` only when
  neither the caller nor the project supplies one.

Stop before committing, and name the gap, when:

- no checkout is in context, or more than one candidate fits;
- the named default checkout is on another branch than the target; name both
  branches;
- the target is missing, contradictory, or not a branch on an authorized
  remote; or
- the checkout has an `index.lock` or an unfinished merge, rebase,
  cherry-pick, or revert. Name it; the developer resolves it, then reruns.

## Commit everything in the checkout

Everything in the checkout is the reviewed change, including unrelated files
and local commits the fetched target lacks. Stage all tracked, untracked, and
deleted paths (`git -C <checkout> add -A`) and commit them there with a
message describing the reviewed change, as an
[agent commit](../dough-execute-plan/references/agent-commits.md) when that
reference applies to the checkout, otherwise with plain `git commit`; its
refusal stops the landing before publication. When there is nothing to commit, create no commit; that is the
normal rerun case.

The owned unpublished suffix is every commit on the checkout's branch that
the fetched target does not contain. Its previously published base is the last
SHA this landing recorded as accepted, otherwise the checkout's recorded
starting revision, otherwise the merge base of the branch and the fetched
target (for the default checkout, its local HEAD before the commit when that
is contained in the fetched target).

A calling skill that must land only its own record checks, before linking
here, that the worktree holds nothing else. Dough Land does not pick paths out
of a worktree. A preparation keep stages its assignment's release in the
worktree before linking here, so it lands with the result; Dough Land itself
never adds or removes an assignment profile.

## Publish

Apply [publish the candidate](../dough-execute-plan/references/publish-the-candidate.md)
from the checkout, for that suffix and target; the default checkout is the
owned workspace when it is the one landing; do not invent a second
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

Do not resolve a backlog reconciliation conflict by removing the other side's
entry. A rewrite onto a newer target rechecks only
proof the combined change affects. Nothing is registered with a CI observer.

A conflict, refusal, failed recheck, or second rejection stops here. Preserve
the checkout, branch, index, and whatever state Git left. Name the conflict or
contention, and report publication, refresh, and cleanup as not done. Do not
loop.

## Visit consumers of completed selected work

After accepted publication and before retirement, apply the shared
[supplier dependency procedure](../dough-product-backlog/references/supplier-dependencies.md)
when retained context establishes a selected completed supplier. Preserve its
recoverable outcome before cleanup, publish directly justified consumer updates
through this authorized landing workflow, and report unresolved work. A generic
landing or an unfinished increment establishes no supplier completion.

## Review this landing's process

Only with `--process-retrospective`, after accepted publication and any
consumer visit, apply the shared
[process review of a run](../dough-execution-retrospective/references/process-review-of-a-run.md)
once per landing, with the landing checkout as the write location. Recorded
findings are uncommitted changes: continue through
[Commit everything](#commit-everything-in-the-checkout) and [Publish](#publish)
before refresh and retirement.

## Refresh the default checkout

After acceptance, when the landing checkout is the default checkout, it is
already at the accepted SHA: record the refresh as already current, or as a
fast-forward to the published trunk when the publication moved it. Otherwise
attempt
[Refresh eligibility](../dough-execute-plan/references/maintain-default-checkout.md#refresh-eligibility)
on the supplied default checkout for the landed remote and branch.
Record its
[maintenance result](../dough-execute-plan/references/maintain-default-checkout.md#independent-maintenance-outcome)
and reason separately from publication; apply [completion attention](#completion-attention)
when responding. No refresh result is a failed landing
or blocks retirement.

Another skill may apply this section on its own after its own accepted
publication.

## Retire the worktree

With supplied dashboard context, read [dashboard completion](references/dashboard-completion.md)
and retain its reporting command, instructions, and surviving working directory before removal.
Prepare attention files outside the checkout; reporting remains the final operation.

Retirement is not applicable when the landing checkout is the default
checkout: nothing is removed, and cleanup is recorded as not applicable.
Otherwise retire the worktree only when both gates hold, and let the command
below check them: the fetched target contains its work, and this work created the
worktree, which
[own a temporary exploration workspace](../dough-manual-testing/references/exploration-workspace.md)
"Close or retain it" establishes from the work's records. Containment alone
does not make a worktree this work's to remove.

Before removing anything, record the worktree's repository management
context, its shared Git directory
(`git -C <worktree> rev-parse --path-format=absolute --git-common-dir`). Give
it to the command below, and run any other Git command here from it
(`git -C <management context> ...`), not from the worktree or a default
checkout, so removing the worktree, even the repository's last one, leaves
them usable.

Run the installed retirement command, where `<installed>` is this project's
installed skills directory that holds `dough-land` (normally `.agents/skills/`
or `.claude/skills/`). It names every checkout it acts on, so run it as
written:

```text
node <installed>/dough-land/scripts/worktree-retirement.mjs retire \
  --repository <management context> --worktree <worktree> \
  --branch <worktree branch> --remote <remote> --target-ref refs/heads/<branch> \
  [--identity <work identity>] [--created-for-work] \
  [--remote-branch <remote branch> --contained <sha>]
```

- `--identity` is the identity of the work the worktree serves, such as its
  story identity (`SEED-NNN#slug`), when the context names one. A worktree
  whose creation record names that work is retired whichever session created
  it.
- Pass `--created-for-work` only when the work recorded that its workspace
  selection reported `created: true`, in the plan or the conversation, or the
  caller or developer states the worktree was created for this work. Do not
  infer it from a clean directory, a claim, or a preparation assignment.
- `--remote-branch` names a branch the worktree's work published separately on
  the same remote, such as a Story Branch execution branch; never the target
  branch itself. Pass `--contained` with each revision the target must hold
  before anything is removed, such as the integrated SHA the caller's receipt
  covers; repeat it for more than one.

The command fetches the target's remote, requires the fetched target to
contain the branch tip, any remote branch tip, and each `--contained`
revision, checks the worktree's ownership and state, removes the worktree,
safely deletes the local branch, and then deletes the remote branch. Remote
acceptance is what makes retirement safe; a default checkout being behind the
target, or absent, does not make the branch unmerged. It prints one JSON line:

| Result | Act on it |
| --- | --- |
| `ok: true` | Record the worktree, branch, and any `remoteBranch` as `removed` or `already-absent`; apply [completion attention](#completion-attention) |
| `reason: "unique unpublished work"` | The fetched target lacks the branch tip or a `--contained` revision. Retain everything and report it; the target lacks that work |
| `reason: "remote execution tip is not integrated"` | The fetched target lacks the remote branch tip. Retain the worktree and both branches and report the remote tip; it holds work the target does not |
| `reason: "remote branch is the target"` | Nothing ran. Name the separately published branch, never the target |
| `reason: "dirty checkout"`, `"ambiguous checkout"`, or `"another workspace"` | Retain and report path, branch, and reason; the developer decides |
| `reason: "created for other work: <work>"` | The worktree belongs to the named work. Retain it, even when you believed this work created it |
| `reason` naming a reused, host-owned, or unrecorded workspace | It stays with its owning workflow. Retain and report it; rerun with `--created-for-work` only from a record named above |
| `partial: true` | A removal was not verified. Report which of `worktree`, `branch`, and `remoteBranch` was done, then rerun |
| `reason: "git error"` | Report its `error` and the step as unfinished; retain what remains |
| Exit 2 | A usage error; nothing ran. Supply the missing input |

Every result other than `ok: true` exits 1; nothing beyond what it reports
was removed.

Accept already-absent resources on a rerun: once the worktree is gone, rerun
the command with the management context recorded before its removal; it
reports `already-absent` and needs no ownership fact. Never force-remove,
force-delete, or reset to make cleanup possible. A calling skill may add its
own gate, such as a completion receipt that must arrive first; retirement then
waits for that gate as well as containment and ownership, runs the command only
once the gate holds, and keeps every resource while any of them is missing.

Another skill may apply this section on its own after its own accepted
publication, with its own gate.

## Stop, rerun, and report

A stop keeps every resource recoverable (worktree, branch, commits, index, any
default checkout, and the remote as it is) and names the exact unfinished
step. A rerun reads real Git and remote state and continues from the first
unfinished step:

| State found | Continue with |
| --- | --- |
| Unfinished Git operation in the checkout | Stop and name it |
| Uncommitted changes | [Commit everything](#commit-everything-in-the-checkout) |
| Branch tip not contained in the fetched target | [Publish](#publish), through the publisher's [resume](../dough-execute-plan/references/publish-the-candidate.md#resume-an-interrupted-publication) |
| Tip already contained | Record it as accepted; push nothing. Then refresh, and retire a worktree |
| Worktree or branch already absent | Record it as already retired |

Never commit the same change twice, push an already accepted candidate again,
or create a replacement worktree.

Keep publication acceptance, refresh outcome, and verified resource disposition
as separate operational facts in the command results and available execution
context. Preserve the accepted SHA and target, refresh result and reason, and
cleanup paths and verified disposition for recovery; no new recap or record is
required solely to repeat them.

## Completion attention

After operations settle, read and apply the shared
[completion attention rule](references/completion-attention.md), including its
[final dashboard operation](references/dashboard-completion.md) with supplied reporting context.
