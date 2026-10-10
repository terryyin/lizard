# Retire an owned worktree

With supplied dashboard context, read [dashboard completion](dashboard-completion.md)
and retain its reporting command, instructions, and surviving working directory before removal.
Prepare attention files outside the checkout; reporting remains the final operation.

Retirement is not applicable when the landing checkout is the default
checkout: nothing is removed, and cleanup is recorded as not applicable.
Otherwise retire the worktree only when both gates hold, and let the command
below check them: the fetched target contains its work, and this work created the
worktree, which
[own a temporary exploration workspace](../../dough-manual-testing/references/exploration-workspace.md)
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
| `ok: true` | Record the worktree, branch, and any `remoteBranch` as `removed` or `already-absent`; apply [completion attention](../SKILL.md#completion-attention) |
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

