# Publish wrap-up closure

Story wrap-up treats each owned wrap-up commit on the execution checkout as a
verified increment whose target remains the authorized remote trunk. Publish
the before-cleanup commit immediately through [the common sequence](trunk-publication.md#publish-the-candidate)
before the next wrap-up mutation that depends on its recovery from shared
trunk. That closure target is not the Story Branch destination of
[execution increments](trunk-publication.md#publish-an-execution-increment-or-repair).
Do not merge the execution branch. A caller-selected current-branch closure
publishes each closure commit with the checkout's `HEAD` from before that
commit as its previously published base, and handles an `unpublished-base` stop
as [current-branch publication](trunk-publication.md#publish-an-execution-increment-or-repair) does.

Resolve observation ownership before the first wrap-up publication: recover
the matching execution observer when it still exists; if observation already
ended, use the existing setup to arm one observer from the same execution
checkout against the authorized target using [CI observation](ci-monitor.md).
Do not start an observer automatically or retarget another mailbox. Register
each confirmed published SHA with that observer. An unavailable bridge or
registration failure is lost coverage: report it truthfully and continue
without inventing successful observation.

A publication stop leaves the commit recoverable on the execution branch.
Do not delete spent history, remove resources, or claim closure.

## Finish Trunk Mode closure

Once the before-cleanup commit is accepted and the final-closure commit is
committed, run the installed closure command once from the coordinator's own
Bash or Shell tool, where `<installed>` is this project's installed skills
directory that holds `dough-story-wrap-up` (normally `.agents/skills/` or
`.claude/skills/`):

```text
node <installed>/dough-story-wrap-up/scripts/trunk-closure.mjs finish \
  --workspace <execution worktree> --branch <execution branch> \
  --before-cleanup <accepted before-cleanup SHA> --final <final-closure SHA> \
  --previously-published-base <accepted before-cleanup SHA> \
  --target-ref refs/heads/<branch> --repo <owner/repo> --host <host> \
  [--remote <remote>] [--identity <work identity>] [--created-for-work] \
  [--session-json <json>] [--default-checkout <path>] [--repository <path>]
```

It confirms the fetched target contains the before-cleanup commit, publishes
the final closure through managed delivery on the recovered or armed observer,
runs [the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
once for the accepted SHA, attempts the default-checkout refresh when one is
given, and only after a receipt whose shutdown is confirmed retires the
worktree and branch under Dough Land's
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree), which
removes the worktree only when its
[creation record](../../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it)
or another record there shows this work created it; trunk containment alone
does not. Pass `--identity` and `--created-for-work` as that section says; `--host`,
`--session-json`, and `--remote` mean what they mean for `deliver`. It prints
one JSON line, exits 1 when `ok` is not true and 2 on a usage error:

| Result | Act on it |
| --- | --- |
| `ok: true` | Report `acceptedSha`, the `completion` receipt, `refresh`, and `cleanup` |
| `step: "before-cleanup"` | Nothing was published. Publish the before-cleanup commit through `deliver` first |
| `step: "context"` | The worktree is gone. Follow its `recovery`: rerun with `--repository`, rerun with an earlier result's `acceptedSha` as `--final`, or report the unpublished final closure |
| `step: "conflict"` or `"publish"` | The final closure is unpublished; the worktree, branch, and both commits remain. Follow its `recovery` |
| `step: "observation"` | The final closure is accepted without an observer. Report lost coverage; resources stay |
| `step: "completion"` | Report the receipt: a CI failure or retained or unconfirmed shutdown keeps the worktree and branch |
| `step: "retire"` | Act on `cleanup` as Dough Land's retirement result table says |
| `step: "git"` | Report `error` as the unfinished step; nothing after it ran |

After an interruption, rerun the same command. Once the worktree is gone, run
it from an installed skills directory that still exists, such as the default
checkout's, adding `--repository` with the `repository` an earlier result
reported. It continues from the first unfinished step: a final closure the
target already holds is not pushed again, completion is repeated on the observer
that covers it, and cleanup already done is reported as `already-absent`. A
final closure the target has moved past without conflict is rebased and
published once.

Report the exact published closure SHAs, the receipt, remaining coverage, and
`repository`, the management context a later rerun uses.

## Complete current-branch closure

After the last wrap-up publication this invocation will perform, invoke
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
once for that final accepted SHA on the matching observer. Handle its combined
CI and shutdown receipt, then report the exact published closure SHAs, that
receipt, and remaining coverage. Invoke completion only after the final
applicable wrap-up publication, never between intermediate
recovery-record publications.

## Observe Story Branch integration

Story Branch wrap-up changes publication targets. Before integrating its saved,
published final-closure tip, close the observer bound to the remote execution
branch with
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
for that tip when it is the last accepted registered revision. A green
execution-branch receipt covers only that branch and never releases later trunk
observation.

From the retained execution workspace, recover one matching observer already
bound to the authorized trunk target or use the existing setup to start one
there. Establish it before integration publication. Do not retarget the old
mailbox, register a revision against a differently targeted observer, or create
a duplicate observer for the same repository, target, and coordinator. When the
branch observer cannot complete, or the trunk bridge cannot be established,
retain explicit unavailable coverage; do not invent success or silently discard
an observer that may still own the checkout.

Publish the history-preserving integration through the common candidate
sequence. After remote confirmation, register only the accepted integrated SHA
with the trunk observer. A saved branch tip or superseded merge candidate is
not that receipt. Invoke
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
once for the accepted integrated SHA on that trunk observer and handle its
combined receipt. With confirmed shutdown as its gate, wrap-up then retires
the execution resources under the same
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree);
publication recovery retains the same observer and repeats neither target
setup nor an already accepted push.
