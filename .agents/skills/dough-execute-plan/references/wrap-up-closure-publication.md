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

Resolve observation ownership before the first wrap-up publication: wrap-up
keeps the observer this coordinator armed for the execution and passes every
closure command the [owner input](trunk-publication.md#publish-an-execution-increment-or-repair)
its increments used. Recover that observer when it still exists; if its
observation already ended, use the existing setup to arm one observer from the
same execution checkout against the authorized target using
[CI observation](ci-monitor.md). Do not start an observer automatically,
retarget another mailbox, or use another coordinator's observer of the same
repository and target. Register each confirmed published SHA with this
coordinator's observer. An unavailable bridge, an owner that cannot be
verified, or a registration failure is lost coverage: report it truthfully
and continue without inventing successful observation.

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
  [--session-json <json>] [--coordinator <value> --observer-directory <directory>] \
  [--default-checkout <path>] [--repository <path>] \
  [--supplier-identity <identity> [--visit-settled <SHA>] [--backlog-path <path>]]
```

It publishes the final closure on this coordinator's own observer, runs
[the shared completion operation](ci-completion-wait.md#await-the-applicable-revision-at-completion)
for the accepted SHA, publishes the session record after a `success` receipt
with confirmed shutdown, refreshes any given default checkout, and then
retires the worktree and branch under Dough Land's
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree), which
removes the worktree only when its
[creation record](../../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it)
or another record there shows this work created it; trunk containment alone
does not. Pass `--identity` and `--created-for-work` as that section says; `--host`,
`--session-json`, `--coordinator`, `--observer-directory`, and `--remote` mean
what they mean for `deliver`. On Cursor and Claude Code the coordinator's own
tool supplies its identity, and `--session-json` names that same coordinator
from a call that lacks it, with the `agent_id` its observer was claimed with; on
Codex pass the observer note's coordinator and exact stream directory.
`finish` registers on, completes, and stops only the observer that owner
claimed. It prints one JSON line, exits 1 when `ok` is not true and 2 on a
usage error. An `integrated_state` success gates retirement as any success
does; report it as [completion mechanics](ci-completion-wait.md#completion-command-and-receipt-mechanics) describe.

Pass the closed work's identity as `--supplier-identity` under the
[visit rule](../../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit);
`--identity` never supplies it. Once the final closure is accepted, `finish` runs the
[current-consumer visit](../../dough-product-backlog/references/supplier-dependencies.md#visit-current-consumers-after-integration)
from the execution worktree against `acceptedSha`. A clean visit continues to
completion and retirement; any consumer, discovery problem or stopped visit
stops with `step: "visit"`. Judge each consumer from its `visit`, publish owned consumer changes from
that worktree through `deliver` on this coordinator's observer, and finish the
visit or explicitly retain its unresolved work. Then rerun `finish` with
`--final <acceptedSha>` and `--visit-settled <last accepted SHA>`, the last
consumer publication or `acceptedSha` when there was none. That rerun pushes
nothing, completes that SHA, and retires. Without `--visit-settled`, a rerun
repeats the visit only.

| Result                            | Act on it                                                                                                                                                                                                                                                                                                                             |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ok: true`                        | Keep `acceptedSha`, the `completion` receipt, `refresh`, and `cleanup` as operational facts; apply [completion attention](../../dough-land/SKILL.md#completion-attention)                                                                                                                                                             |
| `step: "before-cleanup"`          | Nothing was published. Publish the before-cleanup commit through `deliver` first                                                                                                                                                                                                                                                      |
| `step: "context"`                 | The worktree is gone. Follow its `recovery`: rerun with `--repository`, rerun with an earlier result's `acceptedSha` as `--final`, or report the unpublished final closure                                                                                                                                                            |
| `step: "conflict"` or `"publish"` | The final closure is unpublished; the worktree, branch, and both commits remain. Follow its `recovery`                                                                                                                                                                                                                                |
| `step: "record"`                  | The final closure is accepted and completed; its session record is unpublished. Follow its `recovery`                                                                                                                                                                                                                                 |
| `step: "visit"`                   | The final closure is accepted; completion and retirement have not run. Act on `visit` as above; when the visit stopped or `--visit-settled` was refused, follow `reason` and `recovery`. If an earlier `finish` printed a clean visit before the worktree disappeared, rerun with `--final <acceptedSha>` and `--visit-settled <acceptedSha>`. Otherwise report the unfinished visit. Resources stay |
| `step: "observation"`             | The final closure is accepted; none of this coordinator's observers completed it. Take the step `observation.reason` names, such as supplying this coordinator's own owner input or stopping its extra live observers, then rerun `finish`, or report lost coverage. Resources stay |
| `step: "completion"`              | Report the receipt: a CI failure, a receipt with no verdict, or retained or unconfirmed shutdown keeps the worktree and branch. Follow its `recovery`                                                                                                                                                                                 |
| `step: "retire"`                  | Act on `cleanup` as Dough Land's retirement result table says                                                                                                                                                                                                                                                                         |
| `step: "git"`                     | Report `error` as the unfinished step; nothing after it ran                                                                                                                                                                                                                                                                           |

For completion `pending`, use [completion mechanics](ci-completion-wait.md#completion-command-and-receipt-mechanics)'s
retained recovery, then rerun `finish` to continue closure. `completion_expired`
keeps closure unresolved; both outcomes retain the worktree and branch.

After an interruption, rerun the same command with the same owner input. Once
the worktree is gone, run it from an installed skills directory that still
exists, such as the default checkout's, adding `--repository` with the
`repository` an earlier result reported. A session that replaces the
coordinator [stops](ci-notify-hosts.md#stop-for-cancellation) the recorded
observer first. The rerun continues from the first unfinished step and pushes
no accepted closure again.

Retain the exact published closure SHAs, the receipt, remaining coverage, the
owner input, and `repository`, the management context a later rerun uses, in
available execution context. Apply [completion attention](../../dough-land/SKILL.md#completion-attention)
to the final response, then use the supplied launch context for the
[final dashboard operation](../../dough-land/references/dashboard-completion.md)
after all closure/retirement work and final wording settle.

## Finish a Dashboard admission from its owned claim checkout

When the Dashboard's launch reporting directory has an `admitted-claim.json`
beside its `landing-context.json`, the launch started in a borrowed checkout
and published the Taken claim from a separate owned checkout. Read its
`admission.workspace`, `admission.branch`, and `admission.publishedSha` once,
and keep the same identity and publication target. Keep using the selected
landing policy. Publish work from the native checkout through managed delivery
with its landing context, as for other admitted work.

Use the claim checkout for wrap-up. Advance it to the accepted before-cleanup
revision without rewriting local work, then complete the story and commit the
final closure as usual. For a no-change conclusion, the published claim can
serve as the before-cleanup revision. Run `finish` with the claim workspace
and branch, `--identity`, and `--admitted-launch-context` naming this launch's
`landing-context.json`. It verifies the claim, carries the accepted native
session evidence into the claim checkout, publishes the usual closure and
session record, and retires the claim checkout. It skips the borrowed checkout
if supplied as the optional default checkout to refresh. Keep the reporting
directory available for a retry of the same closure.

## Complete current-branch closure

After the last wrap-up publication this invocation will perform, invoke
[the shared completion operation](ci-completion-wait.md#await-the-applicable-revision-at-completion)
once for that final accepted SHA on this coordinator's observer. Handle its combined
CI and shutdown receipt, retaining the exact published closure SHAs, that
receipt, and remaining coverage. Apply [completion attention](../../dough-land/SKILL.md#completion-attention)
to the final response, then use the supplied launch context for the
[final dashboard operation](../../dough-land/references/dashboard-completion.md)
after all closure/retirement work and final wording settle. Invoke completion only after the final
applicable wrap-up publication, never between intermediate
recovery-record publications.

## Observe Story Branch integration

Story Branch wrap-up changes publication targets. Before integrating its saved,
published final-closure tip, close the observer bound to the remote execution
branch with
[the shared completion operation](ci-completion-wait.md#await-the-applicable-revision-at-completion)
for that tip when it is the last accepted registered revision. A green
execution-branch receipt covers only that branch and never releases later trunk
observation.

From the retained execution workspace, recover this coordinator's observer
already bound to the authorized trunk target or use the existing setup to
start one there. Establish it before integration publication. Do not retarget the old
mailbox, register a revision against a differently targeted observer, or create
a duplicate observer for the same repository, target, and coordinator. When the
branch observer cannot complete, or the trunk bridge cannot be established,
retain explicit unavailable coverage; do not invent success or silently discard
an observer that may still own the checkout.

Publish the history-preserving integration with the installed
`history-preserving-publication.mjs integrate` command under
[Preserve published history](preserve-published-history.md).
When the launch supplied a dashboard landing context, pass it as
`--landing-context` and handle the printed `landing` under the
[landing handoff](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing)
before retirement.
After remote confirmation, register only the accepted integrated SHA, the
printed receipt, with the trunk observer. A saved branch tip or superseded merge
candidate is not that receipt. The caller passed `--supplier-identity` under the
[visit rule](../../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit)
and acts on the printed `visit` before completion:
publish its owned consumer changes from the same workspace through
[the common sequence](trunk-publication.md#publish-the-candidate) to the trunk
target, register each accepted SHA with that trunk observer, and finish or
explicitly retain the visit. Then invoke
[the shared completion operation](ci-completion-wait.md#await-the-applicable-revision-at-completion)
once for the final accepted SHA, the integrated SHA or the last consumer
publication, on that trunk observer and handle its combined receipt. With
a `success` receipt with confirmed shutdown as its gate, wrap-up then retires
the execution resources under the same
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree).
After retirement and final wording settle, apply the
[final dashboard operation](../../dough-land/references/dashboard-completion.md)
with supplied launch context. It does not replace the CI receipt or shutdown gate.
Publication recovery retains the same observer and repeats neither target
setup nor an already accepted push.
