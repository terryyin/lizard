# Finish or stop

On a human-judgment stop or cancellation, handle delivered failures, then close
the observer through the current host adapter without waiting for CI and report
pending CI as unobserved. On normal completion, use
[the one bounded completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
and handle its combined CI and shutdown receipt before the final handoff.

After all planned slices satisfy proof/delivery, `--skip-retro` skips only this
execution's automatic retrospective; it changes no preferences or
proof/delivery/completion obligations. Explicit omit/defer instructions also
take precedence. When skipped, publish the
[execution-complete record](#record-execution-completion) with
`Product advice: retrospective skipped`, then invoke the completion operation
for the applicable accepted revision at this execution-completion boundary,
handle its receipt, report the recorded product advice and the CI verdict or
exact unresolved reason with shutdown evidence, and end with
`## PLAN EXECUTION COMPLETE`, retaining the completed plan and judged proof for
any later review and wrap-up.

Otherwise invoke
[dough-execution-retrospective](../../dough-execution-retrospective/SKILL.md)
without another confirmation as soon as delivered implementation is available,
including while applicable CI is pending. Preserve explicit review instructions
and project preferences through its review selection; its authority excludes
implementing findings or changing the backlog. Do not emit
`## PLAN EXECUTION COMPLETE` first or require that banner as retrospective
input. When review returns, publish the
[execution-complete record](#record-execution-completion), invoke the
completion operation, handle its receipt, then report completion, judged proof,
the recorded product advice, CI verdict or limitation, shutdown evidence, and
`## PLAN EXECUTION COMPLETE` as the final execution/review handoff.

Continue in the recorded execution project/checkout. Supply available references/context:
source contract, original plan and approved changes, attributable commits, decisions, proof,
delivery state, CI limitations, and checkout/branch identity. Supply the execution checkout
path as the retrospective's write location, so its process findings and correction stories
and plans stay with this execution's changes and enter its completion commit. Trunk Mode
attributable commits are that identity's retained published revisions, not another ledger or a
rewrite's unpublished SHA. Include an initial quick attempt and its planned
continuation as one execution. Pass existing context to retrospective, which
validates attribution and follows its
[process-review evidence rules](../../dough-execution-retrospective/SKILL.md#review-process-only-from-a-real-record).

Execution completion and review completion are distinct. A retrospective context
stop leaves implementation delivered but the final execution/review handoff
incomplete: preserve its partial review, observer, applicable target/revision,
and CI state, and report the missing input without rerunning implementation or
claiming review completion. It writes no execution-complete record. On
recovery, use retained review state to continue or recognize completed review;
ambiguous state requires recovery rather than duplicate review or guessed
completion. A CI failure handled through an authorized repair invalidates only
the conclusions affected by changed code; resume those conclusions instead of
restarting the full retrospective.

When process history is unavailable, report the review limitation and continue
independently supported outcome review and delivery.

Retain the completed plan, proof decisions, execution checkout, branch, and
worktree for story wrap-up; do not invoke it here. Wholly planless completion
retains source, conversation, identity, delivered changes, and proof, invokes
the completion operation when
publication created that CI obligation,
reports delivered work, the CI verdict or exact unresolved reason, and shutdown,
and ends with `## QUICK EXECUTION COMPLETE` after required delivery/completion,
without automatic retrospective. The coordinator invokes wrap-up after successful branch delivery. Do not
report integrated completion here; wrap-up owns Story Branch merge and required
target push, Trunk Mode closure publication, and resource cleanup.

For incomplete work, failed delivery/shutdown, cancellation, or a human-judgment stop, report
source, active plan/next slice or quick-slice state, preserved work, observer state,
and required decision/recovery action. An applicable CI failure that existing
handling cannot resolve is such an incomplete stop, even when review finished
and the observer remains available for diagnosis. Emit no completion marker or
automatic retrospective.

## Record execution completion

After the retrospective returns, or is skipped, add this record to the plan
after its ordered slices:

```markdown
## Execution complete

Product advice: <advice>
```

Take the advice from the review's product advice or reasoned no-change; it may
continue across following lines or list items until the next `## ` heading.
Write `retrospective skipped` when this execution skipped the retrospective,
and `product review skipped` when the review ran with `--skip-product`. Commit
the record together with the retrospective's records written in the execution
checkout — a correction story and plan, `DearDough.md` process findings — as one commit
under this project's commit conventions, made as an [agent commit](agent-commits.md)
when that reference applies. The commit changes records only, so
accepted slice proof still applies. Publish it through
[increment delivery](trunk-publication.md#publish-an-execution-increment-or-repair)
before the completion operation, so the completion wait covers it; do not start
another CI or publication path. A plan that already carries the record needs no
second record or commit on recovery.
