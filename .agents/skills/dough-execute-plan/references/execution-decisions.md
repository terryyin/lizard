# Execution decisions

Apply these decisions before delegation and after implementation and refactoring,
even when tests pass.

## Stop for human judgment

Stop when an unresolved decision concerns user value, a structural choice
constraining later slices or architecture, credentials or permissions, or
ambiguity that could waste a commit. Also stop when evidence changes the selected
story's beneficiary, outcome, evaluation, scope, or order relative to other
stories. Record the affected story and field; follow
[learning escalation](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices)
before refining more slices.

Explain the finding and required decision, then wait for the human. After a
completed slice, first deliver safe work under [wrap-up](wrap-up.md). A refactor
stop or unresolved proof failure prevents that delivery. Resolve routine naming,
placement, test choices, minor refactoring, and defects caused by the current
change within the authorized scope.

## Diagnose failed proof

For CI events, first use [CI observation and repair](ci-monitor.md#handle-a-notification).
For other failures, use focused diagnosis. Discount a failure as pre-existing,
unrelated, or environmental only with bounded evidence connecting its cause to
the affected proof. Record the cause, supporting observation, affected proof,
and remaining defect or disposition in the existing handoff or active plan.
Successful commands need no extra record.

A passing retry, repeated test name, or successful cleanup does not establish
cause or repair. Use a targeted retry only to test a stated explanation. On
recurrence, compare current conditions with the recorded cause; reuse evidence
when applicable while retaining any remaining defect. Stop diagnosis once the
evidence justifies a disposition. If uncertainty remains, report it and stop
for human judgment. A recorded explanation never waives required proof or the
CI repair protocol; an infrastructure finding cannot excuse a separate assertion
failure.

## Refine an oversized slice

Use this project's target, hard limit, exceptions, and repeated-overrun threshold
under [slice sizing](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
Track elapsed implementation, focused testing, and slice-local cleanup with the
host clock; exclude explicit CI repair pauses. Lack of one coherent behavior or
failure to converge also calls for refinement.

Inventory tracked and untracked changes owned by the attempt. Safely park or
revert only those changes; preserve pre-existing work. Never use broad
`git checkout .` or `git clean -fd`. Unclear ownership requires human judgment.
Record elapsed time, completed proof, and the failed sizing assumption in the
same plan. Invoke
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md) only
when learning escalation permits slice refinement. The coordinator commits and
pushes the updated plan. Report `reverted and refined`, elapsed time, and
whether the hard limit applied, then restart from the plan on disk.

## Handle an implementation commit

An implementation agent committing before coordinator delivery is a process
failure. Stop and report it. A soft reset of an unpushed, attempt-owned commit
is appropriate only when safe and permitted by the human's instructions;
otherwise preserve it for human judgment. Do not continue as if wrap-up passed.
