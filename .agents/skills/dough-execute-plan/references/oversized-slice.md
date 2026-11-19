# Refine an oversized slice

## Track active time

Each slice gets the [15 active minutes](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
The installed slice clock decides how much of it a slice has used. It keeps
one record for the checkout, so run it from the execution checkout; each
command prints one JSON report with `elapsedMinutes`, `remainingMinutes`,
`overrun` and `storyOverruns`:

```sh
node '<installed-execute-plan>/scripts/slice-clock.mjs' start --identity '<identity>' --slice '<slice>'
node '<installed-execute-plan>/scripts/slice-clock.mjs' pause
node '<installed-execute-plan>/scripts/slice-clock.mjs' status
node '<installed-execute-plan>/scripts/slice-clock.mjs' run --log '<log>' --exit-file '<exit file>' -- <command>
```

`<identity>` is the story identity, or the quick execution's identity.
`<slice>` is the slice the coordinator names, normally the plan's slice number.

The coordinator owns `start` and `pause`:

- `start` when it dispatches a slice, resumes one, or begins one itself.
  For the first `start` in a new coordinator session, add
  `--after-ended-session`, whether resuming the same slice or starting another.
  It settles any running slice's open period and every open `run` at the
  checkout's last clock command, excludes the session gap, then starts the
  named slice. With nothing running it is ordinary `start`. Nothing detects
  an ended session automatically; active time after its last clock command is
  not counted, so the settled time can undercount.
  Starting a paused slice continues its elapsed time: a slice returned at 11
  elapsed minutes resumes with 4 remaining. Without the option, `start` is
  refused while another slice is running; pause that slice first.
- `pause` when the implementer returns, the slice is accepted, or execution
  stops for the coordinator or a human, including a CI repair pause. Review,
  the refactor pass and CI repair add no time to the paused slice; a repair's
  diagnosis keeps its own time in [rounds](diagnosis-rounds.md).
- Every brief and every resume, also a replacement implementer's, carries the
  `remainingMinutes` that `start` or `status` reports.
- A slice refined or retitled while unfinished keeps its time under the same
  `<slice>`. When it is renumbered, start it with `--continues '<old slice>'`.
- Start each slice split from an overrun slice with
  `--split-from '<overrun slice>'`; it starts at zero. `storyOverruns` counts
  the story's overruns; at 2 or more,
  [reassess the story boundary](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
- `start --identity '<identity>' --diagnosis '<name>'` starts the next round
  of a diagnosis in place of `--slice`. Its reports add `diagnosis`, `round`,
  `roundsAuthorized` and `cumulativeMinutes`; `pause`, `status` and `run`
  treat a running round as a slice. A round at the limit reports
  `overrun: true` without adding to `storyOverruns`. A start beyond the
  authorized rounds, 3 by default, is refused; `--rounds-authorized <n>`
  raises the total only on a human's authorization.
  [Diagnosis rounds](diagnosis-rounds.md) say when to start a round and when
  to stop.

The implementer runs each proof command (test, build, measurement) through
`run`, keeping its outputs and decisive artifacts under
[proof evidence](proof-evidence.md), and follows the
[host's proof wait](wait-for-proof.md). Time inside `run` is not counted.
Implementation, diagnosis, proof setup, cleanup and any command started
outside the clock count as active time. `status` gives the remaining time.

At the limit, a clock command's report carries `overrun: true` and the elapsed
minutes. On Claude Code and Cursor the installed hook also tells the agent
after its next tool call; on Codex the next clock command does. The
implementer then stops unfinished work and returns it with the clock's elapsed
time, and the coordinator [handles the overrun](#handle-the-overrun).

Set an experiment's trial limit and decision criterion before it starts. Lack
of one coherent behavior or failure to converge also calls for refinement.

## Handle the overrun

When a slice reaches its limit while diagnosing a failure whose cause is
unknown, continue in [diagnosis rounds](diagnosis-rounds.md). The
disposition below applies to other overruns.

Inventory tracked and untracked changes owned by the attempt and preserve
pre-existing work. Never use broad `git checkout .` or `git clean -fd`. Unclear
ownership requires [human judgment](execution-decisions.md#stop-for-human-judgment); do not guess,
silently revert unrelated work, or continue cleanup. Apply the same ownership
stop as [delivery staging](wrap-up.md#deliver-the-change) and
[resume](../SKILL.md#continue-or-recover-at-an-execution-boundary). A verified-result
[delivery](wrap-up.md#deliver-the-change) failure or
[integration](../../dough-story-wrap-up/SKILL.md#integrate-committed-story-branch-mode-closure)
failure is ordinary recovery, not this overrun.

When replanning is disabled, do not plan, refine, or retry. First write useful
evidence under this project's executable-plan root, resolved from this project
as in [slice planning](../../dough-slice-planning/SKILL.md#resolve-execution-context).
Do not invent a location or write a plan. Evidence may be prose, snippets, or a
patch; it need not run. Then remove only current attempt-owned unfinished changes
outside that folder. Leave no abandoned incomplete code or test work elsewhere.
Preserve unrelated work and earlier delivered slices; do not undo delivered
work. Existing Taken entries stay Taken; invent no story, plan, or backlog
entry. Report the stop under [Finish or stop](../SKILL.md#finish-or-stop).

When replanning is allowed, planned execution parks or reverts only unfinished
attempt-owned changes and records the clock's elapsed time, completed proof, and the
failed sizing assumption in the same plan. Invoke
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md) only
when learning escalation permits it. If refinement cannot produce a bounded
next slice within the cap, [stop or split](execution-decisions.md#stop-or-split-before-more-work).
Otherwise the coordinator delivers the revised plan, reports `reverted and refined`
with elapsed active time, and restarts from the plan on disk.

For quick execution, first make a safe stop in the conversation. Identify the
source, elapsed time, the failed sizing assumption, completed compatible
work and proof, and every incomplete attempt-owned change. Keep completed
compatible work and proof in place. Safely park or revert only incomplete
attempt-owned changes; do not discard completed work merely to give later slices
a clean starting point. Keep the same execution identity and backlog placement under
[Take or admit work](../SKILL.md#take-or-admit-work). A quick attempt that becomes
planned keeps its selected mode and checkout; Trunk Mode stays Trunk Mode in
the same worktree. Unclear ownership stops disposition and the dependent
planning path for human judgment. A [one-shot](one-shot.md) attempt instead
[escalates](one-shot.md#escalate-when-the-work-grows) into admission, which
gives it the identity to plan under.

After that stop, when the triggering instruction authorizes planning and
continued execution, use this project's
[ordinary slice planning](../../dough-slice-planning/SKILL.md) for remaining
work from the established source — the selected story or the sufficient
instruction. Replanning permission grants neither missing scope nor execution
authority. Transfer the source, relevant chat evidence, completed work and
proof, incomplete-change disposition, elapsed time, failed sizing assumption,
and retained identity into the ordinary plan as source, decisions, or
learnings needed for resume. Carry every reported gap, loss, limitation, and
interim behavior into the new plan's structured
[story obligations](story-obligations.md), with remaining-slice ownership;
a learning alone cannot carry it. Plan only the remaining work. Do not fabricate a
story, completed planned slices, already satisfied promises, a substitute
quick-execution record, or a second execution. Restart execute-plan from that
plan in the preserved checkout and mode; ordinary plan refinement remains
available before delegation. Reuse preserved proof while its boundary remains
unchanged.

If planning or continued execution is not authorized, or ordinary planning
returns a missing field or disputed decision, report the safe stop and that
exact need without creating the plan or fabricating a story. If evidence
changes the source scope or exposes a disputed constraint, use the existing human
decision path before planning the affected work; complexity alone does not
authorize a scope change.
