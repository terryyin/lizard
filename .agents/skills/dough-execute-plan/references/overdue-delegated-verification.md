# Check an overdue delegated verification

When a delegated verification is still pending and either its expected proof
window is exceeded or remaining applicable CI observation coverage calls for a
check before that coverage lapses, obtain bounded liveness evidence. Do not wait
indefinitely on completion notifications alone, and do not wait for a human
status request to begin the check.

An expectation is a reason to inspect, not a new command timeout and not an
automatic interrupt or failure. A slow command that still shows fresh progress
stays under its existing owner through
[verification ownership](delegation.md#own-verification-to-its-terminal-result).
Reuse the retained decision under
[choose expensive proof before launch](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch)
— host, workload, evidence and unknowns, and reconsideration condition — when
judging what “expected” means for that work. Do not invent a universal duration
or a second process supervisor.

With the check, retain the command identity, owning actor, host/checkout,
process or supported continuation handle, last meaningful progress, and
applicable CI observer identity and coverage. Queue state, live progress, a
supported yield, and a suspected stall are observations that inform the same
next action — not independent handlers or hard-coded duration tiers:

- **Queued.** Queue evidence exists but no process has started. Record the queue
  state and the next bounded check or owner. Do not classify an absent process
  as a stuck worker.
- **Yielded with a supported continuation.** Follow the host's supported
  continuation under verification ownership and continue that exact command to
  its terminal result. Progress alone is not completion. Do not restart or
  duplicate the command solely because it outlasted the expected window.
- **Suspected non-yielding or stuck.** Process or progress evidence supports a
  stall; do not infer stuck from silence or CPU use alone. Diagnose within the
  bounded check and recover only within established command or process
  ownership, or hand back the exact unfinished proof with its recovery owner.
- **Inaccessible.** Available host controls cannot inspect or safely stop the
  work. End the bounded check naming the known state, the missing observation,
  and the recovery owner. Do not invent a PID, an unsupported host API, or an
  unowned termination.

When duration, process state, or observer coverage is unknown, retain that
uncertainty and choose a bounded next observation rather than waiting
indefinitely or inventing an external timeout policy. Existing authority
decides recovery and any human action.

CI observation keeps its own lifecycle under
[CI monitoring](ci-monitor.md). If the observer reports that its budget ended
or coverage is otherwise lost — including `CI_MONITOR_UNAVAILABLE` — report
lost coverage and outstanding local proof separately. Preserve any accepted
local evidence. Command recovery does not manufacture restored CI coverage,
invent an observer restart, or treat lost observation as local proof success.
Do not repurpose CI worker liveness as a supervisor of the delegated local
command.

Inspect and recover only the identified owned command. Leave unrelated
processes with their owners. This extends the coordinator wait under
[Await delegated results without empty calls](delegation.md#await-delegated-results-without-empty-calls);
it does not change verification ownership, watch retirement, or
[interrupted-delegation recovery](interrupted-delegation.md#recover-an-interrupted-delegated-result).
