# Delegate a slice

Assign each planned slice, or the one quick slice, to a fresh implementation
agent. Use a general-purpose agent, or `gsd-executor` when this project uses
`/gsd-execute-phase`. Implement locally only for a single interactive slice.
The coordinator retains
[wrap-up](wrap-up.md); an execution tool does not take over that responsibility.

Give the agent:

- The selected execution checkout and branch. For planned execution, pass the
  complete retained execution identity; for quick execution, pass the location
  retained in the conversation. Require all implementation commands and edits
  to run there rather than relying on the agent's inherited working directory.
- The execution source and current slice with mapped promises and observations,
  including replacement and lifecycle obligations. For planned execution, pass
  the plan path and its selected-story or bounded-correction source. Also pass
  any relevant existing-solution finding and candidate evidence from the plan,
  plus new evidence that triggered a PFE revisit; a fresh agent does not repeat
  a still-valid search merely because delegation occurred. For a correction,
  pass its complete plan-owned
  [correction input](../../dough-story-refinement/references/planning.md#choose-the-planning-level)
  rather than requiring a seed. For quick execution, pass the canonical story,
  the explicit instruction to execute without slice planning, and the relevant
  conversation context; require no plan or substitute execution record. Omit
  unrelated plan or conversation history.
- Any North Star topic cited by the delegated work and the evidence supporting
  it. Require the agent to return contrary evidence through [execution
  decisions](execution-decisions.md#resolve-conflicting-recorded-direction),
  without changing the topic or continuing the affected path.
- For planned execution continuing an oversized quick attempt, the remaining-work
  plan plus the preserved completed work and proof and any incomplete-change
  disposition needed to identify the true starting boundary. Require the agent
  not to repeat completed compatible work or its unchanged proof. Treat the
  quick attempt and planned continuation as one execution, not two handoffs with
  independent histories.
- [Execution decisions](execution-decisions.md), this project's slice budget and
  exceptions, workflow precedence, and literal focused commands with the runtime
  wrapper. Require relevant proof; broaden testing only when the slice, project
  workflow, or human requires it.
- Ownership of the slice's changes. State that other agents may share the
  execution checkout and their work must be preserved.
- A stop before coordinator delivery: no commit, push, marking a planned slice
  done, refactor pass, selective formatting, or independent hook-owned lint
  command.
- The [CI pause and resume contract](ci-monitor.md#pause-and-resume-writers).

## Own verification to its terminal result

The agent that starts a required verification owns that command through its
terminal result. When verification yields a running command identity — for
example a backgrounded command whose result arrives later — the owning agent
follows the host's supported continuation until it observes the terminal
result. Yielding is valid and does not relinquish ownership. A completion
claim and its proof rest on the terminal observation, never on the launch
result or intervening progress output.

If the command fails or its result becomes inaccessible, the agent returns an
explicit incomplete stop instead of progress reported as completion. The stop
names the known command state, the outstanding proof, and the recovery
ownership, and carries no completion marker. The agent retains the command
identity and known state in its ordinary handoff only when needed for
continuation or recovery.

## Retire owned watches when verification ends

A watch the owning agent created to learn a delegated command's result lives
only as long as the verification obligation. Prefer the host's existing
completion handling — the supported continuation under
[verification ownership](#own-verification-to-its-terminal-result) — over
creating an extra watch for the same command.

When the obligation ends — the terminal result is observed or an incomplete
stop is returned — account for the watch's unread evidence first: read any
pending notification or output so a delivered failure is not lost. Then
retire the watch through the host's supported control, so no avoidable watch
remains live for completed work. A notification already queued or in flight
may still arrive afterward; recognize it as a stale duplicate of the
accounted evidence, and do not restart verification or rerun the command
for it.

This decision covers only watches the owning agent created for its own
command. Unrelated watches belong to their owners; in particular the
coordinator's [CI observer](ci-monitor.md) keeps its own lifecycle and is
never retired as part of command cleanup.

If the host offers no supported way to retire the watch, report the
limitation instead of claiming cleanup: name the live watch, the evidence
already accounted for, and what remains observable. Do not modify a host
adapter or add machinery to force cleanup.

## Return a targeted report with focused proof

Require uncommitted changes with passing focused proof, a stop requiring human
judgment, or an oversized-slice report under execution decisions. Require a
targeted return that gives the coordinator:

- the implemented outcome mapped to the slice promises;
- owned changed paths and the product or behavior boundaries they change;
- literal proof commands and concrete observation locations, including the
  relevant setup and assertions or signals;
- uncovered promises, contradictions, and other evidence gaps; and
- only consequential learnings that affect acceptance or remaining work.

Use source paths plus named tests, symbols, assertions, or signals as locations;
include a bounded excerpt only when the location cannot expose the decisive
evidence. Do not routinely attach the raw implementation trace, full command
logs, or a duplicate full diff. The return is an index into inspectable work and
evidence, not proof that the coordinator has inspected or accepted them. An
implementation return does not establish slice completion.

For each passing focused command, use:

```text
proof:
  command: <literal complete focused command>
  covers: <observable behavior or paths covered>
  boundary: <product boundary exercised>
  observations:
    - <source/test path and named assertion or signal>: <what it observes>
  setup: <source/test path and setup supplying only the starting precondition>
  result: pass
```

Connect proof to the planned slice's or quick story's promises. Placeholders,
abbreviations, and paraphrases are ambiguous evidence. When no setup is needed,
say `none`; do not omit the field or mistake behavior supplied by a fixture for
product behavior. Report uncovered behavior as incomplete implementation; the
refactor pass must not supply missing behavior.

The `proof:` block is an example representation, not a required schema. An
equivalent layout that carries the same substantive evidence — the literal
complete command, what it covers, the boundary exercised, concrete observation
locations with their setup, and the result — is a complete report. Headings,
key casing, and field arrangement may vary; the substance may not. A report
missing that substance, or contradicting observed evidence, is incomplete
however closely it matches the example. The coordinator's
[proof acceptance](wrap-up.md#accept-proof) applies the same substance test.

Explicit completion markers with a separate workflow contract — such as
`## REFACTOR COMPLETE` and `## PAUSED FOR CI` — are not report formatting;
return them verbatim.

## Await delegated results without empty calls

When a delegated result is still pending, wait through the host's supported
notification, wait, or resume facility and continue on the meaningful state it
delivers. Do not issue no-op calls — such as a shell command run solely to
keep the turn alive — while waiting; they consume turns without advancing the
delegated work or its evidence.

Bounded state retrieval is not no-op polling: a limited check of a delegated
command's output or status to obtain real evidence remains useful, and
necessary progress communication to the human stays valid. Neither replaces a
supported wait facility when one exists. The delegated agent's
[verification ownership](#own-verification-to-its-terminal-result) is
unchanged; this decision covers only the coordinator's wait.

If the host offers no supported way to await the pending result, report the
exact limitation instead of inventing an API or assuming completion: name the
missing facility, the known state of the delegated work, and what recovery
needs. Do not copy another host's yield semantics, and do not treat ending
the turn or blocking indefinitely as a universal remedy.

CI observation keeps its own lifecycle under
[CI monitoring](ci-monitor.md); this decision does not change it.
