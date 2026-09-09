# Delegate a slice

Assign each slice to a fresh implementation agent. Use a general-purpose agent,
or `gsd-executor` when this project uses `/gsd-execute-phase`. Implement locally
only for a single interactive slice. The coordinator retains
[wrap-up](wrap-up.md); an execution tool does not take over that responsibility.

Give the agent:

- The plan path, current slice, and mapped promises and observations, including
  replacement and lifecycle obligations. Omit unrelated plan history.
- [Execution decisions](execution-decisions.md), this project's slice budget and
  exceptions, workflow precedence, and literal focused commands with the runtime
  wrapper. Require relevant proof; broaden testing only when the slice, project
  workflow, or human requires it.
- Ownership of the slice's changes. State that other agents share the checkout
  and their work must be preserved.
- A stop before coordinator delivery: no commit, push, marking the slice done,
  refactor pass, selective formatting, or independent hook-owned lint command.
- The [CI pause and resume contract](ci-monitor.md#pause-and-resume-writers).

Require uncommitted changes with passing focused proof, a stop requiring human
judgment, or an oversized-slice report under execution decisions. An implementation
return does not establish slice completion.

For each passing focused command, use:

```text
proof:
  command: <literal complete focused command>
  covers: <observable behavior or paths covered>
  result: pass
```

Connect proof to the slice's promises. Placeholders, abbreviations, and
paraphrases are ambiguous evidence. Report uncovered behavior as incomplete
implementation; the refactor pass must not supply missing behavior.
