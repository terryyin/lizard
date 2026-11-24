# Delegate a slice

Assign each planned slice, or the one quick slice, to a fresh implementation
agent. Use a general-purpose agent, or `gsd-executor` when this project uses
`/gsd-execute-phase`. Implement locally only for a single interactive slice.
The coordinator retains
[wrap-up](wrap-up.md); an execution tool does not take over that responsibility.

## List promises and assign the agent

Before planned or planless test-authoring delegation, follow
[shared test-entry selection](test-entry-rules.md). Carry its paths and selected
conventions, including checks the author must perform for newly identified tests.

Before planned delegation, run the installed
[story-obligations script](../scripts/story-obligations.mjs):
`node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N`.
Resolve these inputs under [story obligations](story-obligations.md#use-the-record-at-execution-boundaries).
A refusal blocks delegation until resolved. Include the listed returns, receiving
obligations and open interims in the brief with their required observations.

Give the agent:

- The selected execution checkout and branch. Pass the complete retained
  execution identity. Require all implementation commands and edits
  to run there rather than relying on the agent's inherited working directory.
- The execution source, current slice and expected observations. For planned
  execution, pass the plan path and its full selected story or bounded correction.
  Also pass relevant existing-solution findings and candidate evidence from the plan,
  plus new evidence that triggered a PFE revisit; a fresh agent does not repeat
  a still-valid search merely because delegation occurred. For a correction,
  pass its complete
  [correction input](../../dough-story-refinement/references/planning.md#choose-the-planning-level):
  its minimal story and plan, or a plan-homed correction's plan alone. For
  quick execution, pass the established
  source — the canonical story and skip-planning instruction, or the contextual
  instruction — plus relevant conversation context; require no plan, fabricated
  story, or substitute execution record. Omit unrelated plan or conversation
  history.
- Any North Star topic cited by the delegated work and the evidence supporting
  it. Require the agent to return contrary evidence through [execution
  decisions](execution-decisions.md#resolve-conflicting-recorded-direction),
  without changing the topic or continuing the affected path.
- For planned execution, the slice's
  [premise rows](../../dough-slice-planning/references/settle-decisive-premises.md)
  as the plan records them, including each unverified remainder. Require the
  agent to reuse a settled result without repeating its observation, check a
  remainder in the form the slice prescribes before work that depends on it,
  and return a false premise through [execution
  reassessment](execution-decisions.md#reassess-before-extending-work) rather
  than continuing on it.
- When the delegated work expects to edit a file already over this project's
  [file-size check](../../dough-post-change-refactor/references/refactor-checks.md#file-size),
  including a comment-only change, carry the settled reorganization decision:
  file and current size, cohesive responsibilities and seams, owning slice or
  work, affected callers, preserved proof, and any precisely scoped developer
  decision. Pass that same substance for an authorized quick brief without
  fabricating a plan. Require the agent to deliver that reorganization before
  or with the edit, and to return a newly discovered oversized edit through
  [execution reassessment](execution-decisions.md#reassess-before-extending-work)
  before dependent work rather than editing first and asking for an exception.
- For planned execution continuing an oversized quick attempt, the remaining-work
  plan plus the preserved completed work and proof and any incomplete-change
  disposition needed to identify the true starting boundary. Require the agent
  not to repeat completed compatible work or its unchanged proof. Treat the
  quick attempt and planned continuation as one execution, not two handoffs with
  independent histories.
- [Execution decisions](execution-decisions.md), the resolved
  [replanning permission](execution-decisions.md#choose-replanning-permission),
  the remaining time and literal `run` form of the
  [slice clock](oversized-slice.md#track-active-time), any proof deadline and
  [how to wait for proof](wait-for-proof.md), workflow precedence, the
  [proof evidence contract](proof-evidence.md), and literal focused commands
  with the runtime wrapper, each run through that `run` form. Carry the
  [failure policy](failure-policy.md) and this project's additions by link or verbatim, not as a weaker paraphrase such as "fix or
  report"; pass a known failure's attribution as diagnosis evidence, separate
  from the repair responsibility that policy assigns. For a diagnosis in
  [rounds](diagnosis-rounds.md), CI repair included, pass its record's path and
  any existing record of the same failure. Proof the plan or
  coordinator names is a minimum: require the agent to trace the changed
  effect into its consumers with the installed
  [consumer-trace script](../scripts/consumer-trace.mjs),
  `node '<installed-execute-plan>/scripts/consumer-trace.mjs' --checkout '<checkout>' --base <revision> [--term …]`,
  and select sufficient proof under
  [own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof),
  including every suite it selects, even when the plan says wider suites
  belong to CI.
  At a reported overrun, return unfinished work and the clock's elapsed time. With
  replanning disabled, leave the incomplete attempt in place for the coordinator's
  overrun handling.
- For expensive verification, the retained decision under
  [choose expensive proof before launch](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch):
  actual target host, workload, evidence and unknowns, bounded choice, command/proof
  owner, and reconsideration condition. Include known competing owned proof,
  including separate worktrees on that machine. Require the receiving owner to
  confirm the basis still applies or reconsider it before launching or repeating
  expensive proof; use existing handoffs to coordinate owned heavy work.
- When the slice includes an authorized live action, the plan's named
  regression prerequisite and whether an accepted current observation of it
  already exists. Require the agent apply [require current regression proof
  before a live action](live-action-regression-proof.md)
  at the point it performs that action, not only at coordinator acceptance:
  obtain the missing proof first, or leave the action unperformed and return
  the exact obligation and gap. A passing operational or health check does not
  relieve this requirement.
- Ownership of the slice's changes. State that the Git stash stack is shared
  across all worktrees and that unowned work, from humans or other sessions,
  may be present in the checkout and must be preserved. The agent does not
  stash, pop, reset, clean, check out paths, or switch branches in the shared
  checkout; when it needs a pre-change baseline, it uses a separate temporary
  checkout or reports the need back. The coordinator's
  [CI repair pause](ci-monitor.md#handle-a-notification) is the only
  sanctioned stash.
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

Follow
[Retire owned watches when verification ends](retire-owned-watches.md#retire-owned-watches-when-verification-ends).

## Return a targeted report with focused proof

Follow [the return contract](return-report.md): write the full report in the
evidence location and return a short verdict pointing to it. The contract owns
the required proof detail, promise coverage and completion markers.

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

When the expected proof window is exceeded, or remaining applicable observation
coverage calls for a check before that coverage lapses, follow
[Check an overdue delegated verification](overdue-delegated-verification.md#check-an-overdue-delegated-verification)
rather than continuing an indefinite wait for completion notifications alone.

If the host offers no supported way to await the pending result, report the
exact limitation instead of inventing an API or assuming completion: name the
missing facility, the known state of the delegated work, and what recovery
needs. Do not copy another host's yield semantics, and do not treat ending
the turn or blocking indefinitely as a universal remedy.

CI observation keeps its own lifecycle under
[CI monitoring](ci-monitor.md); this decision does not change it.

## Recover an interrupted delegated result

When a delegated implementation or refactor call is interrupted, follow
[Recover an interrupted delegated result](interrupted-delegation.md#recover-an-interrupted-delegated-result)
before accepting a claim or starting a replacement writer.
