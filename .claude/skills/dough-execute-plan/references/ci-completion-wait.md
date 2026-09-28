# Completion command and receipt mechanics

[CI observation and repair](ci-monitor.md#await-the-applicable-revision-at-completion)
owns when this single completion operation is required — including
execution/review handoff and wrap-up closure or trunk integration — and how its
outcome affects those procedures. This reference supplies only the local
command and receipt mechanics.

Invoke the completion action with the exact retained mailbox and accepted
revision:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' complete-revision '/EXACT/RECORDED/MAILBOX' FULL_ACCEPTED_SHA
```

The command stays silent while pending and uses its fixed ten-minute bound from
invocation. A discovery-delay advisory neither renews nor ends that bound. It
reuses the local read-only applicable wait and then performs local observer
shutdown when that wait's outcome permits ending observation. It does not
contact the provider, acknowledge notifications, or cancel execution. Never
invoke it after ordinary slice delivery, assign an agent to poll it, or
substitute the observer's eight-hour lifetime for this bound. Do not run a
separate stop, process poll, or terminal-report read after it on the normal
completion path.

Inspect the single `CI_OBSERVER` receipt. Retain its requested revision, target,
effective evidence, CI outcome, and `shutdown` evidence together; CI facts stay
distinct from shutdown facts:

- `verdict: "success"` with `shutdown.status: "confirmed"` satisfies completion
  observation and ends local observation.
- `verdict: "failure"` with `shutdown.status: "retained"` ends the wait without
  waiving the failure and keeps the observer available for diagnosis and
  authorized repair. Return to failure handling; do not claim completion or run
  a separate stop.
- `unresolvedReason` is one of `timeout`, `observation_unavailable`,
  `wait_cancelled`, `incomplete`, `evidence_unreadable`, or
  `missing_registration`. Except `wait_cancelled`, a bounded unresolved result
  ends the wait and includes shutdown evidence; it never establishes success.
- `shutdown.status: "retained"` with `reason: "unread_actionable_failure"` keeps
  observation when unread `CI_FAILURE` evidence remains; that evidence is not
  acknowledged or erased by a later green revision.
- `shutdown.status: "unconfirmed"` returns the limitation and retains resources;
  do not treat unavailable shutdown as successful CI or as cleanup authority.

The low-level `await-revision` reader remains available for its existing
read-only purpose and does not shut down observation. Ordinary completion uses
only `complete-revision`. Explicit cancellation and human-judgment stops still
use the host adapter's stop command.

Return that complete receipt to the owning procedure. Do not interpret a
successful local command exit as a successful CI verdict.
