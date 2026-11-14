# Await the applicable revision at completion

This reference owns when the single completion operation is required — including
execution/review handoff and wrap-up closure or trunk integration — how its
outcome affects those procedures, and the local command and receipt mechanics.
[CI observation and repair](ci-monitor.md) owns the observer, notification
handling, and the writer pause.

The coordinator owns one bounded completion operation whenever applicable CI
evidence is required: execution/review handoff, wrap-up closure on the authorized
target, and Story Branch trunk integration.
Use that target's observer and last accepted registered SHA; exclude newer tips,
other writers' revisions, unpublished candidates and other targets' green. Coverage follows ignored-only revisions to their basis.
Local-only work and intermediate closure publications create no wait; report unavailable bridges.

When automatic retrospective is enabled, begin it as soon as implementation is
delivered, even with applicable CI pending. Supply observer, target, accepted SHA
and pending state. After review — or with `--skip-retro` or another explicit
review omission — publish the plan's [execution-complete record](finish-or-stop.md#record-execution-completion),
then invoke exactly one completion action. Wrap-up uses it after registering the
final accepted SHA: the closure or integrated SHA, or the last consumer
publication. Follow [completion mechanics](#completion-command-and-receipt-mechanics),
including its live-Claude Story Branch background handoff; handle delivered failures at safe boundaries.

Success with confirmed shutdown ends observation; report `integrated_state` under
[completion mechanics](#completion-command-and-receipt-mechanics).
Another writer's failure creates no repair obligation. Success completes only once
every actionable failure delivered for this execution is repaired, proved infrastructure,
held by a reported active owner or explicitly excepted; no passing rerun or later green tip settles one.
Failure retains the observer and returns to [Handle a notification](ci-monitor.md#handle-a-notification);
never retries until green. Missing repair authority, ownership or a required
decision leaves incomplete work with failed revision and partial review preserved,
with no completion marker. An unresolved receipt establishes no success: report
its reason, effective evidence and shutdown. Pending or unconfirmed shutdown
retains resources; follow [completion mechanics](#completion-command-and-receipt-mechanics)
for recovery or expiry. Observation cancellation is not execution or wrap-up cancellation.
Authorized repair uses publish/register and invalidates only affected review conclusions;
complete its new accepted revision at a later boundary. The retrospective gains
no repair, commit, push, backlog or observer authority.

## Completion command and receipt mechanics

At a final managed Story Branch execution/review handoff, the same live
interactive Claude coordinator may choose the bounded
[background handoff](ci-background-handoff.md). All other completion paths
below retain this operation.

Invoke the completion action with the exact retained mailbox and accepted
revision:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' complete-revision '/EXACT/RECORDED/MAILBOX' FULL_ACCEPTED_SHA
```

When the observer ended before a receipt, follow
[managed completion recovery](ci-completion-recovery.md).

The command stays silent while waiting and keeps its ten-minute foreground bound.
Before waiting it exclusively records the first completion start and a sixty-minute
overall deadline for this mailbox and accepted SHA. Resuming, caller interruption,
concurrent calls and coalesced queue progress never renew that deadline. A
discovery-delay advisory neither renews nor ends either bound. It
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
- `effectiveEvidence.source: "integrated_state"` is that success under this
  project's declared [integrated-state policy](runtime-setup.md#select-ci): the
  accepted revision had no successful run of its own (`accepted` keeps its own
  state; an ignored-only `not_required` revision also keeps its basis, whose
  applicable attempt was replaced or is still pending), and a successful
  current attempt at a later same-target snapshot
  that contains it (`validated`: SHA, run, attempt, URL, check, context and
  any required jobs; `ancestry`) validated the integrated state. Retain both
  revisions as evidence. When details are requested or needed for an action, say
  "<accepted SHA> published; integrated state validated at <validated SHA>",
  never that the accepted revision passed. This evidence requires no routine
  success recap. Keep the accepted SHA as the publication receipt for any
  closure, rebase or merge.
- `verdict: "failure"` with `shutdown.status: "retained"` ends the wait without
  waiving the failure and keeps the observer available for diagnosis and
  authorized repair. Return to failure handling; do not claim completion or run
  a separate stop.
- `effectiveEvidence.integrated` on an unresolved receipt names why no later
  snapshot qualified yet (`reason`, and each considered SHA's reason). The
  observer judges candidates afresh on every poll from the provider's current
  attempt and the local checkout's Git ancestry. When this checkout lacks a
  snapshot whose listed attempts all succeeded, it fetches only that
  snapshot's objects, from the target's remote and without changing any ref;
  `ancestry_unknown` remains only when that remote cannot serve it or no remote
  for the target is identified. A snapshot with a pending or failed listed
  attempt is never fetched and is reported as `not_current_success`. A
  pending, failed, cancelled, rerun, unreadable, other-target or
  context-incompatible attempt supplies no success.
- `unresolvedReason: "pending"` has no verdict. Fresh legitimate queued, running,
  retrying or applicable pending-attempt evidence outlasted the foreground wait;
  `pending` explains that evidence and `shutdown.status: "retained"` keeps the
  observer. Retain `handoff.record`, its durable `pendingReceipt`, original
  `startedAt` and `deadline`, target/SHA and exact `handoff.command`. Repeat that
  command to resume the same owned observation, without a new owner, observer,
  provider request or rearm. Caller SIGINT/SIGTERM returns `wait_cancelled` with
  the same handoff; after SIGKILL its first record is already on disk. Pending
  supplies no cleanup, wrap-up continuation or retirement authority.
- `unresolvedReason: "completion_expired"` with detail
  `overall_completion_deadline` closes the original handoff durably. Report the
  unresolved cap and shutdown outcome; later green cannot reopen it. An exact
  failure still remains a failure at expiry, and unread actionable failures keep
  observation. `observation` preserves an unavailable, cancelled, unreadable or
  missing-registration cause and its detail alongside the closed cap; worker
  loss remains explicitly `observation_unavailable`, never replacement coverage. A successful receipt already recorded within budget can be
  repeated while the same applicable successful evidence still holds.
- Other `unresolvedReason` values are `timeout`, `observation_unavailable`,
  `wait_cancelled`, `incomplete`, `evidence_unreadable`, or
  `missing_registration`. Stale, withdrawn, unsupported, foreign or unreadable
  evidence cannot justify pending.
  Except `wait_cancelled` and `pending`, a bounded unresolved result
  ends the wait and includes shutdown evidence; it never establishes success.
- `shutdown.status: "retained"` with `reason: "unread_actionable_failure"` keeps
  observation when unread `CI_FAILURE` evidence remains; that evidence is not
  acknowledged or erased by a later green revision.
- `shutdown.status: "unconfirmed"` returns the limitation and retains resources;
  do not treat unavailable shutdown as successful CI or as cleanup authority.

Progress: [`await-revision`](fresh-increment.md#observe-progress-proof)
(read-only). Keep `complete-revision` at completion. Explicit cancellation and
human-judgment stops still use the host adapter's stop command.

Return that complete receipt to the owning procedure. Do not interpret a
successful local command exit as a successful CI verdict.
