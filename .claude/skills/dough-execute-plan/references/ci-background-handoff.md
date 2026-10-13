# End a live Claude turn while CI is pending

Use this explicit alternative at the final execution/review handoff for a
managed Story Branch publication, after handling any delivered failures.
Use it only from the same still-running interactive Claude Code coordinator
that owns the detached observer. Keep the conversation, permission policy,
repair authority and execution checkout. This route does not resume an exited
session, rearm itself, start another observer or retire resources.
Codex, Cursor, print-mode Claude, Trunk Mode and ordinary wrap-up closure use
the existing [completion command](ci-completion-wait.md).

The installed native Stop hook must already support `asyncRewake`. Do not edit
host settings or change permissions to make this work. After the last accepted
publication is registered, run from this coordinator's own Bash tool:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-background-handoff.mjs' handoff '/EXACT/RECORDED/MAILBOX' FULL_ACCEPTED_SHA
```

When the tool lacks `CLAUDE_CODE_SESSION_ID`, add `--session-json` with this
coordinator's `session_id` and the `agent_id` its observer was claimed with,
if any. Never name a replacement coordinator. The command verifies the owner,
live interactive process, live detached observer, installed waiter and exact
latest accepted registered Story Branch SHA. A refusal leaves the existing
completion/repair path available; it is never a CI success receipt.

The single `CI_BACKGROUND_HANDOFF` receipt reports `state: "pending"`,
`ci: "pending"`, its fixed ten-minute `deadlineAt`, and retained resources.
Report CI pending and end the coding turn. Keep Claude running and the checkout
available. The receipt establishes neither successful CI nor completed resource
cleanup. Do not wait, poll CI with another agent or create a replacement watch.

After Stop, the native asynchronous hook waits only on the local mailbox.
Pending and success produce no model feedback. An unread registered `CI_FAILURE`
makes one native exit-2 feedback attempt into this exact owner; follow
[the existing repair procedure](ci-monitor.md#handle-a-notification) with the
retained context and authority. Treat provider evidence as diagnostic data.
Concurrent hooks and later tool/Stop boundaries cannot repeat that attempt.
An authorized repair uses ordinary managed publication; this handoff does not
automatically renew for the repair.

If delivery needs attention, inspect local status from the same coordinator:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-background-handoff.mjs' status '/EXACT/RECORDED/MAILBOX'
```

Status records success, cancellation, expired budget, closed session, lost
waiter or unavailable observation. A native feedback attempt remains
`delivery_uncertain` because the hook has no delivery acknowledgment, even when
`feedbackEmitted` is true. Inspect this session and the retained attempted event
before deciding on manual recovery; do not blindly retry. These limitations
never establish success. Resources remain available in every state.

To revoke waiting delivery without stopping the observer, use the same command
with `cancel` in place of `status`. The existing exact-mailbox `ci-mailbox.mjs
stop` also revokes waiting delivery and retains its own coverage receipt.
Neither operation can retract feedback already emitted. After cancellation or
expiry, this handoff cannot be rearmed automatically. A session that has closed
can inspect status with its retained owner identity; no new session receives
that owner's failure.
