# Cursor and Claude Code notification adapters

Use this adapter on Cursor or Claude Code after [runtime setup](runtime-setup.md).
Both use the same detached Node observer and private mailbox, with host-specific
hook JSON already registered by install or update. Apply
[CI observation and repair](ci-monitor.md) for shared lifecycle and failure
decisions. This adapter verifies readiness and manages the observer; it does
not write host settings.

## Verify the host bridge once per execution

The hooks inspect local mailbox files only, without network calls, AI calls,
or waiting for CI. Stop hooks deliver only ready events; pending or successful
CI does not extend the agent turn.

Run this harmless probe from the checkout root using the normal Shell/Bash
tool, not an implementation agent or a tool that hides its stdout:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' probe
```

The command prints a `CI_OBSERVER` receipt, **not** `CI_MONITOR_READY`. Proceed
with observation only if the host hook adds separate `CI_MONITOR_READY`
context. A receipt alone proves neither hook registration nor notification
delivery. Rerun the probe after a host/session change.

If readiness is missing, report an unavailable bridge once and continue the plan
without promising monitoring and without rewriting host settings. Missing
readiness is explicitly unavailable coverage, not a reason to merge fragments or
edit `.cursor/hooks.json` / `.claude/settings.json` from execute-plan. You may
note hook errors, Node PATH, workspace trust, or disabled hooks as diagnostics;
do not bypass user approval or trust checks. A fresh approved session can retry
the probe when registration is already present. Do not replace observation with
AI polling.

## Start once and continue immediately

After readiness succeeds, launch with the verified runtime setup values:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' start --execution OWNER/REPO BRANCH
```

This starts a detached non-AI process and returns immediately. Retain the one
directory from its `CI_OBSERVER` receipt as the coordinator's execution
handle. The hook must add `CI observer attached to this coordinator`; absence
of that labelled context means observation is not connected. Re-entering setup,
including after a normal or repair push, reuses that directory and must not run
the launcher again. The observer discovers each later selected branch push itself, so a
push changes neither its owner binding nor its process handle. Continue
delegation and plan execution immediately.

The next coordinator hook invocation after a result is ready adds the event to the owning
coordinator's context exactly once. Pending polls and successful CI add no
context. The native hook selects durable records without advancing delivery
progress, writes the unchanged host JSON, and acknowledges those records only
after stdout reports a successful write. If the hook process is interrupted
before that boundary, the next owning invocation can select the records again.
The mailbox is claimed by checkout, host, conversation, and worker identity;
Cursor additionally binds to the coordinator's `generation_id` because its
children can share the conversation ID, and `beforeSubmitPrompt` updates that
binding on a new user message; arbitrary child tool calls cannot rebind it, and
missing generation identity fails the readiness probe. Claude Code isolates
instead by `session_id` plus `agent_id`/`subagent_id`, so a sub-agent sharing
the coordinator's session cannot consume its notification. Keep the same
coordinator session when resuming; if replacing it, stop the old observers
using their recorded directories and start new observers in the new session.

Use the same shared mailbox directory for launcher and hooks as specified in
runtime setup. Mailboxes survive stashing. Use the host's agent message and
resume handles to follow the shared [pause contract](ci-monitor.md#pause-and-resume-writers).

## Stop without waiting for CI

When the shared lifecycle calls for shutdown, stop using the exact saved
directory:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' stop '/EXACT/RECORDED/MAILBOX'
```

This signals cancellation, including an outstanding GitHub request or polling
timer. Its terminal wait is finite and checks authoritative `result.json` when
the file notification is missed. If no terminal result arrives by that
lifecycle deadline, the stop command reads the mailbox's recorded worker PID,
validates that it still runs the exact Node worker command for this mailbox,
and targets only that process; it revalidates before escalating the signal.
The command then returns an explicit lost-coverage terminal result instead of
hanging or implying success. This is local process shutdown, not waiting for
CI. The hook drains an already-finished event even if stop was requested. Unread
records remain in the mailbox, and shutdown reports pending CI as unobserved.
Handle delivered failures before claiming completion. Retain these small
recovery records for interrupted sessions; never kill by a broad process-name
pattern. The runtime budget also bounds an observer whose coordinator disappears.
Stopping the observer ends only the detached process and mailbox coverage.
Retain the installed hook registration; do not unregister or rewrite host
settings on shutdown.

## Host contracts

- [Cursor hooks](https://cursor.com/docs/hooks): `postToolUse` receives
  `conversation_id` and JSON-stringified `tool_output`; the hook returns
  `additional_context`. The `stop` adapter returns `followup_message` only
  for an available event. It uses project-relative commands and does not
  depend on Claude compatibility mode.
- [Claude Code hooks](https://code.claude.com/docs/en/hooks): `PostToolUse`
  receives `session_id` and `tool_response`; the hook returns
  `hookSpecificOutput.additionalContext`. `Stop` uses `decision: block` with
  the event as its reason only once. Commands resolve via
  `CLAUDE_PROJECT_DIR`; local permissions/preferences remain in ignored
  `settings.local.json`. If Cursor also loads this configuration through
  third-party compatibility, the Claude adapter ignores its `cursor_version`
  payload so only the native Cursor adapter can claim notifications.

Neither bridge wakes an idle session for periodic polling. Delivery happens
at a tool/stop boundary of the ongoing coordinator. Native hook support must
pass the readiness probe on the installed host; do not infer it from the
product name or from merely running the script in a terminal.
