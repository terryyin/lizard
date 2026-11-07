# Retire owned watches when verification ends

A watch the owning agent created to learn a delegated command's result lives
only as long as the verification obligation. Prefer the host's existing
completion handling — the supported continuation under
[verification ownership](delegation.md#own-verification-to-its-terminal-result) —
over creating an extra watch for the same command.

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
