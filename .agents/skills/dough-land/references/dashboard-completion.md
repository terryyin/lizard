# Report a dashboard-started closure

Use only the reporting command and launch context explicitly supplied to this
invocation. Without them, follow the ordinary native response and make no
dashboard contact. Do not discover a dashboard, infer a session from the workspace,
or reconstruct missing launch context. If an invocation says dashboard reporting
is required but its command/context is missing, report that gap locally; never
claim acknowledged completion.

Before retirement removes a checkout, retain the supplied absolute reporting
command and a surviving working directory. The dashboard-prepared command lives
outside the workspace. Keep any attention-message file outside that workspace too.
Do not run a script from the deleted checkout or use its removed working directory.

After publication, observation/shutdown, refresh, and retirement have settled under
their existing gates, settle the exact final response using
[completion attention](completion-attention.md). Then make this reporting command
the final operation:

- Completed with nothing requiring attention: append `--outcome completed`, with
  no message file, to the supplied reporting command.
- Completed with attention: write the exact useful native response to a surviving
  file, then append `--outcome completed --message-file <absolute file>`.
- Failed, held, or unfinished work: write the exact useful reason and next action
  to a surviving file, then append `--outcome unfinished --message-file <absolute file>`.
  Missing required context, unaccepted or superseded candidates, unpublished required
  closure, unconfirmed shutdown, and held retirement cannot report quiet completion.

Wait for a matching receipt. The operation stores completion evidence and local
Done for completed/no-attention before acknowledging a recorded session. A
`pending-native-session` receipt confirms this launch only, names no native session,
and binds its completion/Done to that launch when its identity is confirmed.
Neither receipt declares a product story complete. Never infer a receipt or success
from silence, a marker, a native last reply, or process exit.

After the receipt, give the ordinary minimal native response; with attention, give
exactly the response submitted. Do not perform more substantive work after reporting.
If delivery fails, report unacknowledged delivery locally and retain the message;
accepted Git/retirement work stays accepted and is not repeated to retry reporting.
The operation retains the exact submission beside the surviving prepared executable
before contacting the receiver. Run its printed `Retry reporting only` command (or
append `--retry <retained completion file>` to the supplied command) after recovery.
Retry reads the original context, outcome and message; it needs no surviving message
file or checkout. Reuse that delivery even when an acknowledgment was lost. A later
substantive report uses the ordinary outcome/message arguments and its own delivery.
Older retries return their original receipt without replacing newer messages or
repeating local Done; deliberate reopen and manual Done remain the developer's intent.
No background retry is scheduled.

Reporting records local disposition only: it must not rename, detach, stop, or shut
down its sender or a newer turn. Existing terminal-attachment lifecycle owns its
attachments; reporting schedules no delayed disposal. Native Working may remain
visible, and no cosmetic rename or native shutdown is claimed by a receipt. Existing
CI-observer shutdown/retirement gates remain independent and required.

## Retain the one-shot landing

For a dashboard-established one-shot refinement or execution, retain the original
launch's supplied `landing-context.json` outside the checkout.
Without that capability, explain that no landing comparison can be retained;
completion reporting stays available. Never guess a launch, base, target or repository.

Pass `--landing-context <supplied absolute file>` to the installed
`execution-increment-delivery.mjs deliver` command; its shared
`publishExecutionIncrement` API takes the same path as `landingContext`, including
when Dough Land uses that API without an execution observer. The installed
`publication-resume.mjs` API and `execution-increment-resume.mjs resume` accept it
as well. Supply the retained `--suffix-base` with the candidate when resuming.
The handoff writes the exact launch and candidate/base pair before every push,
updates it after reconciliation, and prepares that pair against this launch's
established workspace. The receiver resolves the repository from the established
launch, validates commit types and ancestry, and pins both ends before metadata.
No caller-supplied repository path is accepted.

After remote acceptance, the same handoff records the accepted pair before refresh,
CI finish duties or retirement. Inspect the returned `landing` result independently
of publication and observation. `landing.state: "unacknowledged"` keeps Git acceptance
accepted and names the exact reporting-only retry command. Retain and run that
command from a surviving working directory; it needs no checkout and never republishes.
A prepared pair alone is not a landing receipt. A matching recorded receipt or
`pending-native-session` receipt confirms only this launch's accepted comparison,
without completing the story, setting Done or stopping a native session.

For a later explicit landing, reuse the original retained context, not a new
session's completion channel. A different accepted comparison cannot overwrite
this launch's fixed landing fact. A deleted/expired launch refuses capture and
retry without recreating its record. Its bound launch keeps the original capture
authority and prepared pair for that record's existing lifetime, even after its
startup attempt expires; this extends no retention period. Receipt recovery keeps
the original delivery and comparison, including after native binding or a newer
completion report. If deletion reports failed pin cleanup, retry that deletion;
capture stays refused while cleanup is pending. The final completion report above remains
the final operation after all required duties and wording settle.
