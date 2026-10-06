# Process review of a run

Apply this when Dough Land or Story Wrap Up is invoked with
`--process-retrospective`. The flag alone selects the review: do not read
`open-dough.json` or its `skipProcessRetrospective` for it. Without the flag,
run no process review and do not read the process log for one.

At the caller's review point, review this run's own process so far under
[review process only from a real record](../SKILL.md#review-process-only-from-a-real-record),
and record supported findings under
[process-finding recording](process-finding-recording.md) in the run's own
checkout, which the caller supplies as the write location, as
[write only in an owned checkout](../SKILL.md#write-only-in-an-owned-checkout)
describes. Its execution identity follows that recording rule; when the run's
context names no work identity, the run's accepted revision on its target is
the stable execution-record reference.

The caller commits and publishes the written findings on its own path. The
review, its recording, and that publication never undo or block the run:
report unavailable history, an unresolved log location, a refused write, or a
stopped findings publication as attention with its next action under
[completion attention](../../dough-land/references/completion-attention.md).
With no supported findings, change no log and add no commit, publication, or
wording. Otherwise the final response names the recorded finding IDs.
