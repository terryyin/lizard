## Other completion and stop paths

Wholly planless completion
retains source, conversation, identity, delivered changes, and proof, invokes
the completion operation when
publication created that CI obligation,
reports delivered work, the CI verdict or exact unresolved reason, and shutdown,
and ends with `## QUICK EXECUTION COMPLETE` after required delivery/completion,
without automatic retrospective or automatic planned continuation. Its coordinator
invokes wrap-up separately after successful delivery. Do not
report integrated completion here; wrap-up owns Story Branch merge and required
target push, Trunk Mode closure publication, and resource cleanup.

For incomplete work, failed delivery/shutdown, cancellation, or a human-judgment stop, report
source, active plan/next slice or quick-slice state, preserved work, observer state,
and required decision/recovery action. An applicable CI failure that existing
handling cannot resolve is such an incomplete stop, even when review finished
and the observer remains available for diagnosis. Emit no completion marker or
automatic retrospective. Before giving that stop response, apply
[report attention at the execution-complete handoff](finish-or-stop.md#report-attention-at-the-execution-complete-handoff)
as an unfinished report. For planned work, use the shared completion-attention
judgment: begin with `DEVELOPER ATTENTION REQUIRED` and name the actual gap and
required recovery; quiet output never substitutes for an unfinished gate.
