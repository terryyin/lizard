# Managed completion recovery

When this coordinator's observer for that target has ended before the accepted
SHA has a completion receipt — including a budget-expiry notice that names
`execution-completion-recovery.mjs complete` — run that one managed recovery
instead of polling by hand or inventing a second receipt. It takes the same
owner input as `resume` (`--workspace`, `--accepted-sha`, `--target-ref`,
`--repo`, `--host`, and the host session or Codex stream flags), re-establishes
this coordinator's observation of that SHA on its authorized target, and ends
in the ordinary `CI_OBSERVER` completion receipt under
[completion command and receipt mechanics](ci-completion-wait.md#completion-command-and-receipt-mechanics).
On Codex, arm a fresh yielded stream for the same target and coordinator first
([Codex adapter](ci-notify-codex.md)); never restart a finished stream. Then
pass `--coordinator` and `--observer-directory` to the same command.
