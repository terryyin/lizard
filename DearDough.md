# DearDough Process Findings

## DD-001 — Codex stream can yield before exposing observer identity

The prescribed Codex binding prints the observer directory only in its initial
tool result. If checkout setup or provider discovery outlasts that yield, a later
`CI_OBSERVER` line is parsed without exposing the identity to the coordinator.
Consider emitting identity when it becomes available, within the existing binding.

### Occurrences
- Execution: SEED-006#ci-verified-python-support
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; installed guidance came from commit 01afe9c
  - Evidence: launch d05e8e74-4462-4ca8-b3d9-99a957269f33, 2026-10-27; yielded cells 8 and 4; `.agents/skills/dough-execute-plan/references/ci-notify-codex.md` prints identity before its stream loop.
  - Observed effect: both launches yielded a watching session without a directory, with initial waits of 1 and 10 seconds. Two owner-based recoveries using `classifyOwnedObservation` and `codexStreamOwner` were needed before delivery.
  - Inference: the binding's initial-result assumption is sensitive to setup/discovery latency; coverage itself remained available.
