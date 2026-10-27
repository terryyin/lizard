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

## DD-002 — Hosted-proof slices have no guarded increment that stays unfinished

A slice whose own proof needs a hosted CI receipt of its publication must be
published before it can be accepted. Guarded fresh delivery always marks the
selected slice `done` and correctly refuses while that slice's receiving
obligation is open, so the coordinator fell back to a manual agent commit plus
existing-candidate `deliver`, then a later plan-only fresh increment to mark it
done. Consider a documented fresh-increment path that publishes without the
status transition.

### Occurrences
- Execution: SEED-012#modernize-website-react
  - Timestamp: unknown
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: `execution-increment-delivery.mjs fresh` for slice 3 returned `preparation-failed`, operation `story obligations`, G3 `open-obligation`; refusal shortly before recovery commit `f6af787` (committed 2026-10-27T16:17:22+08:00; agent-commit + `deliver --validated-candidate`), `390e69d`, `ce549a1`.
  - Observed effect: one refused attempt, then three increments instead of two; the obligation guard itself prevented marking slice 3 done before its hosted run and skip receipts existed.
  - Inference: the workaround is sound but not described in the fresh-increment or delivery references, so another coordinator could instead mark done early or stall.

## DD-003 — Nix shellHook reinstalls Python dependencies on every command

Every `nix develop -c` runs `pip install -r dev_requirements.txt` in the
shellHook, printing pip output to stdout and upgrading `platformdirs` past
`virtualenv`'s pin each time. Agents had to filter that output out of
`gcloud meta list-files-for-upload` and other parsed command results.

### Occurrences
- Execution: SEED-012#modernize-website-react
  - Timestamp: unknown
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: `shell.nix` shellHook; slice 2 implementation report (upload list contained non-path pip lines); slice 2 refactor report (platformdirs 4.4.0 conflicts with virtualenv 20.13.2).
  - Observed effect: extra latency and stdout noise on each Nix-prefixed command; no proof result changed.
  - Inference: a project-environment issue rather than shared guidance; a stamp-guarded or quiet install would remove it.
