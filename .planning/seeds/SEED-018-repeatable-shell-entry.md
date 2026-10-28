---
id: SEED-018
status: active
planted: 2026-10-28
planted_during: Combined project and shared feedback maintenance
trigger_when: Repeated or overlapping Nix shell entry rebuilds the environment
scope: small
---

# SEED-018: Repeatable development shell entry

## Why This Matters

Contributors repeatedly enter Nix to inspect and prove changes. [DD-003](../../DearDough.md#dd-003)
records repeated pip output and dependency churn across six executions, with
two overlapping-entry races requiring sequential recovery. The shellHook is
project-owned; this is not a change to shared Open Dough guidance.

## Selected Stories

<a id="repeatable-shell-entry"></a>

### 1. Enter the development shell repeatedly without rebuilding its environment

**Identity:** SEED-018#repeatable-shell-entry

For contributors, reuse a correctly prepared environment on unchanged shell
entry, including overlapping read-only invocations, while refreshing it when
its real dependency inputs change. Parsed command stdout must contain the
command's output without pip installation chatter. Preserve the supported
development tools and make preparation failures visible.

**Evaluation:** Observe cold setup, warm repeated entry, overlapping warm entry
and a dependency-input change. Demonstrate usable Python/test imports and
command stdout in each relevant case; retain before/after evidence of the
reported platformdirs/virtualenv mismatch without inventing a latency claim.

**Completion:** Record the adopted response revision and relevant-use outcome
at DD-003 in DearDough.md. Queueing does not resolve the finding. Parser changes,
performance-harness redesign and shared guidance are outside this story.
