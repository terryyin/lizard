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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/008-repeatable-shell-entry/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"1702fbf4abeea846152165830e7c609ab1624e21cfbd0f4a7952a6aa6e14cc38","plan":"bc98fb31d4ab94022321ecc31f5e4560d77e56fdf6c26f1504bfaa54b28593c1"}}
```

**Beneficiaries:** Contributors and agents who prefix local tooling with
`nix develop -c` (and interactive `nix develop`) in this repository.

**Goal:** Reuse a correctly prepared development environment on unchanged
shell entry—including overlapping warm, read-only invocations—so repeated
commands avoid reinstall churn and pip installation chatter on stdout, while
still refreshing preparation when its real dependency inputs change, keeping
supported development tools usable, and making preparation failures visible.
After delivery, record the adopted response revision and relevant-use outcome
at DD-003; queueing alone does not resolve the finding.

**Scope:** Project-owned Nix development-shell preparation for this repository
(`shell.nix` / flake `devShell` and the local environment it prepares). Required
behavior: cold first entry prepares a usable environment; warm re-entry with
unchanged dependency inputs reuses that environment without reinstalling or
emitting pip install chatter into command stdout; overlapping warm read-only
entries in the same checkout succeed without requiring serialized recovery;
when real dependency inputs that drive preparation change, the next entry
refreshes so the environment matches those inputs; preparation failures remain
visible on the failing entry; existing supported development tools and imports
used for ordinary local work (Python/test tooling and the shell's declared
packages) stay usable after cold, warm, overlap, and refresh cases.

Justified rejection constraints: none beyond failing visibly when preparation
cannot succeed.

Deferred promises: parser or analyzer changes; performance-harness redesign;
shared Open Dough guidance or host-policy changes; inventing a latency or
timing claim beyond before/after evidence of the reported
platformdirs/virtualenv mismatch and the observable reuse/refresh behavior;
changing how non-Nix workflows install dependencies.

**Key examples:**

- Cold setup: with no prepared local environment present, `nix develop -c`
  runs a supported Python or pytest import/check successfully and leaves a
  prepared environment for later entries.
- Warm reuse: after a successful cold setup and with dependency inputs
  unchanged, a later `nix develop -c <command>` completes with that command's
  stdout free of pip installation chatter, and the same Python/test imports
  remain usable without a reinstall cycle.
- Overlapping warm entry: with a prepared environment and unchanged inputs,
  two concurrent read-only `nix develop -c` invocations in the same checkout
  both succeed and leave a usable environment (no ensurepip/race recovery
  required).
- Dependency-input change: after warm reuse works, changing a real
  dependency input that drives preparation and re-entering refreshes so the
  environment reflects that change; subsequent warm entry again reuses without
  pip install chatter.
- Visible failure: when preparation cannot succeed, the failing entry surfaces
  the failure rather than silently leaving a broken shell for later commands.

**Evaluation:** Observe cold setup, warm repeated entry, overlapping warm
entry, and a dependency-input change. Demonstrate usable Python/test imports
and command stdout in each relevant case; retain before/after evidence of the
reported platformdirs/virtualenv mismatch without inventing a latency claim.

**Completion:** Record the adopted response revision and relevant-use outcome
at DD-003 in DearDough.md. Queueing does not resolve the finding. Parser
changes, performance-harness redesign and shared guidance are outside this
story.

**Effort hypothesis:** S — confidence medium; localized to project shell
preparation with observable entry cases already named by DD-003.
**Depends on:** none.
**Safe stop:** Warm reuse and clean stdout alone already remove the dominant
DD-003 noise; overlap safety and refresh remain independently valuable if
later polish waits.
**Mapped plan:** [008 repeatable shell entry](../slice-plans/008-repeatable-shell-entry/PLAN.md).
