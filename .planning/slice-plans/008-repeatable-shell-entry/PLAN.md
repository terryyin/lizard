# Repeatable development shell entry

**Identity:** SEED-018#repeatable-shell-entry
**Source:** [canonical story](../../seeds/SEED-018-repeatable-shell-entry.md#repeatable-shell-entry)
**Source review:** [source-review.md](./source-review.md)

## Outcome and boundaries

Contributors and agents who use `nix develop` / `nix develop -c` in this
repository reuse a correctly prepared environment on unchanged entry—including
overlapping warm read-only invocations—without pip installation chatter on
command stdout, refresh preparation when real dependency inputs change, keep
supported development tools usable, surface preparation failures, and record
the adopted response at DD-003.

In scope: project-owned Nix shell preparation (`shell.nix` / flake `devShell`
and the local environment it prepares). Out of scope: parser/analyzer changes,
performance-harness redesign, shared Open Dough guidance, inventing latency
claims, and non-Nix install workflows.

This preparation authorizes no Take or execution.

## Existing owners and direction

PFE: change the existing owner — `shell.nix` `shellHook` (imported by
`flake.nix` `devShell`) — rather than add a parallel environment bootstrap.
`dev_requirements.txt` remains the declared Python dependency input. No Accepted
ADR governs shell preparation; no North Star topic is warranted. Preserve
AGENTS.md guidance that agents prefix tooling with `nix develop -c`.

## Observed premises

Observed on 2026-10-28 in isolated checkout
`/tmp/lizard-shell-observe/lizard-observe` cloned from this repository's
`master` tip (product revision matching trunk at observation time). Preparation
workspace for planning:
`/Users/terryyin/git/lizard/.worktrees/enter-the-development-shell-repeatedly-without-r`.

- Cold `nix develop -c python -c 'import pytest; print("IMPORT_OK", …)'`
  creates `.venv`, runs `pip install -r dev_requirements.txt`, prints full pip
  install chatter including
  `virtualenv 20.13.2 requires platformdirs<3,>=2, but you have platformdirs 4.4.0`,
  then prints `IMPORT_OK` (exit 0).
- Warm re-entry with the same command still runs pip against
  `dev_requirements.txt` (~72 lines of Requirement/Collecting/Successfully
  chatter, same platformdirs conflict) before `WARM_IMPORT_OK`.
- Two concurrent warm entries both exit 0 with `A_OK`/`B_OK` and each emit ~68
  pip chatter lines (both mutate); environment remains importable afterward.
  Intermittent ensurepip races remain documented in DD-003; current proof route
  for the story is the same `nix develop -c` observation, not serialization.
- Proof route available: `nix develop -c <python/pytest command>` with stdout
  captured; `.venv/` is gitignored.

No scratch product edits remain in the preparation workspace.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Cold prepares usable env | Slice 1: fresh checkout without `.venv`; import/check succeeds |
| Warm reuse without pip install chatter | Slice 1: second entry stdout is command output only; no Requirement/Collecting/Successfully/platformdirs-install lines |
| Overlapping warm read-only entry | Slice 1: two concurrent warm `nix develop -c` succeed; env remains importable |
| Refresh on dependency-input change | Slice 2: change a real prep input; next entry refreshes; following warm reuses cleanly |
| Visible preparation failure | Slice 2: induce a prep failure; failing entry surfaces it |
| DD-003 relevant-use outcome | Slice 2: DearDough.md DD-003 records adopted revision and relevant-use |

Local gate: shell-entry observations above. Full pytest/`make` only if a slice
touches product Python beyond the shell; ordinary shellHook-only delivery does
not invent a product-suite gate. Follow [AGENTS.md](../../../AGENTS.md) for any
Nix-prefixed tooling.

## Ordered slices

### 1. Reuse prepared environment on unchanged shell entry
Type: Behavior
Status: done
Proof: Accepted — `shell.nix` prepares when `.venv/bin/python` missing, else
activates without pip. Cold: isolated checkout `COLD_OK` + `.venv` left
(`/tmp/lizard-shell-slice1-cold.out`). Warm: execution checkout
`nix develop -c python -c 'import pytest; print("WARM_OK")'` → stdout `WARM_OK`
only, no Requirement/Collecting/Successfully/platformdirs. Overlap: concurrent
`A_OK`/`B_OK` exit 0, no pip chatter; env remains importable. Post-refactor warm
recheck same.

Behavior: Given a repository checkout whose Nix shell preparation has already
succeeded and whose dependency inputs are unchanged, when a contributor or agent
runs `nix develop -c <command>` (including two overlapping warm read-only
invocations), then each command's stdout carries that command's output without
pip installation chatter, the prepared environment remains usable for ordinary
Python/test imports, and a first cold entry on a checkout without a prepared
environment still prepares successfully.

### 2. Refresh on dependency-input change and close DD-003
Type: Behavior
Status: done
Proof: Accepted — stamp of `dev_requirements.txt` at `.venv/.dev_requirements.sha256`.
Isolated `/tmp/lizard-shell-slice2-*`: cold/warm/overlap clean; change requirements
→ refresh pip then POST_REFRESH_WARM_OK without chatter; fail pin surfaces ERROR
non-zero without advancing stamp. DearDough DD-003 Adopted response + relevant-use
(no longer queued-only).

Behavior: Given a prepared environment, when a real dependency input that drives
preparation changes, the next shell entry refreshes preparation to match those
inputs and later unchanged entries reuse again without pip install chatter; when
preparation cannot succeed, the failing entry surfaces the failure; when the
reuse/refresh behavior is delivered, DD-003 records the adopted response and is
no longer only queued.

## Current decisions

- Implementation stays inside project `shell.nix` shellHook (and only related
  local prep artifacts it already owns); do not introduce a second bootstrap
  path or change shared Open Dough guidance.
- "Real dependency inputs" are the inputs that currently drive preparation
  (at minimum `dev_requirements.txt` content that `pip install -r` consumes);
  execution may stamp/hash those without inventing extra product scope.
- Overlapping warm safety is proved as non-mutating reuse on the warm path;
  do not treat "serialize all Nix entry" as the delivered product outcome.

## Learnings

Warm gate `test -x .venv/bin/python` alone is not enough for refresh; hashing
`dev_requirements.txt` into `.venv/.dev_requirements.sha256` and writing the
stamp only after successful pip keeps failed prep retryable and warm
non-mutating. Concurrent warm entries stay safe once prep is skipped on the
warm path.

## Story obligations

### G1. Deferred promises outside this story
Reported: slice 1 — "Deferred promises outside this story (parser/analyzer, performance harness, shared guidance, latency claims, non-Nix installs)."
Story clause: "Deferred promises: parser or analyzer changes; performance-harness redesign; shared Open Dough guidance or host-policy changes; inventing a latency or timing claim beyond before/after evidence of the reported platformdirs/virtualenv mismatch and the observable reuse/refresh behavior; changing how non-Nix workflows install dependencies."
Disposition: excluded "Deferred promises: parser or analyzer changes; performance-harness redesign; shared Open Dough guidance or host-policy changes; inventing a latency or timing claim beyond before/after evidence of the reported platformdirs/virtualenv mismatch and the observable reuse/refresh behavior; changing how non-Nix workflows install dependencies."
Commitment: 21, 21.1, 21.2, 21.3, 21.4, 21.5

## Concern evidence

Slice-plan refinement was not needed: two Behavior slices share one coherent
preparation rule (prepare when missing or inputs changed; otherwise reuse),
proof is outside-in shell observation already demonstrated, and no decisive
premise remains unobserved. No remaining slice-specific concerns.

## Execution complete

Product advice: no change — warm reuse, stamp-guarded refresh, visible prep
failure, and DD-003 adopted relevant-use close the selected story without a
further product follow-up. Process review found no new supported findings
beyond the already-adopted DD-003 response.
