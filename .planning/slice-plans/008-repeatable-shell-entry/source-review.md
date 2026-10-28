# Source review — SEED-018#repeatable-shell-entry

Launch: fresh general-purpose reviewer; inputs were only the selected story's
Goal, Scope, key examples, exclusions, and deferred promises from
`.planning/seeds/SEED-018-repeatable-shell-entry.md#repeatable-shell-entry`.
No plan, product, proof, or conversation inheritance.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Beneficiaries are contributors and agents who prefix local tooling with `nix develop -c` (and interactive `nix develop`) in this repository. | slice 2 | observed: flake.nix still imports shell.nix; entry remains `nix develop` / `nix develop -c` |
| 2 | On unchanged shell entry, reuse a correctly prepared development environment. | slice 1 | observed: shell.nix warm path activates existing `.venv` without recreate/pip; coordinator `WARM_OK` / overlap `A_OK`/`B_OK` with no pip chatter |
| 2.1 | This includes overlapping warm, read-only invocations. | slice 1 | observed: concurrent warm `nix develop -c` both exit 0 with `A_OK`/`B_OK`, no pip chatter |
| 2.2 | Repeated commands avoid reinstall churn. | slice 1 | observed: warm path skips `python -m venv` and `pip install` when `.venv/bin/python` exists |
| 2.3 | Repeated commands avoid pip installation chatter on stdout. | slice 1 | observed: warm stdout is command output only (`WARM_OK`); no Requirement/Collecting/Successfully/platformdirs lines |
| 3 | When real dependency inputs that drive preparation change, refresh preparation so the environment matches those inputs. | slice 2 | observed: stamp hash of `dev_requirements.txt`; changed file → pip refresh (`/tmp/lizard-shell-slice2-refresh.out`) |
| 4 | Keep supported development tools usable. | slice 2 | observed: pytest/pygments/flask imports after cold/warm/overlap/refresh (`REFRESH_IMPORTS_OK`) |
| 5 | Make preparation failures visible. | slice 2 | observed: failing pip surfaces ERROR and non-zero; command not reached (`/tmp/lizard-shell-slice2-fail.out`) |
| 6 | Scope is project-owned Nix development-shell preparation for this repository (`shell.nix` / flake `devShell` and the local environment it prepares). | slice 1 | observed: only `shell.nix` `shellHook` changed; flake still imports `./shell.nix` |
| 7 | Cold first entry prepares a usable environment. | slice 1 | observed: isolated cold run printed `COLD_OK`, exit 0, left `.venv/bin/python` (`/tmp/lizard-shell-slice1-cold.out`) |
| 8 | Warm re-entry with unchanged dependency inputs reuses that environment. | slice 1 | observed: warm `nix develop -c` activates existing env; `WARM_OK` |
| 8.1 | Without reinstalling. | slice 1 | observed: warm `shellHook` else-branch only `source .venv/bin/activate` |
| 8.2 | Without emitting pip install chatter into command stdout. | slice 1 | observed: warm stdout `WARM_OK` only; NO_PIP_CHATTER_OK |
| 9 | Overlapping warm read-only entries in the same checkout succeed without requiring serialized recovery. | slice 1 | observed: concurrent A/B exit 0; env still importable; no ensurepip recovery |
| 10 | When real dependency inputs that drive preparation change, the next entry refreshes so the environment matches those inputs. | slice 2 | observed: stamp mismatch triggers `pip install -r`; stamp updates only after success |
| 11 | Preparation failures remain visible on the failing entry. | slice 2 | observed: fail entry ERROR lines; stamp not advanced on failure |
| 12 | Existing supported development tools and imports used for ordinary local work stay usable after cold, warm, overlap, and refresh cases. | slice 2 | observed: COLD_OK/WARM_OK/A_OK/B_OK/REFRESH_IMPORTS_OK |
| 12.1 | Python/test tooling. | slice 2 | observed: `import pytest` across cold/warm/overlap/refresh |
| 12.2 | The shell’s declared packages. | slice 2 | observed: pygments/flask import after refresh |
| 13 | After delivery, record the adopted response revision and relevant-use outcome at DD-003 in DearDough.md. | slice 2 | observed: DearDough.md DD-003 Adopted response/revision/Relevant-use; no longer queued-only |
| 13.1 | Queueing alone does not resolve the finding. | slice 2 | observed: Follow-up text changed from queued, not resolved to Adopted response + relevant-use |
| 14 | Justified rejection constraints: none beyond failing visibly when preparation cannot succeed. | slice 2 | observed: shellHook exits on venv/pip/stamp failure; fail proof surfaces ERROR |
| 15 | Evaluation observes cold setup, warm repeated entry, overlapping warm entry, and a dependency-input change; demonstrate usable Python/test imports and command stdout in each relevant case; retain before/after evidence of the reported platformdirs/virtualenv mismatch without inventing a latency claim. | slice 2 | observed: `/tmp/lizard-shell-slice2-*.out`; platformdirs line retained on cold/refresh pip; no latency claim |
| 16 | Cold setup: with no prepared local environment present, `nix develop -c` runs a supported Python or pytest import/check successfully and leaves a prepared environment for later entries. | slice 1 | observed: cold `import pytest; print("COLD_OK")` succeeded and left `.venv` |
| 17 | Warm reuse: after a successful cold setup and with dependency inputs unchanged, a later `nix develop -c <command>` completes with that command’s stdout free of pip installation chatter, and the same Python/test imports remain usable without a reinstall cycle. | slice 1 | observed: warm `WARM_OK` with clean stdout; pytest import usable |
| 18 | Overlapping warm entry: with a prepared environment and unchanged inputs, two concurrent read-only `nix develop -c` invocations in the same checkout both succeed and leave a usable environment (no ensurepip/race recovery required). | slice 1 | observed: A_OK/B_OK concurrent; AFTER_OVERLAP importable in agent proof; coordinator overlap re-check clean |
| 19 | Dependency-input change: after warm reuse works, changing a real dependency input that drives preparation and re-entering refreshes so the environment reflects that change; subsequent warm entry again reuses without pip install chatter. | slice 2 | observed: refresh then POST_REFRESH_WARM_OK with no pip chatter |
| 20 | Visible failure: when preparation cannot succeed, the failing entry surfaces the failure rather than silently leaving a broken shell for later commands. | slice 2 | observed: fail.out ERROR; SHOULD_NOT_RUN absent |
| 21 | Outside this story (deferred promises). |  | G1 |
| 21.1 | Parser or analyzer changes. |  | G1 |
| 21.2 | Performance-harness redesign. |  | G1 |
| 21.3 | Shared Open Dough guidance or host-policy changes. |  | G1 |
| 21.4 | Inventing a latency or timing claim beyond before/after evidence of the reported platformdirs/virtualenv mismatch and the observable reuse/refresh behavior. |  | G1 |
| 21.5 | Changing how non-Nix workflows install dependencies. |  | G1 |
