# PR #525 correction qualification

Accepted runtime/test bytes: `evidence/accepted-candidate.json`. Frozen current product baseline `6d288d4cdcb85bc50ab4c8a887196177607eeb7d`; original parent `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`. Admission/preparation master `1ba5cb5e619af3d04ad8eb8f4d3325c300660d75` has identical runtime bytes.

The binding owner retains actual/provisional formals; proven calls reuse ordinary nested syntax with an explicit argument-comma context. Replay does not feed brackets twice; subscript/template brackets preserve callbacks and subsequent functions. Public tests cover eleven authored examples, seven suffixes, typed/default/ternary formals, NS/nonstrict, CSV CLI and the real HTTP endpoint.

Independent refactor 1 found no additional work; refactor 2 consolidated dispatch; the G7 correction then removed full tracking from proven calls. Independent final refactor 3 found the resulting concept clean. See `implementation/README.md`, `cost/README.md`, `refactor2/`, `cost-return/README.md` and `refactor3/README.md`.

Final full commands (serialized Nix): `nix develop -c python -m pytest` and `nix develop -c make`, run by `evidence/verify_final.py --out .../verification2`. Both exited 0: **2427 passed, 7 existing skips**. Make includes the configured style checks; existing pylint diagnostics remain. Raw terminal results are in `verification2/`.

## Paired qualification

All eight comparisons pass. Twenty alternating warmed pairs each; API/parameters and NS calibrate to 1.2 seconds, broad CLI to 4 seconds. Every measured batch exceeds one second, source/corpus/harness hashes remain stable, and each paired 95% upper slowdown bound is strictly below 3%. Inputs were frozen prospectively and retained unchanged.

| Comparison | Paired change | Paired 95% interval | Minimum batch |
| --- | ---: | ---: | ---: |
| current-api | -5.965% | [-7.093%, -4.824%] | 1.237s |
| current-ns | -4.951% | [-6.324%, -3.557%] | 1.214s |
| arguments-api | -1.130% | [-1.721%, -0.535%] | 1.188s |
| arguments-ns | -1.449% | [-1.934%, -0.961%] | 1.172s |
| current-cli | -1.176% | [-2.328%, -0.010%] | 4.135s |
| parent-api | -13.443% | [-14.010%, -12.874%] | 1.214s |
| parent-ns | -10.335% | [-11.001%, -9.664%] | 1.190s |
| parent-cli | -0.442% | [-1.401%, +0.526%] | 3.843s |

Negative change means faster. Worst upper bound: **+0.526%**. Exact commands, durations, machine/interpreter/options, imported paths, production/input/harness hashes and observation stability are retained in `measurements/attempt2/`. `evidence/timing-host2.json` records the serial-run decision.

The unfavorable `measurements/attempt1/` is preserved: its isolated call API workload had +5.554% slowdown, upper +6.048%, despite broad API passes. Profiling that exact candidate showed unnecessary full call tracking; the final change removes 35,200 binding-token reads and 3,200 binding candidates on forty fixed sources × forty repeats. This profile establishes removed work; the paired matrix above establishes acceptance.

## Compatibility limits and delivery ownership

No expectation was weakened. The pre-existing labeled-body arrow omission, typed-class-field false positive and anonymous multiline-generic truncation remain separately dispositioned in the plan and retained baseline observations. Those observations establish preservation, not correctness improvements at those unrelated boundaries.

Contributor history is preserved at qualified head `e68c735e11564e3afb7b543f8b3ad7cc1a06b3a0`. The guarded merge completed as `b97f7958bd63bdafce692cf926c70d927410717b`. Both exact-head matrices and change jobs passed; web skips matched the path filter. Merged-master run 41329592732 also passed. One acknowledgement is https://github.com/terryyin/lizard/pull/525#issuecomment-6561098652. `hosting/` retains the fresh packet, guard, wait records, merge confirmation and acknowledgement.
