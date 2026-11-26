# Qualify and merge the held C-family PRs

**Identity:** SEED-078#c-family-pr-timing

**Source:** [Qualify and merge the held C-family PRs](../../seeds/SEED-078-pr-batch-follow-ups.md#c-family-pr-timing)

## Goal and scope

C, C++, Java, C#, Objective-C and Dart users get the C++ raw-string fix ([#619](https://github.com/terryyin/lizard/pull/619)) and the rvalue-reference CCN fix ([#688](https://github.com/terryyin/lizard/pull/688), then [#681](https://github.com/terryyin/lizard/pull/681)) on master. The merge happens only after a paired timing shows every affected workload strictly below the 3% upper bound. SEED-077 already accepted their correctness. This plan does not cover re-review, changes to the 3% policy or timing metric, or a changelog entry.

## Current decisions

- **Gate candidate:** current master with #681 merged (which contains #688), then #619. Time each PR alone only to attribute a miss. The seed records this choice.
- **Workloads**, each against current master: the API workloads on the C/C++, Java, C# and Dart corpora; `-Eignoreassert` on the C/C++ and Java corpora; `-Enonstrict` on the C/C++ corpus; and the CLI on the broad corpus of all extracted snippets, with `--seconds 4` chosen before the run. Use the runner `.agents/skills/lizard-process-pr/scripts/paired_benchmark.py` with its defaults otherwise. One heavy run at a time.
- **Corpora:** capture the `FileAnalyzer.analyze_source_code` inputs from the base `test/` suite on current master by wrapping that method outside the repository (see premises), and add the three PRs' own test sources. Group them by extension into one directory per language, then freeze them by hash before the first timing. Use the same corpus for baseline and candidate. Keep evidence under the Git-ignored `.planning/evidence/c-family-pr-timing/`. Do not commit it.
- **Result states** follow [performance](../../../.agents/skills/lizard-process-pr/references/performance.md). A missed bound leads to profiling, then a correction or a quieter retry, as the seed describes. If the miss comes only from correct tokens that master skips, it goes to the developer. Record the load average with every run and keep every run.
- **Delivery:** execute in Trunk Mode (`--trunk`). Merge the contributor PRs through GitHub, guarded by each PR's exact head SHA. Merge #688 first, then #681, then #619. Merge each only after its hosted checks pass on that head. Before each merge, refresh the head and base. A moved head reopens only the proof it affects. Post a thank-you, and a note of any correction, only after the merge is confirmed.
- **Local gate:** before timing, run `nix develop -c python -m pytest` and `nix develop -c make` on the combined candidate. SEED-077 reviewed the PRs on an older master. [AGENTS.md](../../../AGENTS.md) requires the full suite before a change is complete.
- No Accepted ADR is affected ([0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md) covers only terminology). The PRs change existing owners: the token pattern in `clike.py`, the `CppRValueRefStates` parallel state, and the `lizardignoreassert` extension.

## Decisive premises

Observed in this preparation workspace on 2026-11-26. `origin/master` was `51e06684`. Scratch builds, corpora and runner output stayed outside the repository under the job's temporary directory. Commands ran under `nix --extra-experimental-features 'nix-command flakes' develop -c`.

| Premise and operation | Observation and result |
| --- | --- |
| The PRs combine cleanly on current master. | `git merge-tree --write-tree` gave clean merges for master + #688, master + #681, master + #619, and (master + #681) + #619. The combined tree is `b39803a7`. Heads: #619 `e4679972`, #681 `ec3d383a`, #688 `352020cf`. All three allow maintainer edits. |
| The runner times a combined candidate built from a merge tree. | `git archive` of master and of `b39803a7` into separate directories. `paired_benchmark.py --workload api` on a dense C++ snippet returned the JSON verdict `bound-not-met`, 95% CI +22.6% to +37.4%. For #619 alone, the CI was +19.4% to +28.6%. |
| That dense miss is master mis-tokenizing, not a slower rule. | A tokenizer-only A/B of the old and new raw-string rule, run on the same text, gave 15,400 tokens with master and 20,800 with #619, and +25.6% time. On 1.45M characters of `test/**/*.py`, where the token counts barely differ, the A/B gave −0.4% (CI −1.6% to +0.7%). |
| Base-test snippets can be captured as corpora. | Wrapping `lizard_core.file_analyzer.FileAnalyzer.analyze_source_code` around `pytest test/test_languages` wrote 4,323 unique inputs: 155 `.cpp`, 87 `.java`, 36 `.dart`, 18 `.cs` and 17 `.m`. The run hung at 0% CPU in a test not yet identified and was stopped after 10 minutes. Capturing one file at a time with a timeout avoids the hang. |
| The CLI-with-extension and API routes reach the C++ corpus. | On the 155 `.cpp` snippets at load averages of 5–7: `--workload cli --extension ignoreassert` passed with a CI of −1.8% to +2.0%. `--workload api` passed at −9.8% to +1.0%. These are route checks, not the gate: the corpus was not frozen and lacked the PRs' cases. |

## Ordered slices

### 1. Combined candidate qualified against master on every affected workload

Type: Behavior

Status: done

Behavior: Build the combined candidate (master + #681 + #619) and freeze the corpora. Run the paired workloads in the order listed under Current decisions. Each workload ends with an evidence file and a verdict. All of them pass, or every miss has a profile and a decision: a correction on the contributor's branch, a linked correction story with that PR left unmerged, a quieter retry chosen before launch, or the developer's decision on a miss from master's mis-tokenization.

| Promise | Proof owned by this slice |
| --- | --- |
| The fixes still hold on current master | The seed's key examples `R"d(… if (x) …)d"`, `void f(T&& x)` and `a && (b = c)` pass through `analyze_file` on the combined candidate, and fail or differ on master. Full `pytest` and `make` pass on the combined candidate. |
| Each workload stays strictly under the 3% bound | One `paired_benchmark.py` JSON per workload under `.planning/evidence/c-family-pr-timing/`, with verdict `passes`. Record the corpus hash, revisions and load in the plan's accepted proof. |
| A correction keeps its proof | When a correction is pushed, rerun only the affected workloads and the full suite on the corrected head. Keep the earlier runs. |

#### Accepted proof (2026-11-27)

- **Revisions.** The baseline is `origin/master` `5294e585`, which includes `60a63ca6`, the production change to wrapped type reporting made after preparation. The candidate tree is `0f6daf21`: master + #681 `ec3d383a` (which contains #688 `352020cf`), then + #619 `e4679972`. Both are `git archive` copies outside the repository.
- **Key examples** (`analyze_file`, CCN on master → candidate). The `R"d( ") if (x) { y; } )d"` raw-string function goes from 2 to 1, and the next function keeps CCN 2. `f(T&& x)` with a `T&&` local goes from 2 to 1. `if (a && (b = c))` goes from 2 to 3.
- **Local gate on the candidate.** `pytest` gave 4094 passed and 7 skipped. `make` exited 0.
- **Corpora** were captured from `FileAnalyzer.analyze_source_code` across master's `test/` suite, one file at a time with a 120 s timeout. Only `testHelpers.py` collected no tests. The PRs' tests (`testCAndCPP.py` and `test_cpp_delimited_raw_strings.py`) were added from the candidate. Frozen read-only, with file count and hash prefix: C/C++ 450 `5bd1596e`, Java 106 `36b9e64d`, C# 35 `6041f875`, Dart 37 `797f2207`, broad 4,939 `650e81bb`.
- **Gate results.** Each workload's passing run is listed below. Every run is kept under `.planning/evidence/c-family-pr-timing/run-N/`, with a `DECISION.txt` written before each retry.

  | Workload | Passing run | Change | 95% CI | Load before → after |
  | --- | --- | --- | --- | --- |
  | C/C++ API | 2 | −4.9% | −10.9 to +1.5 | 3.8 → 6.9 |
  | Java API | 4 | −2.3% | −4.3 to −0.2 | 2.3 → 3.4 |
  | C# API | 3 | −0.9% | −2.5 to +0.8 | 2.8 → 4.1 |
  | Dart API | 3 | +0.9% | −0.7 to +2.5 | 3.0 → 3.7 |
  | C/C++ `-Eignoreassert` | 2 | −3.4% | −7.6 to +1.0 | 3.7 → 4.6 |
  | Java `-Eignoreassert` | 2 | −2.6% | −4.4 to −0.8 | 3.7 → 3.6 |
  | C/C++ `-Enonstrict` | 2 | −0.1% | −2.0 to +1.9 | 3.6 → 3.4 |
  | Broad CLI | 3 | +1.2% | −0.4 to +2.8 | 2.7 → 3.4 |

- **Misses and retries.** Run 1 used the default settings at load 6–13. It gave five bound-not-met results and three inconclusive short-batch results.
  - Profiling the candidate found no added cost. It made fewer calls than master on C++: 1.83M vs 1.88M plain, and 1.90M vs 2.01M with `-Eignoreassert`.
  - Run 2 used `--seconds 4` and started each workload below load 4. Four workloads missed with wide intervals as load rose during the runs.
  - Run 3 repeated those four with `--pairs 40`, starting below load 3. Only Java API missed (+1.6%, CI −0.5 to +3.8).
  - Java profiling showed +0.15% calls: `CLikeReader.__init__` constructs `CppRValueRefStates`, which `JavaReader` then discards. A tokenizer-only A/B of #619's rule on the Java corpus gave +0.6% (CI −0.9 to +2.1) with identical token counts. With no correction target, run 4 timed Java alone with `--seconds 8 --pairs 40` and passed.
  - No PR was corrected.

#### Learnings

- On this shared 16-core host, other agents raise the load during a run even when it starts quiet. Calibrated 1.2 s batches gave ±10% intervals there. Gate on 4–8 s batches, start only below load 3, and expect hour-long waits.

### 2. Qualified PRs merged on master

Type: Behavior

Status: planned

Behavior: For each qualified PR, in the order #688, #681, #619: refresh its head, base and hosted checks, and confirm that the head matches the timed candidate. Merge it with an exact head-SHA guard, confirm the merge on `origin/master`, then post the thank-you. A PR deferred in slice 1 stays open with a link to its correction story.

| Promise | Proof owned by this slice |
| --- | --- |
| Users get the fixes | After merging, `origin/master` contains each merged head, and hosted CI on the merge commits is green. |
| Disposition recorded | The seed records merged, corrected or deferred for each of the three PRs. Each merged PR has the acknowledgement comment URL. |

## Concern evidence

Slice-plan refinement was not needed: there are two slices with one outcome each, and qualification comes before delivery. No remaining slice-specific concern was identified. If a correction is needed, its size is unknown until the profile. The plan bounds that with the deferral route, so it is not a readiness concern.
