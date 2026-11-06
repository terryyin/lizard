---
id: SEED-035
status: active
planted: 2026-11-06
planted_during: SEED-034#process-fabio-malpezzi-prs non-merge disposition for PR #554
scope: Nested-structures extension performance for member names that look like structures
---

# SEED-035 — NS member-name skip under the performance gate

<a id="ns-member-name-performance"></a>

### Land nested-structure member-name skip under cumulative NS performance

**Identity:** SEED-035#ns-member-name-performance
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/026-ns-member-name-performance/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"7e307459c18354932fe9c319f8e7d8d8d5bac03670e68c3e5629a3c6a1eb87fe","plan":"c31537f258f60523d33ebe4c5796d65eab2d3d7109c97a87bc53c3a34b4755fe"}}
```

**Goal.** JavaScript/TypeScript users of `-ENS` / `max_nested_structures`
get accurate nesting when a structure word is a member name (`.catch`,
`?.catch`, and similar): those names are not counted as nested structures,
while real `if` / `try`/`catch` still raise NS. Maintainers land
[PR #554](https://github.com/terryyin/lizard/pull/554) (or an equivalent
exact-head revision) only after every required paired performance gate’s
95% upper slowdown is strictly below 3% under `--extension ns`, including
cumulative `api-parameters` against the frozen accepted-set baseline, then
acknowledge the contributor.

**Beneficiary.** `-ENS` users of promise/member `catch` (and similar), and
maintainers closing the SEED-034 non-merge of #554.

**Source.** SEED-034 disposition left #554 unmerged: semantic candidate
adequate; quiet-load cumulative `api-parameters` under `--extension ns`
failed the strict &lt;3% bound (attempt5 median ~3.6%, CI upper ~4.5%).
Evidence: `docs/reviews/fabio-malpezzi-SEED-034/pr554/`. Head at disposition
`2368413b2de05b05ff44ce863dcded8061342492` (OPEN; hosted CI green). Refresh
head, base, and checks before any dependent action. Incremental target after
#553 was `ed78b3b13240297681f82fe6a32c84c377498114`; frozen cumulative
baseline `4a05e4056cebb2edd75cb02b7a43e911369f4ce5`.

**Scope.** Land the member-name skip for the nested-structures extension
under the Fabio cohort complete-cycle performance contract for this story’s
required workloads. Semantic starting point is the as-authored
`_member_name` after `.` / `?.` in `lizard_ext/lizardns.py` plus
`test_member_named_like_a_structure_is_not_a_structure`. Product changes
beyond that patch are in scope when needed so the gates pass; this story is
the follow-up that exists because the SEED-034 ≤15-minute product budget
could not clear the cumulative overage.

Required behavior of a merge disposition:

- Under `-ENS`, a structure word immediately after `.` or `?.` is not a
  nested structure; real statement structures still raise NS.
- Discriminating public analysis matches the key examples below (and the
  named test’s expected names/NS values).
- Focused NS/JS/TS coverage that the change reaches, full `pytest`, `make`,
  and required hosted CI hold on the exact head that is merged.
- Performance uses the installed paired benchmark with `--extension ns` on
  every required workload: `api`, `api-parameters`, and `cli`, each as
  incremental vs current target and cumulative vs the frozen accepted-set
  baseline. Every workload’s paired 95% upper slowdown is strictly below
  3%. Incomplete or contaminated runs (including attempt5’s unfinished
  cumulative CLI) do not count as a pass; quiet-load decisive results are
  required.
- After merge, the contributor receives acknowledgement of the landed work.

Deferred promises: other language readers; other open FabioMalpezzi PRs;
changing how NS counts real control structures; inventing a parallel nesting
counter; relaxing or replacing the strict &lt;3% / cumulative frozen-baseline
contract for this merge.

Rejection constraints: do not merge without every required paired gate
passing under `--extension ns`; do not waive cumulative `api-parameters` or
treat incremental-only speedup as sufficient; do not reopen unrelated PR
dispositions; do not weaken discriminating expectations to hide a fail; do
not repeatedly time the same failing candidate seeking a pass without a
product or contract-compatible measurement change.

**Key examples.**

- Given the source below under `-ENS`, public analysis reports
  `(anonymous)` 0, `(anonymous)` 0, `load` 0, `(anonymous)` 1, `retry` 1
  (as in `test_member_named_like_a_structure_is_not_a_structure`). Current
  trunk still treats member `catch` as a structure.

  ```js
  function load(p) {
    return p.then(() => 1).catch(() => null);
  }
  function retry(p) {
    p?.catch((e) => { if (e) { return 1; } });
    try { p.finally(); } catch (e) { return 2; }
  }
  ```

- Given only the as-authored #554 candidate on a quiet host, incremental
  `api` / `api-parameters` / `cli` under `--extension ns` meet the &lt;3%
  upper bound (attempt5), while cumulative `api-parameters` against the
  frozen baseline does not — so merge stays blocked until that cumulative
  gate (and any still-incomplete required workload) passes.
- Given a product or measurement change that makes every required paired
  workload pass under `--extension ns`, the exact head merges with
  acknowledgement; without that, #554 stays open and unmerged.
