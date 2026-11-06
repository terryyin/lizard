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

**Goal.** For JavaScript/TypeScript users of `-ENS` / `max_nested_structures`
who need `.catch` / `?.catch` (and similar member names) not counted as nested
structures, land a performance-qualified fix so those member names are skipped
while real `if` / `try`/`catch` still raise NS, with every required paired
performance gate’s 95% upper slowdown strictly below 3% — including cumulative
`api-parameters` under `--extension ns` against the frozen accepted-set baseline.

**Scope.** Continue from the SEED-034 review of
[PR #554](https://github.com/terryyin/lizard/pull/554). Semantic candidate is
adequate (as-authored `_member_name` after `.` / `?.` in `lizard_ext/lizardns.py`
plus discriminating test). Do not reopen unrelated FabioMalpezzi PRs.

**In scope:** reduce or otherwise qualify cumulative NS `api-parameters` cost so
the candidate (or a ≤15-minute refinement of it) passes all six incremental and
cumulative workloads with `--extension ns`, then merge #554 (or an equivalent
exact-head revision) with acknowledgement.

**Out of scope:** other language readers; changing NS semantics for real
structures; inventing a parallel counter.

**Key examples / reproducer.**

```js
function load(p) {
  return p.then(() => 1).catch(() => null);
}
function retry(p) {
  p?.catch((e) => { if (e) { return 1; } });
  try { p.finally(); } catch (e) { return 2; }
}
```

Under `-ENS`, expected `max_nested_structures`: anonymous/load/retry as in
`test_member_named_like_a_structure_is_not_a_structure` (`load` 0, `retry` 1,
callbacks accordingly). Current trunk still treats member `catch` as a structure.

**Expected result.** Claim fixtures green; focused NS/JS/TS suite and full
`pytest` / `make` / required CI green; paired 95% upper slowdown strictly below
3% on api, api-parameters, and CLI — incremental vs current target and cumulative
vs the frozen baseline — with `--extension ns` on every required workload.

**Remaining work.** Profile or otherwise explain the quiet-load cumulative
`api-parameters` ~3.6% overage recorded under
`docs/reviews/fabio-malpezzi-SEED-034/pr554/attempt5/` (incremental #554 alone
was a slight speedup). Apply only ≤15-minute product changes if needed, re-run
the contract harness, merge exact head, thank the contributor.

**Source PR:** https://github.com/terryyin/lizard/pull/554
**Evidence:** `docs/reviews/fabio-malpezzi-SEED-034/pr554/`
