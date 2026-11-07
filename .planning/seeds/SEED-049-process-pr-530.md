---
id: SEED-049
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: Python inline function suite discovery
---

# SEED-049: Process PR #530

<a id="process-pr-530"></a>

### Process PR #530 — Fix Python inline function discovery and suite boundaries

**Identity:** SEED-049#process-pr-530
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/030-process-pr-530/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"2d6240a526d8abe7081ce7d5b5a2d8464caf7bf91ca053f5e2bd43210ccddd8f","plan":"5bf3e69fa994ffd1a3334b0ecf420025327bdc365ae4e4afdabc8d7de71e1419"}}
```

**Goal.** Python users who rely on Lizard to discover methods and nested
functions written as inline suites, and maintainers disposing
[PR #530](https://github.com/terryyin/lizard/pull/530), receive one complete
disposition for this launch: the change lands when the story examples and
nearby controls stay correct under the Fabio cohort's paired performance gate,
or the PR stays unmerged with an evidence-backed reason.

**Source.** Dashboard launch instruction
`lizard process pr https://github.com/terryyin/lizard/pull/530`. Full
process-PR scope. Prior completed owners SEED-042 and SEED-045 already judged
head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` unmerged for the same
performance gate; this identity covers the new launch only.

**Beneficiary.** Python users whose inline suites currently disappear from the
function list, and maintainers deciding whether to accept this contribution.

**Scope.** This pull request only. Reproduce the claimed defect on current
trunk through the public analyzer, compare it with the proposal, and check
nearby Python controls (block suites, nested inline suites, shared-line
docstring NLOC). Correct only on a reachable contributor branch. Qualify with
discriminating behavior and the Fabio cohort paired gate (95% upper slowdown
strictly below 3% on frozen Python `api`, `api-parameters`, and `cli`). Merge
only when those gates and required hosted checks hold; otherwise record the
precise unmet gate. Reuse prior paired evidence when production bytes, corpus,
harness, and head remain compatible.

Deferred promises: the separate multiline f-string `end_line` tokenizer
limitation named in the PR, other languages, and pull requests that are not
#530. Rejection constraints: do not merge an inadequate candidate to finish
the cycle; do not silently drop the Fabio cohort 3% bound that SEED-042
recorded for this contribution.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer. Already-correct trunk, an unworthy change, or a correction that
cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given `class Stream` with `def read(self) -> str: ...` and
  `def close(self) -> None: ...`, both methods appear in the function list.
- Given `def outer` whose body contains `def inner(): return 1`, both `outer`
  and `outer.inner` remain with correct spans.
- Given `def value(): """A value."""; return 1`, the function is discovered
  with NLOC 1 (shared definition line kept when excluding the inline
  docstring).
- Given block-bodied `def kept` with an `if`, CCN stays 2 and NLOC stays 4.

Intake metadata: head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` on
`ahuimanu:fix/python-inline-suites`; PR base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`; cross-repo;
`maintainer_can_modify` false; fork `permissions.push` false; evidence under
`.planning/evidence/pr-530/` and packet
`.planning/pr-530-evidence/`.

**Outcome.** Unmerged. Head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` still
fixes the reported discovery defect on inspection, but reusable SEED-042
paired evidence against unchanged Python reader bytes fails the Fabio cohort
&lt;3% gate on `api` / `api-parameters`, and the fork is not maintainer-writable.
Public judgment:
https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144
Local note: `.planning/evidence/pr-530/SEED-049-disposition.md`
