---
id: SEED-045
status: active
planted: 2026-11-07
planted_during: lizard process pr https://github.com/terryyin/lizard/pull/530
scope: Python inline function discovery and suite boundaries
---

# SEED-045: Process PR #530

<a id="process-pr-530"></a>

### Process PR #530 — Fix Python inline function discovery and suite boundaries

**Identity:** SEED-045#process-pr-530
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"0deb1c805087df03ac8f5beef0ff63a760766f869df6023b8b94d7fa51359504"}}
```

**Goal.** Python users whose functions use an inline suite, and maintainers
disposing [PR #530](https://github.com/terryyin/lizard/pull/530), get one
complete disposition for that contribution: inline suites are reported with
sound boundaries under the story's performance gate, or the PR stays unmerged
with an evidence-backed reason.

**Beneficiary.** People who analyze Python that uses one-line function suites
such as `def read(self) -> str: ...`, and maintainers accepting or declining
this change.

**Source.** Maintainer request `lizard process pr
https://github.com/terryyin/lizard/pull/530`. Head at intake
`2948b38ed0bf70c5a4c39163b45dfa50c3020314` on
`ahuimanu:fix/python-inline-suites`; PR base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`. Cross-repository;
`maintainerCanModify` is false. Prior completed owners SEED-040#process-pr-530
and SEED-042#python-inline-function-suites are history; this identity owns the
new process cycle. Session evidence: `.planning/evidence/pr-530/`.

**Scope.** This PR only: reproduce the claim on the current target, review the
Python reader change and the consumers it reaches, correct defects that a
sound disposition requires when a reachable write path and the performance
gate allow it, qualify the result, and merge when the gates hold or record why
it does not.

Required behavior of a merge disposition:

- Public analysis reports an inline function suite, including a class method
  whose body is `...` on the `def` line.
- An inline suite does not drop the enclosing function, and a bracketed or
  explicit continuation stays inside the function that opened it.
- An f-string interpolation does not move the surrounding function's
  indentation boundary.
- A block-bodied function keeps the metrics it has on the current target.
- A one-line function whose suite is a docstring, or a docstring plus another
  statement, still counts the definition line. `def value(): """A value.""";
  return 1` reports `value` with NLOC 1.
- Discriminating tests, the full suite, style checks, and the required CI jobs
  that apply to this diff all hold for the revision that is merged.
- Performance for this per-token reader change uses the installed paired
  benchmark. Compare the corrected candidate with the current target on
  `api`, `api-parameters`, and `cli`. Each workload's paired 95% upper
  slowdown bound is strictly below 3%. This story records that screening
  bound for itself. There is no cohort cumulative baseline.

Deferred promises: the multiline f-string `end_line` limitation the PR leaves
out of this correction; closed PRs #526 and #529; other languages. Rejection
constraints: do not merge an inadequate candidate; do not add a second Python
parser; do not weaken an expectation to hide a failed result; do not treat the
prior SEED-042 reject as automatic clearance without confirming current head,
trunk reader bytes, and gate applicability.

**Key examples.**

- Given this source, public analysis reports `read` and `close`. On current
  trunk the analyzer returns no functions:

  ```python
  class Stream:
      def read(self) -> str: ...
      def close(self) -> None: ...
  ```

- Given a block function whose body contains an inline function, analysis
  reports both the inner function and the outer function.
- Given a function header or argument list that continues inside brackets or
  with an explicit continuation, the following suite still belongs to that
  function.
- Given an f-string whose interpolation contains a decision, that decision
  still counts, and the next function keeps its own boundary.
- Given `def kept(x):` with an `if` body, analysis still reports `kept` with
  CCN 2.
- Given `def value(): """A value."""; return 1`, analysis reports `value`
  with NLOC 1.
- Given a candidate whose paired 95% upper slowdown is not strictly below 3%
  on every required workload, trunk stays unchanged and the PR stays unmerged.

**Outcome.** Unmerged. Head remained
`2948b38ed0bf70c5a4c39163b45dfa50c3020314`. On current trunk the `Stream`
inline methods still produce no functions while block `kept` stays CCN 2 /
NLOC 4. Python reader files are unchanged since the SEED-042 measurement
baseline, so that paired evidence still applies: neither the PR head plus
shared-line NLOC fix nor the cheaper structure-only alternative cleared every
required `<3%` upper bound on `api` / `api-parameters` / `cli`. The published
maintainer judgment stands:
https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144
No further acknowledgement or force-push request. Session evidence:
`.planning/evidence/pr-530/`.
