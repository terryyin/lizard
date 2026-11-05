---
id: SEED-040
status: active
planted: 2026-11-06
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/530
scope: Python inline function discovery and suite boundaries
---

# SEED-040: Process PR #530

## Stories

<a id="process-pr-530"></a>

### Process PR #530 — Python inline function discovery

**Identity:** SEED-040#process-pr-530

**Goal.** Python users whose functions use an inline suite, and maintainers
deciding [PR #530](https://github.com/terryyin/lizard/pull/530), get one
complete disposition for that contribution: inline suites are reported with
sound boundaries, or the PR stays unmerged with an evidence-backed reason.

**Beneficiary.** People who analyze Python that uses one-line function suites
such as `def read(self) -> str: ...`, and maintainers accepting or declining
this change.

**Source.** Maintainer request `/lizard-process-pr
https://github.com/terryyin/lizard/pull/530`. Head at intake
`2948b38ed0bf70c5a4c39163b45dfa50c3020314` on
`ahuimanu:fix/python-inline-suites`; base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`. The PR is open, cross-repository,
and `maintainerCanModify` is false. Refresh head, base, and checks before any
dependent action.

**Scope.** This PR only: reproduce the claim on the current target, review the
Python reader change and the consumers it reaches, correct defects that a
sound disposition requires, qualify the result, and merge when the gates hold
or record why it does not.

Required behavior of a merge disposition:

- Public analysis reports an inline function suite, including a class method
  whose body is `...` on the `def` line.
- An inline suite does not drop the enclosing function, and a bracketed or
  explicit continuation stays inside the function that opened it.
- An f-string interpolation does not move the surrounding function's
  indentation boundary.
- Block-bodied functions and recovery from a malformed declaration keep their
  existing behavior aside from the inline-suite correction.
- Discriminating tests, the full suite, style checks, required CI, and
  performance evidence for this per-token reader change all hold for the
  revision that is merged.

Deferred promises: the multiline f-string `end_line` limitation the PR leaves
out of this correction; closed PRs #526 and #529, which discussion says were
withdrawn in favour of this one; other languages. Rejection constraints: do
not merge an inadequate candidate; do not add a second Python parser; do not
weaken an expectation to hide a failed result.

**Key examples.**

- Given this source, public analysis reports `read` and `close`. The PR
  reports the current target returns no functions:

  ```python
  class Stream:
      def read(self) -> str: ...
      def close(self) -> None: ...
  ```

- Given a function whose suite is inline and itself contains an inline
  function, analysis still reports the outer function.
- Given a function header or argument list that continues inside brackets or
  with an explicit continuation, the following suite still belongs to that
  function.
- Given an f-string whose interpolation contains newlines or braces, the
  function that contains the f-string keeps the boundary it had without that
  interpolation.
- Given a block-bodied function and a malformed `def`, analysis still reports
  the block-bodied function and still recovers.
