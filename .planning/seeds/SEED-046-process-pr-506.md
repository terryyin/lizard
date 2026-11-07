---
id: SEED-046
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: Website /analyse resource limits (CWE-400)
---

# SEED-046: Process PR #506

<a id="process-pr-506"></a>

### Process PR #506 — Bound public /analyse requests against resource exhaustion

**Identity:** SEED-046#process-pr-506
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"63ea25949332dcc46793b1685bc5a942ba8ac0b72a9adb9bfc0baef81890ecca"}}
```

**Goal.** Maintainers disposing
[PR #506](https://github.com/terryyin/lizard/pull/506), and users of the
online demo, receive one complete disposition: the public analyser rejects
oversized request bodies and source payloads so a remote caller cannot exhaust
CPU or memory through `/analyse`, or the PR stays unmerged with an
evidence-backed reason when trunk already provides that protection.

**Source.** Dashboard ad-hoc instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/506. Full process-PR scope: review,
reproduce, correct on the contributor branch when that route exists, qualify,
and merge when the skill's gates hold.

**Beneficiary.** Operators of the public Lizard demo and maintainers deciding
whether to accept this automated CWE-400 suggestion.

**Scope.** This pull request only. Compare the contributor's
`MAX_CONTENT_LENGTH = 1 MiB` change against current trunk's request and source
limits through the website adapter and its tests. Correct a worthwhile gap only
on a reachable contributor branch, preserving contributor history. Merge only
when those gates and required hosted checks hold; otherwise record the precise
unmet gate or already-fixed evidence.

Deferred promises: other endpoints, analyzer CLI limits, and pull requests that
are not #506. Rejection constraints: do not merge an obsolete or conflicting
candidate to finish the cycle; do not weaken trunk's tighter source bound to
match the PR.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given a JSON `/analyze` body whose UTF-8 source exceeds 64 KiB, the service
  responds 413 with the documented limit message and accepts a later in-limit
  retry.
- Given a request body larger than the configured content length, the service
  responds 413 without reflecting the oversized payload.
- Given the PR's sole change of setting `MAX_CONTENT_LENGTH` to 1 MiB on the
  pre-revival `index.py`, decide whether that adds protection trunk lacks or is
  already superseded.

Intake metadata: head `5b5608f2b6006043baf39f162251214620ed90ba` on
`anupamme:fix-repo-lizard-multi-agent-cwe-400-index-py`; PR base
`0488a260e74e73ceba3149d7c600faac6a3616e6`; `maintainer_can_modify` true;
merge state CONFLICTING/DIRTY; evidence under `.planning/evidence/pr-506/`.

**Outcome.** Already fixed / superseded on trunk; closed unmerged. Current
`index.py` sets `MAX_CONTENT_LENGTH` to 512 KiB and `MAX_SOURCE_BYTES` to
64 KiB with 413 JSON handling (homepage revival `f818daf65`, after this PR's
base). The PR only adds a 1 MiB content-length cap to the pre-revival adapter
and conflicts with the rewritten file. Website limit tests:
`test_source_limit_counts_utf8_bytes_and_allows_retry`,
`test_request_body_limit_is_json_and_source_is_not_reflected`, and
`test_source_at_the_documented_limit_is_accepted` — 4 passed. Closing comment:
https://github.com/terryyin/lizard/pull/506#issuecomment-6699790865
