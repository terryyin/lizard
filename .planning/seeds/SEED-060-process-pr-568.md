---
id: SEED-060
status: active
planted: 2026-11-16
planted_during: lizard process pr
scope: Python NLOC of prefixed triple-quoted strings (PR #568)
---

# SEED-060: PR #568 Python prefixed triple-quoted string NLOC

<a id="process-pr-568"></a>

### Process PR #568 — Python NLOC of prefixed triple-quoted strings

**Identity:** SEED-060#process-pr-568

**Goal:** Python users measuring NLOC, and maintainers processing
[PR #568](https://github.com/terryyin/lizard/pull/568) by FabioMalpezzi,
reach an evidenced disposition: a prefixed triple-quoted string used as an
expression (e.g. `return f"""..."""`) and the literal chunks of an f-string
count toward function NLOC instead of being subtracted as docstring-like
comments, so NLOC is never zero or negative for such functions, while real
docstrings remain excluded.

**Source:** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/568 (head
`ae16391d7c1cbdea5686f2f7e8426d23b6ea2a30`, base `master`).

**Beneficiary:** Python projects whose NLOC and NLOC-derived thresholds are
distorted by prefixed triple-quoted strings; maintainers deciding the PR.

**Scope:** Review, correct when bounded, qualify (including performance for
the Python reader) and dispose of PR #568. Deferred: other languages' string
handling and unrelated Python reader changes.

**Key examples:**

- Given `def build(name):\n    return f"""\n\nHello {name}\n\n\n"""`, NLOC is 7
  (master reports 0).
- Given a function whose first statement is a plain `"""docstring"""`, the
  docstring lines remain excluded from NLOC.
- Given `r"""..."""` or `b"""..."""` assigned or returned, the lines count.

**Review state (2026-11-16):** Behavior accepted with maintainer
corrections pushed to the contributor branch (history preserved):
`93e490d8` — a prefix counts only when the tokenizer sees it directly
against the string (`PrefixedStringToken`), fixing a regression where a
variable named `f`/`r`/`b`/`u` before a standalone `"""comment"""` had its
lines counted; `e7bee2db` — drop the redundant whitespace list check that
offset the new per-token tracking cost. Head `e7bee2db`: full pytest and
`make` green locally; hosted CI 16 pass, 1 skipped.

**Performance (Fabio cohort, upper 95% bound strictly < 3%):** evidence in
local `.planning/evidence/pr-568/` (not committed), frozen 306-file Python corpus,
baseline master `00310c5d`. On `e7bee2db`: API 60 pairs passes
(−6.7%…+2.4%); CLI not yet met — 30 pairs −4.9%…+4.3%, 60 pairs
−0.5%…+7.6%, 60 pairs −3.4%…+3.6% (centres −0.3%…+3.5%; host load 3–9 from
concurrent sessions). Deterministic calls +0.03% vs master; `process_token`
stage time at parity after `e7bee2db`.

**Disposition:** Maintainer accepted the measured CLI evidence on
2026-11-16. PR merged guarded on head `e7bee2db` as `fab3406e`, after full
pytest of the head merged with master `29f4f16e` (3164 passed, 7 skipped).
Acknowledgement:
https://github.com/terryyin/lizard/pull/568#issuecomment-6905356808
