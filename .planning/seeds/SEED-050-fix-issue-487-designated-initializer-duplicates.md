---
id: SEED-050
status: active
planted: 2026-11-07
planted_during: lizard process issue
scope: designated C initializer tables false-positive duplicates
---

# SEED-050: Fix issue #487 designated initializer duplicate false positives

<a id="fix-issue-487-designated-initializer-duplicates"></a>

### Fix issue #487 — designated C initializer tables should not report as duplicate code

**Identity:** SEED-050#fix-issue-487-designated-initializer-duplicates
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"53ca4848ff57ede8034aa427fe061df4fa9c427721d81e3cf209a95ff431505a"}}
```

**Goal:** Users who analyze C sources with designated-initializer lookup tables
through Lizard's duplicate detector, and maintainers closing
[issue #487](https://github.com/terryyin/lizard/issues/487), get one complete
repair: positional and designated forms of the same table agree (neither form
reports consecutive rows as duplicate code when the values differ), while
genuine duplicated code elsewhere still matches.

**Source:** Dashboard instruction `lizard process issue`
https://github.com/terryyin/lizard/issues/487. Contributor diagnosis is on the
issue; no contributor PR is expected. Bounded planless repair under
`lizard-process-issue` / `dough-bug-fixing`, then qualify the full repair with
`lizard-process-pr --local-change` before landing or wrap-up.

**Beneficiary:** C/C++ users whose designated-initializer tables are false
positives today, and maintainers who want designated and positional tables to
agree.

**Scope:** Duplicate-token detection for C designated initializers only.
Reproduce the reported `noah` / `titanic` mismatch and the controlled numeric
designated-versus-positional pair; make the smallest change so both forms stay
clean when values differ; keep ordinary renamed-copy matching. Qualify the
aggregate repair before delivery. Deferred: other languages' initializer
syntax, wholesale detector redesign, and unrelated duplicate thresholds.

**Key examples:**

- Given the issue's enum `noah` (designated) and `titanic` (positional) tables
  with distinct row values, `-Eduplicate` reports no duplicate blocks for either
  table.
- Given a numeric designated four-field table with distinct values per row, and
  a positional wide table of comparable token density, neither reports
  consecutive rows as duplicates.
- Given two genuinely duplicated non-initializer code blocks that already match
  today, they still match after the repair.

**Starting revision:** Established session
`caee1cbf243eef4a50c207ceaf9f9634f86e0a18` on
`cursor/lizard-process-issue-https-github-com-terryyin-l-2`.
