# Keep national-character PL/SQL alternative literals opaque

**Identity:** SEED-075#plsql-national-literals

**Source:** [Keep national-character PL/SQL alternative literals opaque](../../seeds/SEED-075-reader-corrections.md#plsql-national-literals). The original proposal is [#639](https://github.com/terryyin/lizard/pull/639) at `887ad0bff86a05c71eca26d9eb6eba6a0b7dde4f`.

## Goal and scope

PL/SQL users get the same CCN and following function locations for `NQ` alternative-quoted literals as for the equivalent `Q` literals. Follow #639's direction: an alternative-quoted literal is one token. Cover an optional `N` prefix in any letter case before #639's `Q` form, with `[]`, `{}`, `()`, `<>` and repeated-character delimiters. Preserve #639's `Q` behavior, ordinary and `N'...'` strings, identifiers named `nq` and unrelated reader behavior. Out of scope: other SEED-075 corrections, non-PL/SQL languages and unterminated alternative-quoted literals.

## Decisive premises

Observed at `bf648012` in this workspace with `nix --extra-experimental-features 'nix-command flakes' develop -c env PYTHONPATH=<checkout> python` calling `analyze_file.analyze_source_code("s.sql", ...)` on `CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN <literal>; END;` then `CREATE PROCEDURE g AS BEGIN NULL; END;`. The #639 rows ran in a scratch detached worktree at `887ad0bf`, removed afterwards. The corrected rows applied #639's diff in this workspace with its pattern prefix changed from `[qQ]'` to `[nN]?[qQ]'`, then reverted both.

| Premise | Consuming operation | Result (master / #639 / corrected) |
| --- | --- | --- |
| The finding reproduces for the national prefix | `nq'[it's if x then end if;]'` | f CCN 2 / 2 / **1**; g at line 2 |
| Other cases and paired delimiters fail the same way | `NQ'{...}'`, `nq'(...)'`, `nq'<...>'` | f CCN 2 / 2 / **1** |
| A repeated-character delimiter loses f | `Nq'#...#'` | no f / no f / **f CCN 1**; g at line 2 |
| #639's `Q` forms keep working | `q'[...]'`, `q'#...#'` | f lost or CCN 2 on master / 1 / 1 |
| Multiline national literal keeps following lines | `nq'[it's` newline `if x then end if;` newline `]'` | f CCN 2 / 2 / **1**; g at line 4 in all |
| `N'...'` strings and identifier `nq` stay unaffected | `N'it''s'` then a real IF; `nq NUMBER` used in `IF nq > 1` | f CCN 2 / 2 / 2 |
| Existing tests stay green with the corrected pattern and #639's tests | `test/test_languages test/test_extensions`; PL/SQL-filtered language tests | 3194 passed, 5 skipped; 45 passed |

The root cause is that #639's alternative-quote pattern starts at `q`, so in `nq'...'` the tokenizer first reads `nq` as an identifier and the quote falls back to a generic string ending at the embedded quote. No premise remains open.

## Current decisions

Extend #639's single alternative-quote pattern with an optional `[nN]` prefix rather than adding a second pattern, so the delimiter table stays in one place. Keep literal token patterns in `generate_tokens`, apart from the oversized PL/SQL declaration and control state machines. Bring in #639's tests rather than a competing set. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

No North Star topic bears on this reader correction beyond that ADR.

## Outside-in proof

| Promise | Owning slice | Proof |
| --- | --- | --- |
| Each `NQ` case and delimiter form gives f CCN 1 with g at line 2 | 1 | `analyze_file.analyze_source_code` on the seed sources |
| Multiline `NQ` literal gives f CCN 1 and g at line 4 | 1 | same entry point |
| #639's `Q` forms keep their results | 1 | #639's `test_lizard_plsql_alternative_quotes.py` cases |
| `N'...'` strings and identifier `nq` open no alternative literal | 1 | same entry point |
| Existing PL/SQL reader tests stay green | 1 | `nix develop -c python -m pytest test/test_languages/testPLSQL.py` and the PL/SQL-filtered language tests |

## Ordered slices

### 1. Keep national-character PL/SQL alternative literals opaque

Type: Behavior

Status: done

Proof: `analyze_file.analyze_source_code` on the key-example sources reports the promised CCN and function start lines. #639's tests and the PL/SQL reader tests stay green. Before merge, run full `nix develop -c python -m pytest` and `nix develop -c make` as `AGENTS.md` requires. Pair affected API and broad CLI timing on the exact corrected production bytes. Require green hosted checks on the published revision.

Behavior: Given PL/SQL source with a `Q` or `NQ` alternative-quoted literal in any letter case, using a paired or repeated-character delimiter, that contains an embedded quote and apparent code, when the reader analyzes it, then only real decisions count and following functions keep their start lines.

Bring in #639's reader change and tests, add the optional national-character prefix, and add the `NQ`, mixed-case, repeated-delimiter and multiline regressions. Compare master, the exact proposal and the corrected revision.

Accepted proof: `test/test_languages/test_lizard_plsql_alternative_quotes.py` (`TestPLSQLAlternativeQuotes`) asserts name, CCN and start line through `analyze_file.analyze_source_code` for #639's five `Q` forms, six `NQ` case/delimiter forms, the multiline `NQ` literal (g at line 4), `N'it''s'` and identifier `nq` (f CCN 2). With `testPLSQL.py`: 49 passed. Language and extension suites: 3198 passed, 5 skipped. Full pytest: 3768 passed, 7 skipped. `make` passed. The master and #639 comparison reuses the settled premise rows. Paired timing on a 4000-function PL/SQL corpus, base versus corrected bytes: API best 1.21–1.54 s versus 1.25–1.44 s, CLI 1.26–2.25 s versus 1.30–1.79 s, all within noise. Refactor pass: documented the alternative-quote form in the `generate_tokens` docstring only. Hosted CI on the published revision remains for the completion boundary.

## Preparation review

Slice-plan refinement: not needed. The plan has one Behavior slice, settled premises and mapped outside-in proof, with no cumulative design or sizing concern.

Remaining slice-specific concerns: none.
