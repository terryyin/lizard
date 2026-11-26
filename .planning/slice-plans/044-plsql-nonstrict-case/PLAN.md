# Keep PL/SQL Boolean operator casing equivalent in nonstrict mode

**Identity:** SEED-075#plsql-nonstrict-case

**Source:** [Keep PL/SQL Boolean operator casing equivalent in nonstrict mode](../../seeds/SEED-075-reader-corrections.md#plsql-nonstrict-case). Proposal [#645](https://github.com/terryyin/lizard/pull/645) at `a86cc28766a7dbc86140715848b546434a8aa040`.

## Goal and scope

PL/SQL condition keywords and Boolean operators count the same in uppercase, lowercase and mixed case, both by default and with the nonstrict extension. Identifiers, labels and the reader's `END IF`, `END LOOP` and `EXIT WHEN` handling are preserved. COBOL's similar uppercase and lowercase condition copies, other readers, and counting `NOT` in nonstrict mode are outside this story.

## Observed baseline

The seed's key-example procedures ran through `analyze_file.analyze_source_code('a.sql', source)` and through `FileAnalyzer(get_extensions([lizardnonstrict.LizardExtension()]))`. They ran under this workspace's `nix develop -c python -I` against `git archive` exports of trunk `0f289ab6` and the proposal `a86cc287`, on 2026-11-26. Values are default CCN / nonstrict CCN.

| Input | Trunk `0f289ab6` | Proposal `a86cc287` |
| --- | --- | --- |
| `If_And_Ready` with `LOOP`, `EXIT WHEN`, `IF … AND`, `ELSIF … OR`, `WHILE … AND NOT`, uppercase | 8 / 8 | 8 / 8 |
| Same, lowercase | 8 / 5 | 8 / 5 |
| Same, capitalized keywords | 2 / 2 | 8 / 5 |
| `FOR … LOOP` and `CASE` with two `WHEN`, uppercase / lowercase / capitalized | 4 / 4 each, except capitalized 1 / 1 | 4 / 4 in every casing |

Every run reported the function name `If_And_Ready`. The uppercase nonstrict result stays 8 because `PLSQLReader.__init__` adds uppercase copies of every condition, while `lizardnonstrict` removes only `reader.logical_operators`, which holds the lowercase `and` and `or`. #645 lowercases a token in `preprocess` only while its lowercase form is still in `reader.conditions`. Once nonstrict removes `and`, `AND` stays uppercase and matches the retained uppercase copy.

## Current decisions

- Keyword case belongs to PL/SQL token preprocessing. Normalize condition keywords from the reader's category sets, not from the current `reader.conditions` that extensions change. Then drop the uppercase copies so nonstrict's removal of `logical_operators` applies to every casing. Normalize only whole keyword tokens, leaving identifiers such as `If_And_Ready`, quoted identifiers, labels, and declaration and long-name text in source spelling. Reuse the state machine's existing case-insensitive handling of `LOOP`, `END` and declaration keywords.
- The consumer trace follows `PLSQLReader.preprocess` and `conditions` through `condition_counter` and `lizardnonstrict` to the function's CCN. Prove the result through `analyze_source_code` with and without the nonstrict extension, and through the CLI with `-Enonstrict`. The full suite covers the existing PL/SQL tests and other readers' condition categories.
- `lizard_languages/plsql.py` has 426 lines. The concurrent story [SEED-075#plsql-formal-parameter-boundaries](../043-plsql-formal-parameter-boundaries/PLAN.md) plans to move its declaration states into a focused module. The two stories are independent, so neither waits for the other.
- Use [ADR 0001's analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Separate PL/SQL keyword preprocessing from the reader

Type: Structure

Status: planned

Move `PLSQLReader.preprocess`'s compound `END` and `EXIT WHEN` handling, together with the case-insensitive condition setup in `__init__`, into a focused PL/SQL keyword module. Keep the reader as the entry point and leave the state machine's flow and builder ownership unchanged. Rebase on trunk first. Bring every edited PL/SQL module within the [250-line file-size check](../../../.agents/skills/dough-post-change-refactor/references/refactor-checks.md#file-size). If the sibling's declaration-state module is on trunk, keep its seam. Otherwise make the split along that same declaration seam, so the two stories reconcile mechanically.

Run `nix develop -c python -m pytest -q test/test_languages/testPLSQL.py test/test_languages/test_lizard_plsql_alternative_quotes.py`. Rerun the baseline probes and require the trunk values above. This slice gives the next slice one location for keyword casing.

### 2. Count PL/SQL keywords the same in every casing

Type: Behavior

Status: planned

Normalize whole condition keyword tokens before condition counting, and drop the uppercase condition copies. The seed's procedures then report the same CCN in every casing, with Boolean operators removed by nonstrict.

| Promise | Proof |
| --- | --- |
| Casing equivalence by default and with nonstrict | Carry forward #645's `test_lizard_plsql_mixed_case.py`. Add the seed's `If_And_Ready` procedure in all three casings and assert CCN 8 by default and 5 with the nonstrict extension through `analyze_source_code`. Assert the function name `If_And_Ready`. Add the `FOR` and `CASE` procedure with CCN 4 in every casing and mode. |
| Compound keywords and identifiers | The same fixture's `EXIT WHEN`, `END IF`, labelled `END LOOP` and `WHILE … LOOP` stay uncounted beyond the values above. The parameter names `p_And` and `y` keep their spelling in the declaration output. |
| CLI | Run `nix develop -c python lizard.py -Enonstrict --csv` on a real SQL file holding the three casings. Each function reports CCN 5. |

The casing proof fails on trunk for the uppercase nonstrict and capitalized cases, and on #645 for the uppercase nonstrict case. Compare the proof on trunk, the exact proposal and the corrected revision. Run the affected PL/SQL and nonstrict tests, then full pytest and `nix develop -c make` under [AGENTS.md](../../../AGENTS.md). Retain the PR qualification on the exact corrected production bytes: use the [paired runner](../../../.agents/skills/lizard-process-pr/references/performance.md) for an affected PL/SQL API workload and a broad frozen `cli` corpus. Require green hosted checks on the final head before any merge.

## Readiness review

Plan refinement was not needed. The behavior-preserving move is separate from the casing correction, and each slice has a bounded proof loop within the project sizing rule. The observed failures on trunk and #645 settle the decisive premise and show the proof discriminates. No remaining slice-specific concerns were found.
