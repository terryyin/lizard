# Count Perl word operators only in operator positions

**Identity:** SEED-075#perl-word-operator-context

**Source:** [Count Perl word operators only in operator positions](../../seeds/SEED-075-reader-corrections.md#perl-word-operator-context). The original proposals are [#634](https://github.com/terryyin/lizard/pull/634) at `3ab95dca71d145de86dd79ba264644c096a926cf`.

## Goal and scope

Perl users get the extra CCN for short-circuit `and` / `or` while calls named `and` or `or` keep their original CCN. Count those word forms as logical operators when used as operators; do not count them after `->` or `::`. Preserve `&&` / `||`, quoted text, nonstrict removal of logical-operator decisions, and unrelated reader behavior. Follow #634's direction for which word forms to add. Out of scope: other SEED-075 corrections, non-Perl languages, and Perl's `not` / `xor`.

## Decisive premises

Observed at `2d196509` in this workspace with `nix develop -c python` calling `analyze_file.analyze_source_code` (and `FileAnalyzer` with the nonstrict extension for those rows). The #634 rows scratch-apply only the proposal's `_logical_operators` change; the corrected rows also wrap `and` / `or` after `->` / `::` as `MemberName`. Scratch edits were reverted.

| Premise | Consuming operation | Result |
| --- | --- | --- |
| Master leaves word-form operators uncounted | `analyze_source_code` on `sub check { return $a and $b; }`, `$a or $b`, and `$a and $b or $c` | CCN 1, 1, and 1 |
| Master keeps method and package-qualified names at CCN 1 | Same entry on `sub f { return $obj->and(); }` and `Package::and()` | CCN 1 for both |
| #634 counts word operators and falsely counts member names | Same sources after adding `and` / `or` to `_logical_operators` only | operators CCN 2 / 2 / 3; `$obj->and()` and `Package::and()` CCN 2 |
| Quoted words and symbol forms stay as today on master and #634 | `"and or"`; `$a && $b`; `$a \|\| $b` | quoted CCN 1; symbols CCN 2 on both |
| Shared `mark_member_names` does not mark logical-operator words | `mark_member_names` on `$obj->and()` tokens with `name_before` `{->, ::}` | `and` is not `MemberName` (`and` is neither structure, control-flow, nor case) |
| `MemberName` after `->` / `::` plus the shared condition skip settles the finding | Same operator and member sources with `and` / `or` in `_logical_operators` and those words wrapped as `MemberName` after `->` / `::` | operators CCN 2 / 2 / 3; method and `Package::and()` CCN 1 |
| Nonstrict removes logical-operator decisions and leaves member names alone | `FileAnalyzer` with nonstrict on `$a and $b` and `$obj->and()` under the corrected simulation | both CCN 1 |

No premise remains open.

## Current decisions

Add `and` and `or` to Perl `_logical_operators`, following #634. Reuse `MemberName` and the shared condition counter's skip for member names (`file_analyzer` already ignores `is_member_name`). Do not rely on `mark_member_names` as written: it marks only structure, control-flow, and case keywords, so it leaves word-form logical operators after `->` / `::` countable. Mark those words as `MemberName` in Perl preprocessing (the same seam PHP uses for member access), either by extending the shared helper to include logical operators or by a Perl-local wrap that still uses `MemberName`. Keep that lexical context out of the Perl function state machine. No reader-file split is required for this correction. Use Lizard's established [analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md) (CCN, language reader, token).

No North Star topic bears on this reader correction beyond that ADR.

Chose the shared `mark_member_names` extension (include `logical_operators`) over a Perl-only wrap. Keep `code_reader.py` without a split for this correction: the one-line keyword-set change stays inside that helper's existing responsibility. Plan-settled no reader-file split for `perl.py`; keep `TestPerlWordOperators` in `testPerl.py`.

## Outside-in proof

| Promise | Owning slice | Proof |
| --- | --- | --- |
| `$obj->and()` stays CCN 1 | 1 | `analyze_file.analyze_source_code` on the finding source |
| `$a and $b`, `$a or $b`, and `$a and $b or $c` yield CCN 2, 2, and 3 | 1 | same entry point |
| `Package::and()` stays CCN 1 | 1 | same entry point |
| `"and or"` stays CCN 1; `&&` / `\|\|` stay CCN 2 | 1 | same entry point |
| Nonstrict drops the word-operator decision and leaves a method named `and` at CCN 1 | 1 | `FileAnalyzer` with the nonstrict extension on those sources |
| Existing Perl reader tests stay green | 1 | `nix develop -c python -m pytest test/test_languages/testPerl.py test/test_languages/test_perl_func_names.py` |

## Ordered slices

### 1. Count Perl word operators only in operator positions

Type: Behavior

Status: done

Proof: `analyze_file.analyze_source_code` on the key-example sources reports the promised CCN values; nonstrict checks use `FileAnalyzer` with that extension; focused Perl reader tests stay green. Before merge, run full `nix develop -c python -m pytest` and `nix develop -c make`, and require green hosted checks on the published revision.

Behavior: Given Perl sources that use word-form `and` / `or` as operators, as method or package-qualified names, inside quotes, or alongside symbol-form `&&` / `\|\|`, when the corrected reader analyzes them (default and nonstrict), then only real operator uses add CCN and member names do not.

Bring in #634's word-operator regressions and add the member-name / package-qualified / nonstrict cases. Compare master, the exact proposal, and the corrected revision.

Accepted proof (coordinator): `TestPerlWordOperators` via `analyze_file.analyze_source_code` / `FileAnalyzer`+nonstrict covers promises 1–5; focused `testPerl.py` + `test_perl_func_names.py` 46 passed; PHP/Swift member-name + nonstrict consumers 17 passed after shared helper change. Evidence: `open-dough/evidence/SEED-075-perl-word-operator-context-slice1-report.md`. Refactor: none — already clean (`## REFACTOR COMPLETE`). Full `pytest` and `make` green after repairing flaky Svelte absolute-1s linear-time bounds (contention under full-suite load; unified `_LINEAR_TIME_SECONDS = 2` in `testSvelte.py` — attribution unrelated to Perl reader; failure policy required repair).

## Preparation review

Slice-plan refinement: not needed — one Behavior slice, settled premises, mapped outside-in proof, no cumulative design or sizing concern.

Remaining slice-specific concerns: none.

## Execution complete

Product advice: Key-example scope (`->` / `::`) is delivered; wrap-up may accept that scope or queue a separate story for other non-operator `and`/`or` forms (hash keys, `=>`, `&name()`, `qw`) that now add CCN. Optional follow-up: replace Svelte absolute linear-time budgets with relative scaling. Process review deferred to wrap-up (intent-coverage likely). Hosted CI on `cb1ffa22` was pending at review.
