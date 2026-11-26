# Preserve quoted PL/SQL formals and their declarations

**Identity:** SEED-075#plsql-formal-parameter-boundaries

**Source:** [Preserve quoted PL/SQL formals and their declarations](../../seeds/SEED-075-reader-corrections.md#plsql-formal-parameter-boundaries). [Original proposal #644](https://github.com/terryyin/lizard/pull/644) at `bb2bf6b13c0fbbe36772d07ebec341ad19205478`.

## Goal and scope

Report one parameter per PL/SQL formal. Preserve each name in source spelling and expose the full token-spaced declaration and a readable long name. Apply the same boundary to procedures and functions, including nested declarations. Preserve body CCN, declaration locations, and following function recognition. Package specifications, malformed-list recovery, nonstrict Boolean casing, and changes to duplicate-list policy are outside this story.

## Observed baseline

Ran the source examples through `analyze_file.analyze_source_code(sample, source)` under `nix develop -c python` in the preparation checkout at `4c221ed5` and with the exact proposal's production files exported into that workspace under the same interpreter. The analyzer and duplicate-list extension need no other checkout inputs for these cases.

| Consuming operation and input | Trunk `4c221ed5` | Proposal `bb2bf6b1` |
| --- | --- | --- |
| Quoted and plain formals in a procedure | Count 2, names `NUMBER` and `VARCHAR2`. Both declarations contain mode and type. | Count 1, only name `b`. Declaration strings are only `"x name"` and `b`, apart from spacing. |
| Function with `DEFAULT GREATEST(ABS(-1), 2)` followed by `b NUMBER` and a procedure on line 2 | Count 1, name `1`. The declaration is cut off in the inner call. | Count 2, names `a` and `b`. The full declaration text remains only in `long_name`. |
| Nested procedure with quoted punctuation, a `:=` call default, and a trailing OUT formal | Count 2 from a comma inside the call, names `NUMBER` and `2`. The trailing OUT formal is lost. | Count 1, only name `tail`. Neither complete declaration is returned. |
| Empty signature | Count 0, no names, and a stray opening parenthesis in `full_parameters`. | Count 0 with both lists empty. |
| Cross-file duplicate-list extension on three five-formal procedures, two sharing `"x name"` and one using `"y name"` | Occurrence and file counts are `3, 3, 3` because the five extracted names are all `NUMBER`. | Both counts are `0, 0, 0` because only four names reach the extension threshold. |

Both analyzer runs reported CCN 1 for these bodies and placed the nested, enclosing, and following procedures on lines 2–3, 1–4, and 5 respectively. On trunk, `nix develop -c python -m lizard --csv --verbose -t 2 -E duplicated_param_list <fixture-dir>` on the three real SQL files reported `PARAM` 5 and both duplicate counts 3 for each function. This reaches the extension and formatter through batch analysis with worker processes.

## Current decisions

- Reuse the existing PL/SQL tokenizer, state transitions, `FileInfoBuilder`, and `FunctionInfo`. The quoted identifier already reaches the reader as one token. Adopt the proposal's formal-list depth rule. The shared bracket helper uses `br_count` for its nesting, and PL/SQL already uses that state for the enclosing body. Keep formal-list depth local to the signature.
- Have the reader supply the name with each complete declaration. The name is known during parsing and should travel with the declaration through the builder. Preserve the list of declaration strings and the generic name and count inference for readers that supply only declaration tokens, including nameless JavaScript slots. Use [ADR 0001's analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).
- The consumer trace follows `PLSQLReader` registration, `PLSQLStates`, `full_parameters`, and the duplicate-list extension. Slice 1 verifies reader registration and state handoffs through the analyzer. Slice 2 verifies the changed public metrics, signatures, and extension result. The full suite covers existing `FunctionInfo` callers and the shared projections used by CLI warnings, other formatters, and the website. Repeat the trace on the implementation diff, including terms for any moved or changed contract.

## Ordered slices

### 1. Separate PL/SQL declaration states from body tracking

Type: Structure

Status: done

Move PL/SQL declaration recognition from `lizard_languages/plsql.py` (426 lines) into a focused declaration module. The seam runs from identifying top-level and nested procedure, function, and trigger headers through the handoff to body tracking. Keep the reader entry point, token processing, and body state in the original module and preserve their state-machine flow and builder ownership. Keep the resulting edited modules within the [250-line file-size check](../../../.agents/skills/dough-post-change-refactor/references/refactor-checks.md#file-size).

Run the existing PL/SQL reader tests, including `testPLSQL.py` (751 lines, run as-is) and the alternative-quote fixtures. Compare the analyzer observations above before and after extraction, including exact metrics, function order, and declaration outputs. The declaration seam gives the next slice one location for formal-list parsing.

- Delivered proof: extracted `PLSQLDeclarationStates` into `lizard_languages/plsql_declarations.py` (205 lines) and reduced `lizard_languages/plsql.py` to 227 lines; removed dead `in_parameter_list` attribute. Pre/post analyzer baseline identical across 5 cases; `testPLSQL.py` + `test_lizard_plsql_alternative_quotes.py` (49 passed), consumer tests (55 passed), full pytest (3777 passed, 7 skipped).

### 2. Count complete PL/SQL formal declarations

Type: Behavior

Status: done

When a procedure or function header has a formal list, consume each complete formal with one depth rule. Only a comma at the formal-list level starts another declaration. Use the same path for nested procedures and standalone functions. Commit each declaration and its name once, keeping its full type, mode, and default in both public declaration output and the long name.

| Promise | Proof |
| --- | --- |
| Nested defaults, trailing formals, and empty signatures in functions and procedures | Carry forward the proposal's focused fixtures in `test_lizard_plsql_nested_parameters.py`. With `analyze_source_code`, assert counts, names, full declaration strings, CCN, and following function locations. Prove both `DEFAULT` and `:=` and an empty declaration list for `g()`. |
| Quoted identifier punctuation and placement of nested declarations | Add the seed's quoted and nested procedure examples to the focused fixture file. Assert exact names and token-spaced declarations, that the signature renders each formal once in `long_name`, and that the enclosing and following procedures keep their own metrics and source boundaries. |
| CLI and duplicate-list consumer | Analyze three real SQL files using the observed CSV command with two workers. Two files share the five names `"x name"`, `b`, `c`, `d`, and `e`, with different defaults. One uses `"y name"`. Assert `PARAM` 5, the complete long-name tokens with the CSV formatter's quoting, and both duplicate counts of `2, 2, 1` by function name. |

Compare the discriminating proof on trunk, the exact proposal, and the corrected revision. Run the affected language and extension tests and full pytest through Nix under [AGENTS.md](../../../AGENTS.md). Run `nix develop -c make` for the planned format check. Retain the existing PR qualification on the exact corrected production bytes using the [paired runner](../../../.agents/skills/lizard-process-pr/references/performance.md) for PL/SQL `api-parameters` and a broad frozen `cli` corpus. Require green hosted checks on the final implementation head before any merge.

- Delivered proof: added `FunctionInfo.add_parameter_declaration` / `FileInfoBuilder.add_parameter_declaration` (preserving generic `add_parameter` inference for other readers) and signature-local depth tracking in `PLSQLDeclarationStates` (`lizard_core/function_info.py` 172 lines, `lizard_core/file_info_builder.py` 138 lines, `lizard_languages/plsql_declarations.py` 230 lines). Added 5 focused tests in `test/test_languages/test_lizard_plsql_nested_parameters.py` covering nested `DEFAULT` / `:=` calls, empty `g()`, quoted identifiers (`"x name"`, `"a,)"`), nested procedure boundaries, and 3-file CLI `--csv --verbose -t 2 -E duplicated_param_list` (`PARAM` 5, duplicate counts `2, 2, 1`). Verified focused suite (60 passed), full pytest (3782 passed, 7 skipped), `nix develop -c make` (tests + `pep8` + `pylint`, exit 0), and paired performance qualification on exact final production bytes vs base `79b4d85c`: PL/SQL `api-parameters` 95% CI `[-17.95%, -5.41%]` (`passes: true`); broad frozen `cli` (128 files, `--pairs 40` after noisy 20-pair runs under host load >22, prior runs retained) 95% CI `[-5.57%, +1.78%]` (`passes: true`).
- Minor non-blocking follow-up from refactor pass: `lizard_languages/plsql.py` lines 30–32 docstring still describes old parameter-counting behavior.

## Readiness review

Slice-plan refinement separated the reader extraction from the behavior change. Each slice now has its own proof loop and fits the 15-minute active-work budget. One formal-list rule covers the examples. The baseline reaches the analyzer and extension behavior needed for the change. No remaining slice-specific concerns were found.

## Execution complete

Product advice: no change
