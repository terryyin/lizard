# Count R variadic and quoted formals with nested defaults

**Identity:** SEED-075#r-formal-parameter-boundaries

**Source:** [Count R variadic and quoted formals with nested defaults](../../seeds/SEED-075-reader-corrections.md#r-formal-parameter-boundaries). Proposal [#643](https://github.com/terryyin/lizard/pull/643) at `ecf44a70dd83a17fed5b47fca9f1fa5b5bc0fb90`.

## Goal and scope

Report one parameter per R formal, including `...` and backtick-quoted names, even around nested defaults. Keep full declarations and readable long names, and preserve body CCN and following function recognition. Raw strings, malformed-input recovery, and changes to other language readers or to the duplicate-list policy are outside this story.

## Observed baseline

The same fixtures ran in separate `git archive` exports inside the preparation workspace with `nix develop -c python` and `analyze_file.analyze_source_code('sample.r', source)`. Each declaration from the story was followed by a body with one `if` and by `g <- function(z) { z }` on line 2.

| Input | Trunk `458b0d40` | Proposal `ecf44a70` |
| --- | --- | --- |
| `a = list(1, 2), b = 3` | Two names, `a` and `2` | Two names, `a` and `b` |
| `a = list(1, 2), ...` | Two names, `a` and `2` | Two declaration strings, but only `a` is counted and named |
| `` `x y` = list(1, 2), b `` | Names `1` and `2` | Only `b` is counted and named |
| `` a = {list(1, 2)[1]}, ..., `x, ) y` = list(1, 2) `` | Stops in the first nested call | Closes at the `)` inside the quoted name, with names `a` and `x` |
| `x, y = 2` and empty formals | Counts and names correct. Ordinary long name repeats tokens. | Counts and names correct. Long names `f( x , y = 2)` and `f()`. |

Both revisions report `f` with CCN 2 and `g` on line 2 with parameter `z` in these fixtures. `nix develop -c python -m pytest -q test/test_languages/test_lizard_r_nested_parameters.py` passed on the proposal. On trunk, `nix develop -c python lizard.py --csv --verbose sample.r` exposes `PARAM`, `long_name`, and `start` for both functions and repeats the tokens in their signatures. Through `analyze_files` with the duplicated-parameter-list extension, three files using `a`, `b`, `c`, `...` and a quoted name reported zero duplicate counts on both revisions. The proposal exposes only three names, below the extension's threshold of five. Two files used the same quoted name with different defaults and the third used a different quoted name.

## Current decisions

- Reuse `RReader`'s token generator, #643's balanced collection of defaults, and the existing `FileInfoBuilder` and `FunctionInfo` path for declaration text and `long_name`. `FunctionInfo` currently infers generic names from declaration strings. The R reader has the lexical information needed to supply the name with each complete declaration, so keep that R-specific decision at the reader boundary and keep one authoritative name per slot. Preserve the fallback for readers that provide only tokens, including JavaScript's nameless formal slots. Scala's backtick function-name state keeps only one word, and Go's backticks delimit raw strings, so neither supplies R's quoted identifier semantics. Use [ADR 0001's analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).
- The consumer trace follows `RStates`, `full_parameters`, `parameter_count`, `long_name`, and `duplicated_param_list` from the reader through `FunctionInfo` to the public analyzer, CLI and formatters, and the extension. The website and other formatters use those same public values. Prove the changed R outputs at the analyzer and CLI, and prove the extension's threshold and distinct-name behavior at `analyze_files`. Keep existing shared API, formatter, website backend, and language-reader assertions in the full pytest run.

## Ordered slices

### 1. Separate R function declaration from body tracking

Type: Structure

Status: done

Move the declaration logic in `lizard_languages/r.py` (293 lines) into a focused R declaration module. The seam includes recovering left-assignment names from recent tokens and consuming the header through the handoff to the body. Keep the `RReader` and `RStates` entry points, body and nested-function tracking, and post-body assignment handling in the existing reader. The global and nested declaration routes use the same declaration logic. Keep both edited modules within the [250-line file-size check](../../../.agents/skills/dough-post-change-refactor/references/refactor-checks.md#file-size).

Run `nix develop -c python -m pytest -q test/test_languages/testR.py` and compare the analyzer observations above against pre-slice trunk. Pay particular attention to top-level, anonymous, nested, chained and right-assigned functions and to the exact metrics on ordinary declarations. Reuse the existing R test file for verification.

- Proof: `lizard_languages/r.py` (187 lines) and `lizard_languages/r_declaration.py` (103 lines) both satisfy the 250-line check. `nix develop -c python -m pytest -q test/test_languages/testR.py test/test_analyzer.py` passed (51 passed), and pre- vs post-slice `analyze_file.analyze_source_code` JSON outputs were identical across 18 fixtures (31 functions) covering top-level, anonymous, nested, chained, right-assigned, single-line, parenthesized, S3 dotted, and baseline declarations. Independent post-change refactor pass returned `none — already clean`.
- CI observation: GitHub Actions workflow `python-app.yml` (`Python application`) on `terryyin/lizard` branch `cursor/count-r-variadic-and-quoted-formals-with-nested`, mailbox `/tmp/dough-ci-501/watch-W9cyYD`.

### 2. Count complete R formal declarations

Type: Behavior

Status: done

When an R header contains nested defaults, only commas at the level of the formal list separate declarations. Backtick-quoted names stay whole, including punctuation inside the quotes. Commit each completed formal once through the builder, with ordered names and one slot per formal. Keep full declaration text and a single rendering of each token in the long name. Continue into the body and the next declaration at the same source positions.

| Promise | Proof |
| --- | --- |
| Ordinary and empty declarations, nested calls, indexing and braces | Carry forward the proposal's `test_lizard_r_nested_parameters.py` fixtures. Assert counts, names, body CCN and the following function through `analyze_source_code`. Verify `f( x , y = 2)` and `f()`. |
| Ellipsis and quoted-name boundaries | Extend the focused R proof with the `...` and quoted-punctuation examples from the story. Assert ordered names, counts and whole declarations in `full_parameters`, with complete defaults and written backticks in `long_name`. Check `g` on line 2. Verify `PARAM` and `long_name` with `nix develop -c python lizard.py --csv --verbose sample.r` on the combined example. |
| Duplicate-list consumer | Analyze three real temporary R files through `analyze_files` and `LizardExtension`. Two share the five names `a`, `b`, `c`, `...`, and `x y` across different defaults. One uses `x z` in the fifth slot. After the cross-file pass, assert duplicate counts and duplicate-file counts of `2, 2, 1`. |

Compare the affected proof on trunk, the exact proposal, and the corrected revision. Run the focused R and duplicated-parameter-list tests, full pytest and `make` through Nix under [AGENTS.md](../../../AGENTS.md). Retain the existing PR qualification: use the [paired runner](../../../.agents/skills/lizard-process-pr/references/performance.md) for the R `api-parameters` workload and a broad frozen `cli` corpus on exact corrected production bytes, then require green hosted checks on the final head before any merge of the implementation.

- Proof:
  - Focused tests: `nix develop -c python -m pytest -q test/test_languages/testR.py test/test_languages/test_lizard_r_nested_parameters.py test/test_extensions/testDuplicatedParamList.py` passed (`40 passed in 0.36s`), covering all #643 fixtures, `f( x , y = 2)`, `f()`, `...`, backtick-quoted names with punctuation/spaces/escapes, `full_parameters`, `long_name`, body CCN 2, `g` on line 2, and real-temp-file `analyze_files` + `LizardExtension` duplicate counts `[2, 2, 1]`.
  - CLI check: `nix develop -c python lizard.py --csv --verbose <sample.r>` on the combined example reported `PARAM=3`, `CCN=2`, full `long_name` with written backticks, and `g` on line 2 (`PARAM=1`).
  - Full suite & style: `nix develop -c python -m pytest` passed (`3776 passed, 7 skipped`) and `nix develop -c make` passed (`pycodestyle` clean, `pylint` 9.35/10). All 6 changed files are <= 250 lines.
  - Independent post-change refactor pass returned `none — already clean`.
  - Paired performance qualification (noisy host, load averages 8.25–25.55): `api-parameters` run 1 (`--seconds 1.2`) `[-4.46%, +4.10%]`, `api-parameters` run 2 (`--seconds 3`) `[-6.43%, +4.61%]`, `cli` (`--seconds 1.2`) `[-3.73%, +4.96%]`. Point estimates are near zero (`-0.18%`, `-0.91%`, `+0.61%`), but host load widened the 95% CIs above the `< 3%` upper bound; a quiet-machine paired rerun is required before merging to `master`.

## Readiness review

Plan refinement was unnecessary after separating the behavior-preserving declaration move from the formal-count correction. Both slices have a bounded proof loop and fit the project sizing rule. The observed failures and extension threshold are covered by the second slice. No remaining slice-specific concerns were found.

## Execution complete

Product advice: No product change. The implementation meets the story's goal, scope, and key examples, with no follow-up correction needed. The paired performance gate was waived by the developer after inconclusive runs on a loaded host (`api-parameters` `[-4.46%, +4.10%]` and `[-6.43%, +4.61%]`; `cli` `[-3.73%, +4.96%]`; point estimates `-0.18%`, `-0.91%`, `+0.61%`). Hosted CI was green on `551eca83`.
