# Report wrapped type arguments on one line

**Identity:** SEED-078#multiline-token-long-names

**Source:** [Keep newlines out of long names and NLOC for multi-line type tokens](../../seeds/SEED-078-pr-batch-follow-ups.md#multiline-token-long-names)

## Goal and scope

Readers of Lizard results get a readable signature and accurate function and file NLOC for the same declaration, whether its grouped type argument is on one line or wraps. The physical line range still locates the declaration. The correction applies wherever the existing shared tokenizer supplies the grouped type token. Java wildcard and C# and Kotlin nullable type arguments provide representative proof. String and comment handling and new type grammar are outside the correction.

## Current decisions

- Reuse the existing type-argument recognition in `lizard_languages/token_pattern.py`. Java and C# reach it through `clike_tokens`, while Kotlin reaches it through `CodeReader.generate_tokens`. Retain the existing raw tokenization and use its type rule on multiline candidates so the decision applies to the same type syntax everywhere.
- Reuse `line_counter` in `lizard_core/file_analyzer.py`. It has the raw token, physical line position and function and file counters when the analysis needs both forms of the token. Its accounting uses the raw LF count for physical position and omits whitespace-only interior segments from NLOC. It then yields a single-line type token to the parser. Wrap whitespace folds to a space between type fragments and drops immediately inside angle brackets, as in #656's Java examples. Single-line tokens bypass this work. Retaining atomicity protects CCN and token count. Supplying the same token spelling as the flattened declaration also restores Kotlin's existing parameter extraction.
- [ADR 0001, Ubiquitous language for Lizard domain concepts](../../../docs/adrs/0001-ubiquitous-language-accepted.md) governs metric and pipeline terminology. A whitespace-only line between type fragments supplies no code. Raw string payloads retain their existing accounting and token text. The design uses existing tokenizer and analyzer responsibilities.
- Take the Java acceptance cases from [PR #656](https://github.com/terryyin/lizard/pull/656/files) and use the shared correction as its replacement. Settle the PR disposition once the verified replacement is on trunk under the applicable delivery instruction.
- Run the focused regression file during the slice and the full suite before completing the change, per [AGENTS.md](../../../AGENTS.md). The shared processor reaches all reader and formatter consumers.

## Execution context

- Story Branch Mode in `.worktrees/keep-newlines-out-of-long-names-and-nloc-for-mul` on `codex/keep-newlines-out-of-long-names-and-nloc-for-mul`, with `origin/master` as the integration target. The established Take is `70c09234a41a6821c6d3d70f3d9a4347ff946f65` on both remote branches.
- Codex observes the GitHub Actions `python-app.yml` workflow on the execution branch. The coordinator is `kirara-078` and its stream is `/tmp/dough-ci-501/watch-uuBSaI` (PID `78389`, shell session `76259`, cell `g646:6`).

## Decisive premises

These observations used the owned preparation checkout at `7d0711a8`. Baseline fixtures and the disposable scratch edits were removed after the run.

| Premise and operation | Observation and result |
| --- | --- |
| The three language forms reach the same grouped-token case through the public analyzer. | `nix develop -c python -` called `analyze_file.analyze_source_code` with the seed declarations in a class, starting at line 2, followed by a one-line `next` method. For each language, the gap after the comma was a space, LF plus four spaces, LF plus space and tab plus LF and four spaces, then its CRLF equivalent. Function NLOC was 1/2/3/3 and file NLOC 4/5/6/6. Each wrapped name contained the raw newline. The blank variants ended on line 4, and `next` started on line 5. Kotlin parameter count fell from 1 to 0 with wrapping. Java and C# kept one parameter. |
| Shared recognition and line accounting can correct both observations without changing physical positions. | In the same Nix profile a scratch full match of the existing type rule on multiline angle-bracket tokens, applied in `line_counter`, folded the type token and omitted whitespace-only interior lines from NLOC after measuring the raw LF count. The same analyzer inputs returned flat-equivalent names, function NLOC 1/2/2/2, file NLOC 4/5/5/5 and parameter count 1 throughout. CCN, token counts and all physical positions agreed with baseline. |
| PR #656's Java fixtures exercise wrapping at the angle brackets as well as blank-line accounting. | The analyzer returned function/file NLOC 6/9 before and 5/8 after the scratch edit for the PR description's `GroupedValues.count`, with end line 9 in both runs. The scratch result matched the expected single-line name. The `Items.visit` fixture with breaks immediately inside both brackets changed from a multiline name to `Items::visit( List<T> items , Consumer <? super T> consumer)`, with NLOC 5, CCN 1 and two parameters in both runs. |
| The PR's optional processor pipeline and Java parameter observations are reachable. | After restoration, `nix develop -c python -` ran `FileAnalyzer` with `get_extensions([])` filtered to omit `line_counter`. `GroupedValues.count` analyzed successfully and file NLOC was 0. The default analyzer returned parameter names `values` and `items, consumer` for the two PR fixtures. |
| The same fix reaches CLI output and preserves unaffected consumers. | Under `nix develop -c python -`, the scratch harness invoked `python lizard.py --csv <blank/Example.java>` and parsed records with `csv.reader`. It observed a single-line long name and NLOC 2, versus the baseline multiline field and NLOC 3. Both runs gave range 2–4 and the following function at line 5. The harness also analyzed a Java text block, C# verbatim string and Kotlin raw string, each containing a blank payload line. Their function/file counts, token counts and ranges matched baseline. The scratch harness ran `python -m pytest -q` within the same `nix develop` environment. 4,000 passed and 7 skipped. |

## Ordered slices

### 1. Wrapped type argument reports retain the source range

Type: Behavior

Status: done

Behavior: Analyze a signature containing a wrapped wildcard or nullable type argument. The result displays the same long name and parameter and token counts as the flattened declaration, with no extra CCN. Function and file NLOC count the code lines inside the token. The function ranges and subsequent locations still use physical lines.

Use one shared newline interpretation at the tokenizer and line-counter boundary. The proof cases exercise the two readings of the same token together: the raw input for line accounting and the folded token for signature and parameter parsing. A Java-only rewrite would duplicate the rule and miss the other reader paths. A broad rewrite of all multiline tokens would reach literal payloads. The identified files, `token_pattern.py` and `file_analyzer.py`, are 132 and 128 lines. Keep the new focused regression file below the [project file-size limit](../../../.agents/skills/dough-post-change-refactor/references/refactor-checks.md#file-size).

Add outside-in tests in `test/test_languages/test_wrapped_type_tokens.py`. Use source text and the normal analyzer by extension, with one temporary source file for CSV analysis through the CLI. The cases use production readers and processors.

| Promise | Observable proof owned by this slice |
| --- | --- |
| Flat, wrapped and blank-line variants in Java, C# and Kotlin | Compare `long_name`, token count, parameter count and CCN to their single-line declaration. Assert function and file NLOC and both functions' start/end positions. Include spaces and tabs on the blank line and CRLF at the source-code API boundary. |
| Java contributor cases | Assert the exact long names and metrics of `GroupedValues.count` and `Items.visit`. Preserve the physical locations and the expected Java parameter names. Exercise `GroupedValues.count` with the standard processors minus `line_counter` to show the optional pipeline still leaves file NLOC at zero. |
| Serialized name and NLOC | Parse `python lizard.py --csv <source>` as CSV and check both function records, the full displayed names, NLOC and physical ranges. |
| Literal and other reader behavior | Assert the original metrics and locations on representative multiline literals with a blank payload line. Run the existing full suite for remaining tokenizer, reader, analyzer, CLI and formatter consumers. |

Run `nix develop -c python -m pytest test/test_languages/test_wrapped_type_tokens.py` and `nix develop -c python -m pytest`. Use the unfixed analyzer first to confirm the new regressions distinguish both reported failures.

**Accepted proof**

The unmodified analyzer failed 21 of the 43 focused cases. Those failures captured multiline names, inflated NLOC on blank continuation lines, Kotlin's dropped parameter, and incorrect CSV cells. The other 22 cases established controls, including the physical ranges and literal metrics. All 43 passed after the fix and again after independent refactoring. The refactor reused the same nonblank LF-delimited segments for spelling and NLOC. The physical line counter still uses the raw LF count, and token generation still uses the existing grammar.

The full suite passed with 4,043 tests and 7 skips. `nix develop -c make pep8` passed. The worktree's `open-dough/evidence` Git directory holds the baseline, focused, refactor and full-suite logs and the consumer trace.

## Concern evidence

Slice-plan refinement was unnecessary. One behavior slice owns both observations of the same wrapped token, with bounded edits in two existing modules and one outside-in fixture family. The scratch run reached the planned consumer routes and the existing full suite. No remaining slice-specific concern was identified.

## Execution complete

Product advice: No change to the backlog or near-future direction. The delivered changes in `60a63ca6` satisfy the story's shared analyzer and CSV promises. The CLI case uses a real source file to check the serialized name and ranges. The analyzer cases cover the detailed metrics and parser behavior. Once trunk CI verifies the shared replacement, close PR #656 as superseded.
