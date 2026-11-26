# Resume Kotlin tokenization after each complete block comment

**Identity:** SEED-075#kotlin-adjacent-nested-comments

**Source:** [Resume Kotlin tokenization after each complete block comment](../../seeds/SEED-075-reader-corrections.md#kotlin-adjacent-nested-comments). The original proposals are [#647](https://github.com/terryyin/lizard/pull/647) at `a30a5c63f1e7b2e2b2bdc8cf7dbf282beeceb2f1`.

## Goal and scope

Kotlin users get accurate function metrics when consecutive nested comments contain quotes or apparent executable code. Follow #647's direction: nested block comments are read whole. Resume the trivia scan right after each complete block comment so that quotes, raw-string quotes, character quotes or backticks inside one comment cannot hide the next comment's opener. Preserve #647's nested and literal-marker behavior and unrelated reader behavior. Out of scope: other SEED-075 corrections, non-Kotlin languages, Kotlin string templates, and unterminated block comments.

## Decisive premises

Observed at `6598d758` in this workspace with `nix develop -c python` calling `analyze_file.analyze_source_code`. The #647 rows ran in a scratch detached worktree at `a30a5c63`. The corrected rows used that same tree with `_flatten_nested_comments` replaced by a loop that calls `_KOTLIN_TRIVIA.search(source, start)` from the end of the last consumed piece, not `finditer`. The scratch worktree was removed.

| Premise | Consuming operation | Result (master / #647 / corrected) |
| --- | --- | --- |
| The finding reproduces, and resuming after each comment settles it | Adjacent example: `/* " */`, then `/* outer /* inner */ if (hidden) {} */`, `val s = "x"`, real `if`, then `fun g() {}` | f CCN 3 / 3 / **2**; corrected f NLOC 4, g at line 7, file NLOC 5 |
| A raw-string or character quote in the first comment fails the same way | Same shape with `"""` and a following `"""x"""`, or `'` and a following `'x'` | f CCN 3 / 3 / **2** for both |
| A backtick in the first comment fails only on master | Same shape with a backtick and a following `` val `x` = 1 `` | f CCN 3 / 2 / 2 |
| #647's own nested cases keep working | `/* outer /* inner */ if (fake) {} */` before a real `if` | f CCN 3 / 2 / 2; g keeps line 5 |
| A `/*` inside a string opens no comment | `val s = "/*"` before a nested comment and a real `if` | f CCN 3 / 2 / 2 |

The root cause is the scan's position. `finditer` resumes at the end of the `/*` match, which lies inside the comment just consumed. The `match.start() < start` guard then discards a string match that began inside that comment, together with the next comment's opener it swallowed. No premise remains open.

## Current decisions

Fix #647's `_flatten_nested_comments` scan position rather than replacing its approach. Continue matching from the end of each consumed comment or literal, as Rust's `_flatten_nested_comments` already does with its index loop. Reuse #647's `_KOTLIN_TRIVIA` and `_COMMENT_DELIMITER` patterns. Do not merge the Kotlin and Rust scanners: their literal grammars differ (Rust lifetimes and `r#"` raw strings, Kotlin backtick identifiers and `"""`). Keep the scanner at module level in the Kotlin reader. Use Lizard's established [analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

`kotlin.py` was 271 lines at the Take and 310 with the scanner. Slice 1 follows the existing Perl and Fortran reader pattern by moving `KotlinStates` intact to `kotlin_states.py`. `KotlinReader` and its scanner stay in `kotlin.py`, which imports `KotlinStates` under the existing name. Keep the registration and declaration parsing behavior. Verify the unchanged state machine and its import, then rerun the affected analyzer proof before timing the final production bytes.

No North Star topic bears on this reader correction beyond that ADR.

## Outside-in proof

| Promise | Owning slice | Proof |
| --- | --- | --- |
| Adjacent example gives f CCN 2, NLOC 4, g at line 7, file NLOC 5 | 1 | `analyze_file.analyze_source_code` on the seed source |
| `"""`, `'` and backtick variants give f CCN 2 | 1 | same entry point |
| #647's nested and literal-marker cases keep their results | 1 | #647's `test_lizard_kotlin_nested_comments.py` cases |
| Existing Kotlin reader tests stay green | 1 | `nix develop -c python -m pytest test/test_languages/ -k kotlin` |

## Ordered slices

### 1. Resume Kotlin tokenization after each complete block comment

Type: Behavior

Status: planned

Proof: `analyze_file.analyze_source_code` on the key-example sources reports the promised CCN, NLOC and start lines. #647's tests and the existing Kotlin reader tests stay green. Before merge, run full `nix develop -c python -m pytest` and `nix develop -c make`. Pair affected API and broad CLI timing on the exact corrected production bytes. Require green hosted checks on the published revision.

Behavior: Given Kotlin source with a block comment that contains a quote, raw-string quote, character quote or backtick, followed by a nested block comment with apparent code, when the reader analyzes it, then only the real decision counts and following functions keep their start lines.

Bring in #647's reader change and tests, correct the scan position, and add the adjacent-comment regressions. Compare master, the exact proposal and the corrected revision.

#### Execution evidence

- Story Branch Mode uses `origin/codex/resume-kotlin-tokenization-after-each-complete-b`, with Take `75d72bc218d404fd9d001ef02f43291708ce462e`. GitHub Actions `python-app.yml` is observed by coordinator `nana-chan-de863105`.
- Pinned master `bdafc0a1` reported CCN 3 for all four adjacent-comment examples. Proposal `a30a5c63` reported 3 for the ordinary, raw-string and character quotes and 2 for backticks. The corrected analyzer reported f CCN 2 and NLOC 4, g at line 7 with CCN 1, and file NLOC 5 for all four. The deeper nested and literal-marker controls preserved the proposal's corrected metrics. The CLI and HTTP adapter exposed the same corrected metrics for the ordinary-quote case.
- `KotlinStates` matches the original class body byte for byte and both import paths resolve to the same class. After the move, 82 focused Kotlin tests and 201 affected consumer tests passed. Full pytest passed with 3,766 tests and seven skips, and `nix develop -c make` passed.
- The paired Kotlin API timing used eight fixed sources and 20 alternating pairs against `bdafc0a1`. The change was 16.4% faster on that corpus (95% interval 12.1% to 20.5% faster).
- The paired broad CLI timing used 153 fixed UTF-8 sources and 20 alternating pairs against the same baseline. Four-second calibration produced a valid estimate of 0.8% faster (95% interval 5.7% faster to 4.4% slower). The interval includes no change. The plan sets no numerical acceptance threshold. The earlier CLI attempts stopped on an unsupported binary asset and yielded sub-second samples, respectively.

## Preparation review

Slice-plan refinement: not needed. The plan has one Behavior slice, settled premises and mapped outside-in proof, with no cumulative design or sizing concern.

Remaining slice-specific concerns: none.
