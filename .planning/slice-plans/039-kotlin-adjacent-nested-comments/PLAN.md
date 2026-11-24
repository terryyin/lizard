# Resume Kotlin tokenization after each complete block comment

**Identity:** SEED-075#kotlin-adjacent-nested-comments

**Source:** [Resume Kotlin tokenization after each complete block comment](../../seeds/SEED-075-reader-corrections.md#kotlin-adjacent-nested-comments). The original proposals are [#647](https://github.com/terryyin/lizard/pull/647) at `a30a5c63f1e7b2e2b2bdc8cf7dbf282beeceb2f1`.

## Goal and scope

Kotlin users get accurate function metrics when consecutive nested comments contain quotes or apparent executable code. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

Put `/* " */` before `/* outer /* inner */ if (hidden) {} */` followed by `val s = "x"` and one real if. #647 still reports CCN 3 instead of 2. A trivia match that starts inside an already consumed comment can skip the following nested comment.

## Current decisions

Reuse the existing Kotlin lexical patterns where both tokenization and a nested-comment scanner need identical literal boundaries. Keep the scanner outside the already oversized function-state module and inspect the Rust nested-comment logic before introducing another shared solution. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Resume Kotlin tokenization after each complete block comment

Type: Behavior

Status: planned

Analyze the adjacent-comment input and the original nested cases. Assert f has CCN 2, expected NLOC, and a following function retains its start line. Include markers inside ordinary strings, raw strings and backtick identifiers.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
