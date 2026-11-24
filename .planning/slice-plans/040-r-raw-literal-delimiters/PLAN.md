# Keep valid R raw-string delimiter forms opaque

**Identity:** SEED-075#r-raw-literal-delimiters

**Source:** [Keep valid R raw-string delimiter forms opaque](../../seeds/SEED-075-reader-corrections.md#r-raw-literal-delimiters). The original proposals are [#638](https://github.com/terryyin/lizard/pull/638) at `ad503916d19b37ca0eedf208f35dbbda589dcc71`.

## Goal and scope

R users get correct CCN and following function locations for raw literals with either quote style and supported delimiters. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

A valid single-quoted `r'(some ' if (hidden) {} ' text)'` adds a false decision on master and #638. The R lexer accepts both quote characters. The current R lexer also supports a vertical-bar delimiter and #638 leaves that form unchanged. [Language reference](https://raw.githubusercontent.com/wch/r-source/trunk/src/main/gram.y).

## Current decisions

Use the existing R language reader token patterns and one definition of the delimiter table. Isolate lexical pattern construction from the already oversized R function states. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Keep valid R raw-string delimiter forms opaque

Type: Behavior

Status: planned

Analyze both quote characters with embedded quotes, bracket pairs, matching dashes, and a vertical-bar case supported by the current R grammar. The fake decision stays opaque and the following real decision and function are retained.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
