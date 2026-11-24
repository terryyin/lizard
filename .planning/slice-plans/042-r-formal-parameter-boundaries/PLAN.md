# Count R variadic and quoted formals with nested defaults

**Identity:** SEED-075#r-formal-parameter-boundaries

**Source:** [Count R variadic and quoted formals with nested defaults](../../seeds/SEED-075-reader-corrections.md#r-formal-parameter-boundaries). The original proposals are [#643](https://github.com/terryyin/lizard/pull/643) at `ecf44a70dd83a17fed5b47fca9f1fa5b5bc0fb90`.

## Goal and scope

R users get the number of formal parameter slots and readable names when defaults contain nested expressions. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

For `function(a = list(1, 2), ...)`, #643 reports one parameter. The public full-parameter strings contain two slots but the common name extraction ignores the ellipsis. A quoted formal paired with a nested default is also omitted by #643.

## Current decisions

Assess existing formal-slot and name extraction in FunctionInfo before changing a reader-specific representation. Separate R declaration and body state responsibilities before changing the oversized reader. Let the public analyzer proof determine whether a shared extraction change is needed. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Count R variadic and quoted formals with nested defaults

Type: Behavior

Status: planned

Analyze nested defaults followed by a plain name, an ellipsis and a backtick-quoted formal. Require the intended count and names, predictable long names and preservation of the next function. Check an ordinary declaration and the duplicated-parameter-list extension.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
