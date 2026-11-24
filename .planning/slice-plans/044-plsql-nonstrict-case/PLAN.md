# Keep PL/SQL Boolean operator casing equivalent in nonstrict mode

**Identity:** SEED-075#plsql-nonstrict-case

**Source:** [Keep PL/SQL Boolean operator casing equivalent in nonstrict mode](../../seeds/SEED-075-reader-corrections.md#plsql-nonstrict-case). The original proposals are [#645](https://github.com/terryyin/lizard/pull/645) at `a86cc28766a7dbc86140715848b546434a8aa040`.

## Goal and scope

PL/SQL users get the same CCN for uppercase, lowercase and mixed-case control flow in default and nonstrict modes. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

On #645, otherwise identical IF x AND y and If x And y statements score 3 and 2 with the nonstrict extension. The reader's uppercase copies escape removal of the logical-operator category.

## Current decisions

Use the one language reader condition category consumed by nonstrict. Separate statement preprocessing from the oversized PL/SQL function state machine if the correction changes those boundaries. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Keep PL/SQL Boolean operator casing equivalent in nonstrict mode

Type: Behavior

Status: planned

Analyze the same procedure in all three casings with and without nonstrict. Require CCN 3 by default and 2 with nonstrict. Preserve labels, user identifiers, and the reader's handling of combined END and EXIT WHEN tokens.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
