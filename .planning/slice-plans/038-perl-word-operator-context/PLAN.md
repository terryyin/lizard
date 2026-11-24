# Count Perl word operators only in operator positions

**Identity:** SEED-075#perl-word-operator-context

**Source:** [Count Perl word operators only in operator positions](../../seeds/SEED-075-reader-corrections.md#perl-word-operator-context). The original proposals are [#634](https://github.com/terryyin/lizard/pull/634) at `3ab95dca71d145de86dd79ba264644c096a926cf`.

## Goal and scope

Perl users get the extra CCN for short-circuit logic while calls named `and` or `or` keep their original CCN. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

The valid subroutine `sub f { return $obj->and(); }` changes from CCN 1 to 2 on #634. Both real word operators and an actual method name reach the same condition set.

## Current decisions

Use the shared condition counter's member-name convention and the Perl token generator's operator tokens. Keep lexical context out of the Perl function state machine and split the already oversized reader along that seam. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Count Perl word operators only in operator positions

Type: Behavior

Status: planned

Analyze the method-call case and a namespaced call, real word operators, symbol-form operators and quoted words. Confirm nonstrict mode removes the extra operator decision and leaves member names alone.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
