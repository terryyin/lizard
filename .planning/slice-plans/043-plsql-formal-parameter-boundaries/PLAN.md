# Preserve quoted PL/SQL formals and their declarations

**Identity:** SEED-075#plsql-formal-parameter-boundaries

**Source:** [Preserve quoted PL/SQL formals and their declarations](../../seeds/SEED-075-reader-corrections.md#plsql-formal-parameter-boundaries). The original proposals are [#644](https://github.com/terryyin/lizard/pull/644) at `bb2bf6b13c0fbbe36772d07ebec341ad19205478`.

## Goal and scope

PL/SQL users get correct formal parameter counts around nested defaults without losing quoted names or the full parameter declaration API. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

For `f("x name" IN NUMBER, b OUT VARCHAR2)`, master counts 2 and #644 counts 1. #644 also strips the type and mode from full_parameters on ordinary typed parameters.

## Current decisions

Use the existing FunctionInfo representation for slots and full declarations. Refactor the PL/SQL declaration parsing out of its oversized reader along the boundary needed for this change. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Preserve quoted PL/SQL formals and their declarations

Type: Behavior

Status: planned

Analyze quoted and plain names, nested defaults, nested procedure declarations and an empty signature. Assert parameter count, names, full declaration strings, and the next procedure. Verify the duplicated-parameter-list consumer.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
