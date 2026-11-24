# Keep national-character PL/SQL alternative literals opaque

**Identity:** SEED-075#plsql-national-literals

**Source:** [Keep national-character PL/SQL alternative literals opaque](../../seeds/SEED-075-reader-corrections.md#plsql-national-literals). The original proposals are [#639](https://github.com/terryyin/lizard/pull/639) at `887ad0bff86a05c71eca26d9eb6eba6a0b7dde4f`.

## Goal and scope

PL/SQL users get the same metrics for Q and NQ literals containing apparent executable code. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

In a function returning `nq'[it's if x then end if;]'`, #639 reports CCN 2. The equivalent Q form passes and both prefixes are valid Oracle syntax. [Language reference](https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/Literals.html).

## Current decisions

Extend the existing alternative-quote rule to cover the optional national-character prefix. Keep literal token patterns distinct from the already oversized PL/SQL declaration and control state machines. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Keep national-character PL/SQL alternative literals opaque

Type: Behavior

Status: planned

Analyze Q, NQ and mixed-case prefixes with paired and single-character delimiters. The literal contents produce no decisions, the enclosing function finishes on its own END, and the next procedure remains visible.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
