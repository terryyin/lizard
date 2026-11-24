# Preserve Ruby modulo alongside percent literals

**Identity:** SEED-075#ruby-percent-literal-context

**Source:** [Preserve Ruby modulo alongside percent literals](../../seeds/SEED-075-reader-corrections.md#ruby-percent-literal-context). The original proposals are [#646](https://github.com/terryyin/lizard/pull/646) at `50e9f92334eede41a4287b8ab12ae90cda23bb27`.

## Goal and scope

Ruby users can count decisions after modulo or modulo assignment while literal punctuation still protects text. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

On #646, `def f(a); a%=2; if a==0 then a else 0 end; end` reports CCN 1. Master reports 2 and Ruby accepts the source. The broad delimiter rule consumes the assignment and part of the condition as a percent literal.

## Current decisions

Use the Ruby token generator and its existing regex/literal context. A character-only delimiter rule cannot settle the operator ambiguity. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Preserve Ruby modulo alongside percent literals

Type: Behavior

Status: planned

Analyze the modulo assignment with and without whitespace, an adjacent modulo expression, and the proposed `%q@…@` and `%q#…#` cases. Require CCN 2 for the real decision and retain the following method and nested paired literals.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
