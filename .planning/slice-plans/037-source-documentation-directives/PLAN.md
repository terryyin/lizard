# Keep Perl and Ruby documentation from acting as Lizard directives

**Identity:** SEED-075#source-documentation-directives

**Source:** [Keep Perl and Ruby documentation from acting as Lizard directives](../../seeds/SEED-075-reader-corrections.md#source-documentation-directives). The original proposals are [#636](https://github.com/terryyin/lizard/pull/636) at `7e89c51a83bdc2254ac832872e01acf5d97c4430`, [#642](https://github.com/terryyin/lizard/pull/642) at `0bf972c66cce3e4b6cb44d8fa450ca2b5a303ddb`.

## Goal and scope

Perl and Ruby users keep correct function lists and source locations when embedded documentation contains text that resembles analyzer directives. Preserve unrelated reader behavior and follow the existing PR's direction.

## Decisive premise

With GENERATED CODE between a Perl =head1 and =cut or a Ruby =begin and =end, the proposed reader stops before the later executable function. The same sources retain that function on master. Both proposals route opaque documentation through the shared comment directive handler.

## Current decisions

Preserve the shared comment counter and explicit distinction between language comments carrying analyzer directives and opaque document or data tokens. Keep the Perl documentation recognition separate from its function state machine when organizing the already oversized reader file. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Keep Perl and Ruby documentation from acting as Lizard directives

Type: Behavior

Status: planned

Analyze POD followed by a subroutine and Ruby embedded documentation followed by a method. Include the literal words GENERATED CODE and a forgiveness example inside each section. The following function retains its line and CCN. Actual language comments with existing Lizard directives still behave as before.

Use `analyze_file.analyze_source_code` and real source fixtures. Compare
master, the exact proposal and the corrected revision. Run the affected
reader and extension tests, then full pytest and make through Nix. Pair
affected API and broad CLI timing on the exact corrected production bytes and
require green hosted checks before any merge.
