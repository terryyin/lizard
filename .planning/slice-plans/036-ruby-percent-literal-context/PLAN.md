# Preserve Ruby modulo alongside percent literals

**Identity:** SEED-075#ruby-percent-literal-context

**Source:** [Preserve Ruby modulo alongside percent literals](../../seeds/SEED-075-reader-corrections.md#ruby-percent-literal-context). The original proposal is [#646](https://github.com/terryyin/lizard/pull/646) at `50e9f92334eede41a4287b8ab12ae90cda23bb27`.

## Goal and scope

Ruby users can count decisions after modulo or modulo assignment while literal punctuation still protects text. Correct the Ruby percent-literal reader so punctuation delimiters stay opaque without treating operator `%` or `%=` as literals. Follow #646's direction. Preserve paired-delimiter nesting and unrelated reader behavior. Out of scope: other SEED-075 corrections and non-Ruby languages.

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| On #646, tight modulo assignment hides the following `if` | `analyze_file.analyze_source_code('t.rb', 'def f(a); a%=2; if a==0 then a else 0 end; end')` after scratch-applying `50e9f923` `lizard_languages/ruby.py` in this workspace; reverted | 2026-11-25, workspace then at `52ba5f75` | CCN 1 |
| Master keeps that `if` | Same command on current `lizard_languages/ruby.py` | 2026-11-25, `52ba5f75` | CCN 2 |
| Master leaves `%q@…@` content visible as decisions | `analyze_file.analyze_source_code` on `def f; value = %q@ if fake end @; if real then 1 end; end` plus `def g` | 2026-11-25, `52ba5f75` | `f` CCN 3, `g` retained |
| Master treats `%q#…#` as a comment and drops the enclosing method | Same shape with `#` delimiters | 2026-11-25, `52ba5f75` | only `g` reported |
| #646 keeps both punctuation forms opaque and retains the following method | Same two sources after the `50e9f923` scratch; reverted | 2026-11-25, `52ba5f75` | `f` CCN 2, `g` retained for `@` and `#` |

## Current decisions

Reuse the Ruby token generator and `_PERCENT_LITERAL` / `_any_delimited` in `lizard_languages/ruby.py`. Broaden punctuation delimiters in the direction of #646, but only when `%` begins a percent literal—not when it is modulo or modulo assignment. A character-only delimiter rule cannot settle that operator ambiguity. Keep existing paired-delimiter nesting. Use Lizard's established [analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md) (CCN, language reader, token).

No North Star topic bears on this reader correction beyond that ADR.

## Outside-in proof

| Promise | Owning slice | Proof |
| --- | --- | --- |
| Tight `%=` before a real `if` yields CCN 2 | 1 | `analyze_file.analyze_source_code` on the finding source |
| Spaced `%=` and `value % divisor` before a real `if` yield CCN 2 | 1 | same entry point |
| `%q@…@` and `%q#…#` with a fake `if` inside and a real `if` after yield CCN 2 and retain a following method | 1 | same entry point |
| Nested paired `%q{…{…}…}` stays opaque | 1 | same entry point |
| Existing percent-literal and modulo token cases stay green | 1 | `nix develop -c python -m pytest test/test_languages/testRuby.py` |

## Ordered slices

### 1. Preserve Ruby modulo alongside percent literals

Type: Behavior

Status: done

Proof: `analyze_file.analyze_source_code` on the key-example sources reports the promised CCN and following-method retention; `nix develop -c python -m pytest test/test_languages/testRuby.py` stays green. Before merge, run full `nix develop -c python -m pytest` and `nix develop -c make`, and require green hosted checks on the published revision.

Behavior: Given Ruby sources that mix modulo / `%=` with a real decision, and percent literals delimited by `@`, `#`, or nested paired braces that contain apparent keywords, when the corrected reader analyzes them, then each real decision counts in CCN, literal text does not, and a following method remains in the function list.

## Execution evidence

Slice 1, 2026-11-26, published base `98908ecd`, trunk `df576222` (freshness `unchanged`):

- Reader: `_any_delimited` takes bracket pairs (nesting via `_paired`) or any other self-closing punctuation. A bare `%` cannot open with `=`. A `%` right after an operand character (`\w ) ] }`) is never a literal, so `n%i==0` stays modulo.
- Proof: `Test_Ruby_percent_literal` in `test/test_languages/testRuby.py` runs every key example through `analyze_file.analyze_source_code`. Modulo forms (`a%=2`, `a %= 2`, `a = a % 2`, `a%i==0`) give CCN 2. `%q` with `@ # = ^ -` gives `f` CCN 2 with `g` retained. A nested `%q{…{…}…}` gives CCN 2. `testRuby.py`: 54 passed. Full `python -m pytest`: 3732 passed, 7 skipped. pycodestyle is clean.
- `make` under coverage failed only Svelte linear-time wall-clock assertions at load average ~29. They pass alone, and Svelte does not use the Ruby reader.

Learning: a bare `%` after a space-separated name, followed by punctuation that recurs on the line (`n %-2 - 1`), still reads as a literal. Ruby resolves this only by knowing whether the name is a local variable. Master already had this ambiguity for `| ! / " '`.

Refactor pass: moved the new `check_ccn` helper beside `check_tokens` (`testRuby.py` + `testRubyRegex.py`: 61 passed). The pass stopped at the file-size gate: `test/test_languages/testRuby.py` is 822 lines (796 before this slice). The developer chose to defer it on 2026-11-26.

Follow-up: split `test/test_languages/testRuby.py` (822 lines, limit 250) by Ruby reader topic: tokenizing, def/CCN, comment continuation, keyword method names, early-ending blocks, percent literals.

## Preparation review

Slice-plan refinement: not needed — one Behavior slice, settled premises, mapped outside-in proof, no cumulative design or sizing concern.

Remaining slice-specific concerns: none.
