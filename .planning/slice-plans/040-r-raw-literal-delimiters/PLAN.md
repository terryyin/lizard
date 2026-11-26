# Keep valid R raw-string delimiter forms opaque

**Identity:** SEED-075#r-raw-literal-delimiters

**Source:** [Keep valid R raw-string delimiter forms opaque](../../seeds/SEED-075-reader-corrections.md#r-raw-literal-delimiters). The original proposals are [#638](https://github.com/terryyin/lizard/pull/638) at `ad503916d19b37ca0eedf208f35dbbda589dcc71`.

## Goal and scope

R users get correct CCN and following function locations for raw literals with either quote style and supported delimiters. Follow #638's direction: a raw string is one token. Cover the `r` or `R` prefix, a matching double or single quote, zero or more dashes, and `(`, `[`, `{` or `|` delimiters closing with the same dashes and quote. Preserve #638's double-quote behavior, ordinary strings, identifiers named `r` and unrelated reader behavior. Out of scope: other SEED-075 corrections, non-R languages and unterminated raw strings.

## Decisive premises

Observed at `bdafc0a1` in this workspace with `nix develop -c env PYTHONPATH=<checkout> python` calling `analyze_file.analyze_source_code("s.r", ...)` on `f <- function() { x <- <raw>; if (a) {} }` then `g <- function() { 1 }`. The #638 rows ran in a scratch detached worktree at `ad503916`, removed afterwards. The corrected rows applied one pattern in `RReader.generate_tokens` in this workspace and reverted it: `[rR](?P<rq>["'])(?P<rd>-*)(?:\(.*?\)|\[.*?\]|\{.*?\}|\|.*?\|)(?P=rd)(?P=rq)`. R's lexer in [gram.y](https://raw.githubusercontent.com/wch/r-source/trunk/src/main/gram.y) (`RawStringValue`) accepts `'` and `"`, dashes, then `(`, `[`, `{` or `|`, and closes on the delimiter, the same dashes and the same quote.

| Premise | Consuming operation | Result (master / #638 / corrected) |
| --- | --- | --- |
| The finding reproduces for single quotes | `r'(some ' if (hidden) {} ' text)'` | f CCN 3 / 3 / **2**; g at line 2 |
| Other single-quote forms fail the same way | `R'[...]'`, `r'--{...}--'` | f CCN 3 / 3 / **2** |
| Vertical-bar forms fail on both | `r"\|...\|"`, `r'\|...\|'`, `r"--\|...\|--"` | f CCN 3 / 3 / **2** |
| #638's double-quote forms keep working | `r"(text " if (x) {} " end)"` | f CCN 3 / 2 / 2 |
| A close without the dashes or quote ends nothing | `r"-(a)" if (x) {} )-"` | f CCN 3 / 2 / 2 |
| Multiline single-quote raw string keeps following lines | `r'-(line` newline `' if (x) {} '` newline `last)-'` | f CCN 3 / 3 / **2**; g at line 4 in all |
| Ordinary strings and identifier `r` stay unaffected | `"r\"("` and `r <- 1; if (r) {}` | f CCN 2 / 2 / 2 |
| Existing R reader tests stay green with the pattern | `testR.py` and `test_lizard_ruby_noncode.py`, plus the language and extension suites | 41 passed; 1429 language-filtered and 3052 language and extension tests passed |

The root cause is that the R lexer recognizes raw strings only through its generic string tokens, which end at the first embedded quote. #638 adds a prefix pattern for `"` with three delimiters; single quotes and `|` remain generic strings. No premise remains open.

## Current decisions

Use the existing R language reader token patterns and one definition of the delimiter table: a single raw-string pattern with a quote and a dash backreference covers every form. Isolate lexical pattern construction from the already oversized R function states, outside `RStates`. Bring in #638's tests rather than a competing set. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

No North Star topic bears on this reader correction beyond that ADR.

## Outside-in proof

| Promise | Owning slice | Proof |
| --- | --- | --- |
| Each quote, delimiter and dash form gives f CCN 2 with g at line 2 | 1 | `analyze_file.analyze_source_code` on the seed sources |
| Single-quote multiline raw string gives f CCN 2 and g at line 4 | 1 | same entry point |
| #638's double-quote forms and the false-close case keep their results | 1 | #638's `test_lizard_r_raw_strings.py` cases |
| Ordinary strings and identifier `r` open no raw string | 1 | same entry point |
| Existing R reader tests stay green | 1 | `nix develop -c python -m pytest test/test_languages/testR.py` |

## Ordered slices

### 1. Keep valid R raw-string delimiter forms opaque

Type: Behavior

Status: done

Proof: `analyze_file.analyze_source_code` on the key-example sources reports the promised CCN and function start lines. #638's tests and `testR.py` stay green. Before merge, run full `nix develop -c python -m pytest` and `nix develop -c make`. Pair affected API and broad CLI timing on the exact corrected production bytes. Require green hosted checks on the published revision.

Behavior: Given R source with a raw string using either quote, any dash count and a `(`, `[`, `{` or `|` delimiter that contains an embedded quote and apparent code, when the reader analyzes it, then only the real decision counts and following functions keep their start lines.

Bring in #638's reader change and tests, extend its pattern to both quotes and the vertical-bar delimiter, and add the single-quote, bar and multiline regressions. Compare master, the exact proposal and the corrected revision.

Accepted proof, before publication: `nix develop -c python -m pytest test/test_languages/test_lizard_r_raw_strings.py` (6 passed) and `nix develop -c python -m pytest test/test_languages/testR.py` (29 passed). Both ran at `d576262e` with these uncommitted files. The new file asserts CCN and, where the promise names one, the following function's start line, through `analyze_file.analyze_source_code`. The existing R suite still passes. Refactor found no edit: the one raw-string pattern is in `RReader.generate_tokens`, outside `RStates`. No later slice remains, and no learning changes later work.

## Execution complete

Product advice: no change. The delivered raw-string token matches the story, and the focused R proof is accepted. No correction.

## Preparation review

Slice-plan refinement: not needed. The plan has one Behavior slice, settled premises and mapped outside-in proof, with no cumulative design or sizing concern.

Remaining slice-specific concerns: none.

## Execution

Story Branch Mode. Owned workspace `/Users/terryyin/git/lizard/.worktrees/keep-valid-r-raw-string-delimiter-forms-opaque`, branch `cursor/keep-valid-r-raw-string-delimiter-forms-opaque`, starting revision `47e27cf4b521cd5febe7a6bc4fc911e11a789c33`, reused. Integration checkout `/Users/terryyin/git/lizard`. Remote `origin`, trunk `master`.

Published claim `d576262e3e0d1f6c02ea15046a1832bbb1e5dc22` on `origin/cursor/keep-valid-r-raw-string-delimiter-forms-opaque`. That claim is `pendingCi: unobserved`. Increment `04341f29ecd4b97fa6acfe9fab2e46caad4f1207` is the accepted story-branch candidate; suffix base `d576262e3e0d1f6c02ea15046a1832bbb1e5dc22`. CI source is GitHub Actions, workflow `python-app.yml` (`Python application`). Observer `/tmp/dough-ci-501/watch-wzfKXe` watches `terryyin/lizard` branch `cursor/keep-valid-r-raw-string-delimiter-forms-opaque`.
