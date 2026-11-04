# F# language support

**Mapped story identity:** SEED-029#fsharp-support
**Source:** [canonical story](../../seeds/SEED-029-ocaml-fsharp-support.md#fsharp-support)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

F# maintainers analyze `.fs` and `.fsx` sources through the existing CLI and
Python API and get each named function binding and method with CCN, NLOC,
token count, and parameter count, so they can find the functions that need
review. The story's Scope section is authoritative for required behavior,
deferred promises, and boundary assumptions.

**In scope:** a new `FSharpReader` in `lizard_languages/fsharp.py`, registered
in `lizard_languages/__init__.py`, with `ext = ['fs', 'fsx']` and
`language_names = ['fsharp']`; a token generator over Pygments' F# lexer that
joins comment (including `String.Doc`) and string fragments into single
tokens, joins adjacent operator characters (`||`, `|>`, `|||`, `<|`, `:?>`,
…) into one token, keeps newlines, and emits each line's leading indentation
so the reader's `preprocess` can tell the state machine the column of a
line's first code token; F# states for offside item boundaries, `let` and
member recognition, argument counting, and match / `function` / `try` arm
decisions; directive lines made inert; the README language list and
`docs/fsharp-analysis.md`; tests under `test/test_languages/testFSharp.py`.

**Excluded (story's deferred promises):** `.fsi` files and non-light syntax;
properties with bodies, explicit accessors and `new(…)` constructors as
functions; active patterns and operator definitions; type-qualified names;
interpolated-string expressions; undentations other than `|` and infix lines;
tab indentation; modified CCN for `match`; extension metrics; legacy `or` /
`&`.

**Architecture / PFE:** The reader is Lizard's established extension point
(ADR-0001 *Language reader*: inherits `CodeReader`, registered in
`lizard_languages/__init__.py`); no ADR change. Reuse found:

- `lizard_languages/ocaml_tokens.py` already joins Pygments comment and string
  fragments and `|` `|`. F# needs the same joining plus `String.Doc` as a
  comment, adjacent-operator joining, and indentation tokens. Slice 1 moves
  the joining into one parameterized module shared by both readers rather
  than copying it (AGENTS.md principle 1: one concept in one place).
- `lizard_languages/ocaml_arms.py` (`ArmTracker`) counts `match` / `try` /
  `function` arms by `with`, `|` and `->` at a bracket depth; F# uses the same
  tokens (`match!` lexes as `match` + `!`). Slice 2 gives it a language-neutral
  module name and imports it from both readers.
- `lizard_languages/python.py` `preprocess` is the precedent for computing a
  line's leading indentation from raw tokens before whitespace is dropped;
  `lizard_core/file_analyzer.py` `preprocessing` calls it when present.
- `lizard_core/file_analyzer.py` `condition_counter` counts every token in
  `reader.conditions`, so `when` is not an F# condition keyword there; the
  arm tracker counts it only in a pattern (guard) position.
- `get_comment_from_token` plus `comment_counter` exclude comment lines from
  NLOC once a comment is one token; `//` and `///` tokens return their text.
- Test entry point follows `test/test_languages/ocaml_helpers.py`
  (`get_<lang>_function_list`, `spans`) and the CLI subprocess test
  `TestOCamlCliDiscovery` in `testOCaml.py`.

No shared ML-family base class: the two readers share two helper modules and
nothing else.

**Assumptions:** F# 6+ light syntax indented with spaces, in files that
compile; Pygments 2.21.0 as in this workspace; no .NET toolchain.

## Outside-in proof

Primary boundary: `analyze_file.analyze_source_code("a.fs", source)` through a
`get_fsharp_function_list` helper, one test per key example, asserting names,
CCN, parameter counts, NLOC, and start/end lines where boundaries are the
point. CLI discovery and `-l fsharp` at the `lizard.py` subprocess boundary
with a temporary directory holding `src/Lib.fs`, `src/Lib.fsi`, `Script.fsx`
and `tool.py` (filesystem boundary only; no parsing mocks).

Local gates, cited from `AGENTS.md`: focused
`nix develop -c python -m pytest test/test_languages/testFSharp.py` during
each slice, plus `test/test_languages/testOCaml.py
test/test_languages/testOCamlItemBoundaries.py` whenever a shared module
moves; before completing the change, `nix develop -c make` (full pytest,
`pycodestyle`, `pylint`). Hosted CI (`python-app.yml`) runs pytest across
Python 3.8–3.14 after publication; avoid syntax newer than 3.8.

| Promise (story scope) | Owning slice | Proof signal |
| --- | --- | --- |
| `.fs` and `.fsx` discovered, `.fsi` not, `-l fsharp`, help lists `fsharp`, README and docs page (example 1) | 1 | CLI test: default run lists `.fs`, `.fsx` and `.py` functions, `-l fsharp` only F#; nothing from `Lib.fsi`; `-h` contains `fsharp`; README line and `docs/fsharp-analysis.md` exist |
| Top-level `let` functions, curried / annotated parameters, `()` is zero, `if` / `elif` / `&&` / `\|\|` (example 2; example 12's `f`) | 1 | `fact` CCN 2, 1 param, NLOC 1; `greet` 2 params; `g` CCN 2; `let f () = ()` 0 params |
| Offside boundaries: locals, `fun`, values, tuple patterns stay inside; next item at or left of the column ends the binding (example 6) | 1 | exactly `sum` (lines 1–6) and `after`; no `go`, `next`, `a`, `table`, `build` |
| Comments, strings, chars, `'a`, attributes inert; comment lines out of NLOC (example 9) | 1 | `main` CCN 1, NLOC 5, starts at its `let` line; file NLOC excludes the 3 comment lines; `h` found |
| Bracket-suspended offside and `\|` / infix continuation (example 10) | 1 | `f` spans through the `]` line; `eval` spans both arm lines (CCN asserted in slice 2) |
| Match / `function` / `match!` arms n − 1, `when` guards, `elif`, or-patterns nothing, `try` handlers one each, `finally` nothing, pipelines and `\|\|\|` not arms, constraint `when` nothing (examples 3, 4, 5, 10, 12) | 2 | `classify` CCN 7; `f` CCN 2, 0 params; `g` CCN 3; `read` CCN 3; `safe` CCN 1; `eval` CCN 2; generic `f` CCN 1; `run` CCN 2 |
| Members, type-level `let`s, properties and abstract declarations not reported, union members, nested modules, `and` groups (examples 7, 8) | 3 | `bump`, `Increment` (0 params, 3 lines), `Add` (2 params), `Create`, `ToString`, `Foo` as listed; no `count`, `Value`, `Name`; `Scale` CCN 2, no `Area`; `f`, `g`, `h`, `top`; `even`, `odd` CCN 2 |
| Scripts and `#` directive lines inert (example 11) | 3 | `Script.fsx`: `f` CCN 2; `g` ends before the `printfn` line |
| Other languages unchanged (example 13) | every slice | full suite green (`nix develop -c make` at the end) |

## Settled decisive premises

Observed on 2026-11-04 in this workspace
(`/Users/terryyin/git/lizard/.worktrees/support-f-analysis`, branch
`claude/support-f-analysis` at `c8ae80a`), with one-off Python processes under
the job's temporary directory: a Pygments lexing probe, the installed OCaml
reader run on F# text, and a scratch `CodeReader` subclass appended to
`lizard_languages.languages()` at runtime; no checkout file was edited.

| # | Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- | --- |
| P1 | Pygments 2.21.0 has an F# lexer that keeps nested comments, strings, chars and `'a` safe, and whose whitespace tokens carry indentation | 1 | `get_lexer_by_name('fsharp')` → `FSharpLexer` (`*.fs`, `*.fsi`, `*.fsx`); `(* outer (* if nested *) \|\| *)` all `Comment`; `"a\"if\" b"`, `@"…"`, `"""…"""`, `$"…"` all `String` fragments; `'\|'` one `String.Char`; `'`, `a` for `'a`; `'\n    '` one whitespace token; CRLF and `﻿` absent from output | True |
| P2 | The lexer splits multi-character operators and types `///` as a string | 1, 2 | `a \|\| b` → `\|`, `\|`; `x \|> f` → `\|`, `>`; `1 \|\|\| 2` → three `\|`; `:?>` → `:`, `?`, `>`; `/// doc` → `String.Doc`; `[<`, `>]`, `[\|`, `\|]`, `()`, `->`, `<-`, `..` single tokens; `match!` → `match`, `!`; `and` → `Operator.Word`; `rec`, `inline`, `mutable`, `private` → `Name.Variable` | True |
| P3 | The OCaml heading-position rule mis-splits F# light syntax | 1 | `OCamlReader` on `let f x =` / `printfn "a"` / `let g y = y + 1` / `g x` / `let h z = z`: `f` 1–2, `g` 3–4, `h` 5 | True (rule rejected) |
| P4 | A reader `preprocess` sees raw whitespace tokens and can hand the state machine each line's first-token column | 1 | `file_analyzer.preprocessing` calls `reader.preprocess(tokens)` before any filtering; `PythonReader.preprocess` already counts leading spaces this way | True (from code) |
| P5 | Offside at bracket depth 0, with `\|` / infix continuation and `type` / `module` / `namespace` as item containers, yields the story's boundaries, names and counts | 1, 2, 3 | Scratch reader on examples 2–12: `fact` (2, 1 param), `greet` (2 params), `g` 2; `classify` 7, `f` (2, 0 params); `g` 3; `read` 3, `safe` 1; `sum` 1–6 CCN 2 and `after` only; `bump` 2, `Increment` 4–6 0 params, `Add` 2 / 2 params, `Create` 0, `ToString`, `Foo` 2, `Scale` 2, no `Value` / `Name` / `Area`; `f`, `g` 2, `h`, `top`; `main` 5–9 CCN 1 NLOC 5, file NLOC 7; `f` 1–4 through `]`, `eval` 8–10 CCN 2; `run` 2; `even`, `odd` 2; CRLF sample identical to LF | True |
| P6 | `#if` tokens and constraint `when` reach the condition counter unless the reader removes them | 2, 3 | Same scratch reader without those fixes: example 11's `f` CCN 3 (expected 2); example 12's generic `f` CCN 2 (expected 1) | True (fixes required) |
| P7 | Nothing in product code, tests or README references F# | 1 | `grep -rni "fsharp\|f#"` over `lizard_languages`, `lizard_core`, `test`, `README.rst`, `docs`: no matches | True |

## Ordered slices

### 1. F# files are discovered and their top-level functions analyzed
Type: Behavior
Status: done
Proof: `testFSharp.py`: example 2 (`fact` CCN 2 / 1 param / NLOC 1; `greet`
2 params; `g` CCN 2); `let f () = ()` 0 params; example 6 boundaries
(exactly `sum` 1–6 and `after`; no `go`, `next`, `a`, `table`, `build`);
example 9 (`main` CCN 1, NLOC 5, comment lines out of file NLOC, `h` found);
example 10 spans (`f` through `]`, `eval` over its arm lines); CLI test for
example 1; README lists F#; `docs/fsharp-analysis.md` exists; OCaml tests
still green after the shared token module moves; full suite green.

Behavior: Given a `.fs` or `.fsx` file → analyze through API or CLI → each
`let` / `let rec` binding that is the first code token of its line with
brackets closed, takes arguments or starts with `fun` / `function`, is one
function ending on the last code line before the next line whose first code
token is at or left of its column (brackets closed; `|` and infix lines
continue), with `if` / `elif` / `while` / `for` / `&&` / `||` counted, curried
and tuple arguments counted (`()` zero), locals, lambdas and values folded
in, and comments, doc comments and strings inert.

Interim behavior (named, replaced later): match / `function` / `try` arms are
uncounted and `when` counts nowhere until slice 2; `member`, `and`, `type`
and `module` headings are plain offside items until slice 3, so members are
not yet reported and bindings nested under `type` or `module` are found only
by column; `#` directive tokens still reach the counters until slice 3.

Files: `lizard_languages/fsharp.py`, `lizard_languages/fsharp_tokens.py` or
the shared fragment module it extracts from `ocaml_tokens.py`,
`lizard_languages/__init__.py`, `README.rst`, `docs/fsharp-analysis.md`
(behavior delivered so far), `test/test_languages/testFSharp.py`,
`test/test_languages/fsharp_helpers.py`.

### 2. Match, `function`, `try` arms and guards count as decisions
Type: Behavior
Status: done
Proof: `testFSharp.py`: example 3 (`classify` CCN 7; `f` CCN 2, 0 params),
example 4 (`g` CCN 3), example 5 (`read` CCN 3, `safe` CCN 1), example 10
(`eval` CCN 2), example 12 (generic `f` CCN 1, `run` CCN 2); OCaml tests
green after the arm module is shared.

Behavior: Given a function with `match … with`, `match! … with`, `function`,
or `try … with` → analyze → n arms add n − 1, each `try` handler arm adds
one, each pattern-guard `when` adds one, or-patterns, `|>` / `||>` / `|||`,
union-case bars, non-arm `->`, `finally`, and constraint `when` add nothing.

Files: `lizard_languages/fsharp.py`, the shared arm module renamed from
`ocaml_arms.py` with `ocaml.py` updated, `docs/fsharp-analysis.md`.

### 3. Members, nested modules, groups and directives bound functions
Type: Behavior
Status: done
Proof: `testFSharp.py`: example 7 (`bump`, `Increment` 0 params over three
lines, `Add` CCN 2 / 2 params, `Create` 0 params, `ToString`, `Foo` CCN 2 on
the `default` line only; no `count`, `Value`, `Name`, abstract `Foo`; `Scale`
CCN 2, no `Area`), example 8 (`f`, `g` CCN 2, `h`, `top`; `even`, `odd` CCN
2), example 11 (`f` CCN 2; `g` ends before `printfn`); CLI test extended with
`Script.fsx`; `nix develop -c make` green.

Behavior: Given `type … =` with `let` and `member` / `static member` /
`override` / `default` bindings, union types with members, nested `module … =`
blocks, `let rec … and …` groups, attributes and `#` directive lines →
analyze → each member with a parameter list and each nested `let` function is
its own function under its own name; properties, auto-properties, abstract
declarations and value bindings are not; directive lines add nothing and
belong to no function.

Files: `lizard_languages/fsharp.py`, `lizard_languages/fsharp_tokens.py` (or
the shared module), `docs/fsharp-analysis.md` (final rules and limitations),
`test/test_languages/testFSharp.py`.

## Current decisions

- Tokens come from Pygments' F# lexer (P1, P2); the reader joins fragments and
  adjacent operator characters rather than replacing the lexer.
- Item boundaries use the offside rule at bracket depth 0 with `|` / infix
  continuation and container headings (P5), not the OCaml heading-position
  rule (P3).
- `when` is counted by the arm tracker in pattern position, not by the
  generic condition counter (P6); `#` directive lines are removed before the
  counters (P6).
- `()` counts zero parameters (story scope); the OCaml reader is unchanged.

## Learnings

- Slice 1 (accepted proof: `nix develop -c python -m pytest
  test/test_languages/testFSharp.py test/test_languages/testOCaml.py
  test/test_languages/testOCamlItemBoundaries.py` 54 passed; full suite 2987
  passed, 7 skipped). The shared joiner is `lizard_languages/pygments_fragments.py`
  (`ocaml_tokens.py` removed); the value-binding fold is
  `CodeStateMachine.fold_into_enclosing_function` in `code_reader.py`.
- Line starts reach the state machine as `LineStart` str tokens carrying
  `column` (not via `preprocess`); later processors must not rebuild them as
  plain strings. Operator runs are joined (`|>`, `||`, `|||`, `->`, `:?>`, …);
  `#`, `$`, `,` never join; `{|` / `|}` arrive split.
- F# lexes with `stripnl=False` to keep leading blank lines' line numbers.
- `[<Attr>] let f …` on one line starts a binding at the attribute's column;
  slice 3 extends this to `[<Attr>] member …`.
- Example 6's `sum` CCN 2 depends on `function` arm counting: owned by slice 2
  (O2), not slice 1.
- `_starts_item` uses one `_item_column`; slice 3's `type` / `module` /
  `namespace` containers need a stack of item columns.
- Slice 2 proof adds example 6's `sum` CCN 2.
- Slice 2 (accepted proof: `nix develop -c python -m pytest
  test/test_languages/testFSharp.py test/test_languages/testFSharpItemBoundaries.py
  test/test_languages/testFSharpArmDecisions.py test/test_languages/testOCaml.py
  test/test_languages/testOCamlItemBoundaries.py` 67 passed; full suite 3000
  passed, 7 skipped). The arm tracker is `lizard_languages/match_arms.py`
  (`ocaml_arms.py` renamed); F# counts `when` only where
  `ArmTracker.in_pattern` holds. The tracker resets at each item start, so
  slice 3 members get a fresh tracker. Arms attach by bracket depth; nested
  indentation-only `match` / `try` totals were probed correct (4 and 3).
- F# tests are split: `testFSharp.py` (recognition, comments, CLI),
  `testFSharpItemBoundaries.py`, `testFSharpArmDecisions.py`; shared helpers
  live in `fsharp_helpers.py` (`by_name`, `spans`).
- Slice 3 (accepted proof: focused F# + OCaml modules incl.
  `test/test_languages/testFSharpMembers.py` 76 passed; `nix develop -c make`
  exit 0, 3009 passed, 7 skipped). Offside items are a stack in
  `lizard_languages/fsharp_items.py` (`OffsideItems`, `CONTAINER_HEADINGS`);
  `and` continuing a `type` group is a type heading; `#` directive lines are
  dropped in `fsharp_tokens.py` (so they count in no NLOC, O5). `fsharp.py` is
  at 250 lines; the binding-heading parser is the seam for a later split.

## Plan concerns

Refinement: not needed — three Behavior slices each own one proof loop over
one boundary/decision model (tokens, columns and offside items in 1; arm
counting on that token stream in 2; more heading kinds and inert directives
in 3); the two shared-module moves are the structure each slice's behavior
needs and are proven by the existing OCaml tests.

Remaining slice-specific concerns: none identified in this review.

## Story obligations

### O1. Deferred promises stay out of this story
Reported: slice 1 — "Source review lists the story's deferred promises X1–X17 as exclusions."
Story clause: "Deferred promises (not built or verified by this story):"
Disposition: excluded "Deferred promises (not built or verified by this story):"
Commitment: X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17

### O2. Arms and guards uncounted until slice 2
Reported: slice 1 — "match / `function` / `try` arms are uncounted and `when` counts nowhere until slice 2; example 6's `sum` reads CCN 1, not 2."
Story clause: "A `match … with`, `match! … with`, or `function` with n arms adds n − 1"
Disposition: proved by slice 2: test/test_languages/testFSharpArmDecisions.py TestFSharpArmDecisions and the E6/E10 span tests in testFSharpItemBoundaries.py asserting `sum` CCN 2 and `eval` CCN 2

### O3. Members and nested items only by column until slice 3
Reported: slice 1 — "`member`, `and`, `type` and `module` headings are plain offside items until slice 3, so members are not yet reported and bindings nested under `type` or `module` are found only by column."
Story clause: "Bindings written directly under a `type`, `module`, or `namespace` heading are bindings at their own deeper column, so nested modules and type members are found."
Disposition: proved by slice 3: test/test_languages/testFSharpMembers.py TestFSharpMembers, TestFSharpNestedItems

### O4. Directive lines not inert until slice 3
Reported: slice 1 — "Pygments lexes `#if` as `#` + `if`, so a directive at a function's column ends that function and the `if` counts as a decision."
Story clause: "`#r`, `#load`, `#if`, `#else` and `#endif` lines add no decisions and no functions"
Disposition: proved by slice 3: test/test_languages/testFSharpMembers.py TestFSharpDirectives.test_directive_lines_are_inert

### O5. Directive lines count in no NLOC (first reported in slice 3)
Reported: slice 3 — "Directive lines are removed from the token stream, so they also count in no NLOC, file NLOC included (`Script.fsx` file NLOC is 5)."
Story clause: "`#r`, `#load`, `#if`, `#else` and `#endif` lines add no decisions and no functions"
Disposition: no user cost "so they can find the functions that need review": directive lines belong to no function, so every function's NLOC, CCN, tokens and parameters are unchanged; only the file-level total omits compiler directives, which hold no reviewable code (inspected: Script.fsx example 11 `f` NLOC/CCN in testFSharp directive test and CLI run)

### O6. Interface implementation and object-expression members not reported (first reported in slice 3)
Reported: slice 3 — "Members inside `interface … with` and object expressions are not reported."
Story clause: "interface implementations and object expressions beyond what the common rule yields"
Disposition: excluded "interface implementations and object expressions beyond what the common rule yields"
