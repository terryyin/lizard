# OCaml language support

**Mapped story identity:** SEED-029#ocaml-support
**Source:** [canonical story](../../seeds/SEED-029-ocaml-fsharp-support.md#ocaml-support)

## Goal and scope

OCaml maintainers analyze `.ml` sources through the existing CLI and Python
API and get each named function binding with CCN, NLOC, token count, and
parameter count, so they can find the functions that need review. The story's
Scope section is authoritative for required behavior, deferred promises, and
boundary assumptions.

**In scope:** a new `OCamlReader` in `lizard_languages/ocaml.py`, registered in
`lizard_languages/__init__.py`; a token generator over Pygments' OCaml lexer
that merges comment and string fragments into single tokens, keeps newlines,
and merges `|` `|` into `||`; OCaml states for structure-item boundaries,
argument counting, and match / `function` / `try` arm decisions; the README
language list; tests under `test/test_languages/testOCaml.py`.

**Excluded (story's deferred promises):** value bindings without arguments as
functions; module-qualified names; functors, objects, `method`s beyond the
common rule; separate reporting of local helpers or `fun` literals; `.mli`,
`.mll`, `.mly`, Reason; inert `{|…|}` quoted strings; modified CCN for
`match`; `match … with exception` arms, `or` / `&`; extension metrics.

**Architecture / PFE:** The reader is Lizard's established extension point
(ADR-0001 *Language reader*: inherits `CodeReader`, registered in
`lizard_languages/__init__.py`); no ADR or North Star change. Reuse found:

- `lizard_languages/erlang.py` already tokenises through
  `pygments.lex(…, lexers.get_lexer_by_name(...))`; Pygments is a declared
  dependency (`setup.py` `install_requires`). OCaml differs in keeping
  whitespace/newline tokens (Erlang drops them) and merging typed comment and
  string fragments, both needed for NLOC and inert contents (P2, P3).
- Rust's `_in_match_arms` / `_seen_match_arm` counting in
  `lizard_languages/rust.py` is the model for "n arms add n − 1, or-patterns
  add nothing"; OCaml counts arm `->` tokens inside `match`/`function`/`try`
  arms rather than Rust's `=>`.
- `get_comment_from_token` plus `comment_counter` in
  `lizard_core/file_analyzer.py` exclude comment lines from NLOC once the
  comment is one token.
- Test entry point follows `test/test_languages/dart_helpers.py`
  (`get_<lang>_function_list` via `analyze_file.analyze_source_code`) and the
  CLI subprocess test in `test/test_languages/testDart.py`
  (`TestDartCliDiscovery`).

No shared ML-family base for the sibling F# story (indentation-sensitive F#;
story membership does not justify that structure).

**Assumptions:** OCaml 4.14/5.x syntax; Pygments 2.21.0 as in this workspace;
no OCaml toolchain.

## Outside-in proof

Primary boundary: `analyze_file.analyze_source_code("a.ml", source)` through a
`get_ocaml_function_list` helper, one test per key example, asserting names,
CCN, NLOC, parameter counts, and start/end lines where boundaries are the
point. CLI discovery and `-l ocaml` selection at the `lizard.py` subprocess
boundary with a temporary directory holding `src/main.ml`, `src/main.mli`, and
`tool.py` (filesystem boundary only; no parsing mocks).

Local gates, cited from `AGENTS.md`: focused
`nix develop -c python -m pytest test/test_languages/testOCaml.py` during each
slice; before completing the change, `nix develop -c make` (full pytest,
`pycodestyle`, `pylint`). Hosted CI (`python-app.yml`) runs pytest across
Python 3.8–3.14 after publication; avoid syntax newer than 3.8.

| Promise (story scope) | Owning slice | Proof signal |
| --- | --- | --- |
| `.ml` discovered, `.mli` not, `-l ocaml`, help lists `ocaml`, README (example 1) | 1 | CLI test: default run lists `.ml` and `.py` functions, `-l ocaml` only `.ml`; no `main.mli` function; `-h` contains `ocaml`; README line |
| Top-level function bindings, curried / labelled / optional / unit parameters, `if` / `&&` / `||` / `while` / `for` (example 2) | 1 | `fact` CCN 2, 1 param, NLOC 1; `greet` CCN 1, 3 params; `let g a b = a \|\| b` CCN 2 |
| Local `let … in` and `fun` stay inside; value bindings not reported; boundary by heading-position `let` (example 6 boundaries) | 1 | functions exactly `sum` (all its lines) and `after`; no `go`, `next`, anonymous |
| Nested comments, escaped strings, char `'|'`, `'a` inert; comment lines out of NLOC (example 8) | 1 | function CCN 1; comment line excluded; next function recognised |
| Match / `function` arms n − 1, `when`, or-patterns nothing, `||` not an arm, `try` handlers each one (examples 3, 4, 5, 6 CCN) | 2 | `classify` CCN 6; `f` CCN 2, 0 params; `g` CCN 3; `read` CCN 3; `sum` CCN 2 |
| `let rec … and …` siblings, `module … struct … end`, `type` closes and adds nothing (example 7) | 3 | `even`, `odd` CCN 2 each; `g` CCN 3 ending at `end`'s line; `h` reported; type `\|` adds nothing |
| Other languages unchanged (example 9) | every slice | full suite green (`nix develop -c make` at the end) |

## Settled decisive premises

Observed on 2026-11-04 in this workspace
(`/Users/terryyin/git/lizard/.worktrees/support-ocaml-analysis`, branch
`claude/support-ocaml-analysis` at `2e1303f`), with a scratch `CodeReader`
subclass in a one-off Python process under the job's temporary directory,
appended to `lizard_languages.languages()` at runtime; no checkout file was
edited.

| # | Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- | --- |
| P1 | A `CodeReader` subclass with `ext = ['ml']`, `language_names = ['ocaml']` is selected by `analyze_source_code` and `lizard.main -l ocaml`, and `main.mli` is not analyzed | 1 | Temp dir with `main.ml`, `main.mli`, `tool.py`: `lizard -l ocaml <dir>` printed `g@3-5` and `h@5-6` from `main.ml` only, "1 file analyzed" | True |
| P2 | Pygments' OCaml lexer handles nested comments, escaped strings, `'x'` chars and `'a` type variables, but splits comments and strings into typed fragments, `\|\|` into two `\|`, and leaks `{\|…\|}` contents | 1, 2 | `lex()` on a sample: `'(*',' outer ','(*',' if nested ','*)',…` typed Comment; `"a\"if"` as String fragments; `'x'` one token; `'`,`a` for `'a`; `a`,`\|`,`\|`,`b`; `{`,`\|`,`if`,`\|`,`\|`,`then`,`\|`,`}` | True |
| P3 | Lizard's own regex tokenizer cannot replace it cheaply | 1 | `CodeReader.generate_tokens` on the same sample: `(`,`*` with no comment token; `'a) : '` swallowed as one char literal | True |
| P4 | Merging typed comment fragments into one `(*…*)` token with `get_comment_from_token` excludes comment lines from NLOC; merging `\|\|` makes it a counted condition | 1 | Two-line nested comment then `let g a b =` / `a \|\| b && c`: `g` CCN 3, NLOC 2; file NLOC 5 for 5 code lines | True |
| P5 | "A `let` awaiting no `in` is a new item" mis-splits functions | 1 | `let h x =` / `let y = x in` / `if y …`: `h` ended at line 6, its `if` uncounted (CCN 1) | True (rule rejected) |
| P6 | Heading-position rule (token before `let` ends an expression or is `;;`; after `=`, `in`, `->`, `then`, `else`, `;`, `(`, operators it is local) separates local and item `let`s | 1, 3 | Same probe: `h` lines 5–8 CCN 2; `sum` with `let rec go acc = function … in go 0 (List.filter (fun x -> …) l)` one function lines 8–14; `next`, `after` start at their own lines; `k` inside `module M = struct … end` CCN 3 ending at `end` | True |
| P7 | Nothing in product code or tests references OCaml | 1 | `grep -rni ocaml` over `lizard*`, `test`, `README.rst`: no matches | True |

## Ordered slices

### 1. OCaml files are discovered and their top-level functions analyzed
Type: Behavior
Status: planned
Proof: `testOCaml.py`: example 2 (`fact` CCN 2 / 1 param / NLOC 1; `greet`
CCN 1 / 3 params); `let g a b = a || b` CCN 2; example 6 boundaries (exactly
`sum` with its start/end lines, and `after`; no `go`, `next`); example 8 (CCN
1, comment line out of NLOC, next function found); CLI test for example 1;
README lists OCaml; full suite green.

Behavior: Given a `.ml` file → analyze through API or CLI → each top-level
`let` / `let rec` binding with arguments, or whose body begins with `fun` /
`function`, is one function ending at the next heading-position item or end of
file, with `if`, `while`, `for`, `&&`, `||` counted, curried arguments counted,
local lets and lambdas folded in, and comments / strings inert.

Interim behavior (named, replaced later): match / `function` / `try` arms and
`when` are uncounted until slice 2; `and` siblings, `module … struct … end`,
and `type` items are not boundaries until slice 3.

Files: `lizard_languages/ocaml.py`, `lizard_languages/__init__.py`,
`README.rst`, `test/test_languages/testOCaml.py` (+ `ocaml_helpers.py` if the
Dart helper pattern is followed).

### 2. Match, `function`, and `try` arms and guards count as decisions
Type: Behavior
Status: planned
Proof: `testOCaml.py`: example 3 (`classify` CCN 6; `f` CCN 2, 0 params),
example 4 (`g` CCN 3), example 5 (`read` CCN 3), example 6 (`sum` CCN 2); a
`fun x -> …` inside an arm and a type annotation `int -> int` add nothing.

Behavior: Given a function with `match … with`, `function`, or `try … with` →
analyze → n match/`function` arms add n − 1, each `try` handler arm adds one,
each `when` adds one, or-patterns and non-arm `->` add nothing, and `||`
remains one logical decision.

### 3. `and` siblings, modules, and type items bound functions
Type: Behavior
Status: planned
Proof: `testOCaml.py`: example 7 (`even`, `odd` each CCN 2; `g` CCN 3 with
end line at `end`; `h` reported; the type's `|` adds nothing); a `;;`
separator and `open M` between functions end the previous function; full
suite and `nix develop -c make` green.

Behavior: Given `let rec … and …`, `module Name = struct … end`, `type`,
`open`, or `;;` items → analyze → each `and` binding is its own function, a
struct's `end` closes its last function, and non-`let` items close the open
function without adding decisions.

## Current decisions

- Tokens come from Pygments' OCaml lexer (P2, P3); the reader corrects its
  fragmenting and `||` split rather than replacing it.
- Item-versus-local `let` uses the heading-position rule (P6), not `in`
  balance (P5).

## Plan concerns

Refinement: not needed — three Behavior slices each own one proof loop over
one boundary/decision model (tokens + heading rule in 1, arm counting on that
token stream in 2, more heading kinds in 3); no hidden preparation or
independent outcomes found.

Remaining slice-specific concerns: none identified in this review.
