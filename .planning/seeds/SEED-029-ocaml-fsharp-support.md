---
id: SEED-029
status: active
planted: 2026-11-04
planted_during: Product backlog intake requested by the developer
scope: OCaml and F# language support
---

# SEED-029: OCaml and F# support

## Why This Matters

OCaml and F# maintainers want to use Lizard to find complex functions in
their codebases. This request comes from [GitHub issue #303](https://github.com/terryyin/lizard/issues/303).
Each language can deliver that outcome independently.

OCaml has no braces or statement terminators around a function: a top-level
`let` binding ends where the next structure item begins, and the same `let`
keyword also opens local `let … in` expressions. Recognising where a function
ends, and counting `match` arms without miscounting `|` in or-patterns and
`||`, is the substance of OCaml support.

## Alternatives and Decision

Compared for OCaml against the goal and the constraint that Lizard readers are
self-contained token/state-machine parsers depending on no external language
toolchain (ADR-0001, *Language reader*; every existing reader):

- **Recommended tokenizer: Pygments' OCaml lexer, as the Erlang reader already
  uses for Erlang.** Pygments is already a required Lizard dependency (2.21.0
  in this workspace). Observed here on a scratch sample: nested `(* (* *) *)`
  comments, `"…\"…"` strings, `'x'` char literals, and `'a` type variables are
  all tokenised correctly, with comments and strings typed so their contents
  can be kept inert. Observed gaps the reader must correct: `||` arrives as two
  `|` tokens (and `<=` / `<>` as two tokens), and `{|…|}` quoted strings are not
  recognised, so their contents leak as code.
- Lizard's own regex tokenizer with OCaml additions: observed here to treat
  `(*` as two tokens with no comment, and to swallow `'a) : '` as one char
  literal because `'a` type variables look like quote openers. Nested comments
  also need a non-regex scanner. Rejected as more code for the same result.
- An external OCaml parser (compiler-libs, tree-sitter): contradicts the
  self-contained reader architecture. Rejected.
- Column-0 / indentation boundaries: OCaml is not layout-sensitive and hand
  formatting varies. Rejected.

Borrowed from document outlines, as the function-boundary rule: a heading
closes every open section at its level, and inline text never does. A
top-level structure item (`let`, `and`, `type`, `module`, `open`, `include`,
`exception`, `external`, `;;`) is the heading and closes the open function; a
struct's `end` closes its last section; `let … in`, `fun` and `function` are
inline text and stay inside the enclosing function. Where the analogy breaks:
an outline heading is marked syntactically, but OCaml's `let` is both heading
and inline text, and its `in` comes later. A first candidate, "a `let` is a
heading when no earlier `let` awaits its `in`", was observed false here: in
`let h x =` / `let y = x in …` the inner `let` has nothing pending yet. The
rule that held on the scratch probe is positional: outside brackets and
`begin … end`, a `let` is a heading when the token before it ends an
expression (a name, literal, `)`, `]`, `}`, `end`, `done`) or is `;;`, and is
inline text after a token that expects an expression (`=`, `in`, `->`,
`then`, `else`, `;`, `do`, `with`, `|`, an operator). OCaml's grammar does
not let a bare `let` be an application argument, which is why this holds.
The probe is evidence for the listed examples only, not a proof of the rule.

Challenge: the purpose (find functions to review) rests on complexity living
in named function bindings and on CCN ranking them usefully. The strongest
doubt is that an exhaustive `match` over a large variant (an interpreter's
`eval`) scores one per arm and can dominate warnings while being idiomatic,
low-risk dispatch. This is Lizard's established rule for `switch`, Rust
`match`, and Dart switch expressions, so the story keeps it; the usual escape,
modified CCN (`-m`), does not reach OCaml or Rust `match` today and is
deferred below rather than added here.

Architecture: no new consequential concern. The reader is one new module in
`lizard_languages/` registered like the others; Pygments is an existing
dependency. No shared ML-family base for the sibling F# story is built here:
F# is indentation-sensitive, and story membership alone does not justify that
structure.

## Selected Stories

<a id="ocaml-support"></a>

### 1. Support OCaml analysis

**Identity:** SEED-029#ocaml-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/020-ocaml-support/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"9cddd3731e3533bf8c4c676abd764d4f6be533270c0bca75f19985274e4fae91","plan":"7ded1c71125469e821ff22aa3b2726f5e84531dc5c0fbcbd7076f0634b3ad167"}}
```

**Goal:** OCaml maintainers point Lizard at their OCaml sources through the
existing CLI and Python API and receive each named function binding with CCN,
NLOC, token count, and parameter count, so they can find the functions that
need review. This answers the OCaml half of GitHub issue #303 and extends
Lizard's many-language promise to the ML family.

**Scope:**

Required behavior:

- `.ml` files are discovered by default, selectable with `-l ocaml`, listed in
  the `-l` help, and analyzable through `analyze_file` and
  `analyze_source_code`. The README language list gains OCaml.
- Function boundaries: each top-level `let` / `let rec` binding that takes
  arguments, or whose body starts with `fun` or `function`, is one function,
  including each `and` sibling of a `let rec … and …` group and bindings inside
  `module Name = struct … end`. A function ends at the next structure item
  (a `let` or `and` in heading position, `type`, `module`, `open`,
  `include`, `exception`, `external`, `;;`), at its struct's `end`, or at end
  of file. A `let` is in heading position outside brackets and `begin … end`
  when the token before it ends an expression or is `;;`; after `=`, `in`,
  `->`, `then`, `else`, `;`, or an operator it is a local `let … in`.
- Names: the binding's own name (`fact`, `g` inside `module M`).
- Local and anonymous functions: `let … in` local bindings, `fun`, and
  `function` belong to the enclosing function; their lines, tokens, and
  decisions count there and they are not reported separately.
- Decisions (CCN): `if`, `while`, `for`, `&&`, `||`, and each `when` guard add
  one. A `match … with` or `function` with n arms adds n − 1, the same rule as
  Rust `match`, so a match and its equivalent `if … else` chain score alike;
  or-patterns (`| A | B ->`) add nothing. Each handler arm of `try … with` adds
  one, as `catch` does elsewhere. Not decisions: `else`, `then`, `begin`,
  type-definition `|`, `->` in `fun` or type annotations.
- Parameters: each curried argument before `=` counts one, including labelled
  `~x`, optional `?x` / `?(x = d)`, annotated `(x : t)`, and unit `()`.
- Strings and comments: `(* … *)` including nested comments, `"…"` with
  escapes, and `'x'` char literals introduce neither decisions nor functions;
  `'a` type variables do not disturb tokenisation; comment lines are excluded
  from NLOC.
- Existing language analysis is unchanged: the full suite stays green.

Deferred promises (not built or verified by this story):

- Value bindings without arguments (`let x = …`, `let () = …`, `let _ = …`)
  are not reported as functions; reporting a complex `let () =` entry block is
  a possible follow-up.
- Module-qualified names (`M.g`), and functors, first-class modules, objects
  and `method`s, beyond what the common rule yields.
- Reporting local `let … in` helpers or `fun` literals as separate functions.
- `.mli` signature files (they hold no function bodies), `.mll` / `.mly`
  generator sources, and Reason syntax.
- `{|…|}` and `{id|…|id}` quoted strings: their contents are not promised to be
  inert (an observed Pygments gap).
- Modified CCN (`-m`) treating a whole `match` as one; it does not reach Rust
  `match` today either.
- `match … with exception E ->` arms, deprecated `or` / `&` operators, and
  extension metrics beyond the core four.

Rejection constraints: none. No product requirement calls for rejecting any
OCaml input.

Boundary assumptions:

- Current OCaml 4.14/5.x syntax as formatted by hand or `ocamlformat`; Lizard
  depends on no OCaml toolchain.
- Pygments' OCaml lexer, already a Lizard dependency, supplies tokens.

**Key examples:**

1. **Discovery and selection.** A directory holds `src/main.ml`,
   `src/main.mli`, and `tool.py` → `lizard .` reports the `.ml` functions and
   the Python ones; `lizard -l ocaml .` reports only the `.ml` functions; the
   `-l` help lists `ocaml`.
2. **Straight-line and branching.** `let rec fact n = if n <= 1 then 1 else n *
   fact (n - 1)` followed by `let greet ~name ?(punct = "!") () =
   print_endline (name ^ punct)` → `fact` CCN 2 with 1 parameter; `greet` CCN 1
   with 3 parameters; both NLOC 1.
3. **Match arms, guards, and or-patterns.** `let classify x = match x with | 0
   | 1 -> "small" | n when n < 0 -> "neg" | n -> if n > 9 && n < 100 then "mid"
   else "big"` → `classify` CCN 6: two extra arms, `when`, `if`, `&&`; the
   or-pattern adds nothing. `let f = function Some x -> x | None -> 0` →
   function `f`, CCN 2, 0 parameters.
4. **`||` is not an arm.** `let g a b = match a with | true -> a || b | false
   -> not b` → CCN 3: one extra arm and `||`.
5. **Exceptions.** `let read k = try find k with Not_found -> None | Invalid_argument
   _ -> None` → CCN 3: two handler arms.
6. **Local helpers and lambdas stay inside.** `let sum l = let rec go acc =
   function [] -> acc | x :: t -> go (acc + x) t in go 0 (List.filter (fun x ->
   x > 0) l)` laid out over several lines, followed by `let next = 1` and `let
   after x = x` → functions `sum` (CCN 2, all its lines) and `after`; no `go`,
   anonymous, or `next` function.
7. **Groups and modules.** `let rec even n = if n = 0 then true else odd (n -
   1) and odd n = if n = 0 then false else even (n - 1)` → `even` and `odd`,
   each CCN 2. `module M = struct let g a b = a && b || not a end` followed by
   `type t = A | B` and `let h x = x` → `g` (CCN 3, ending at `end`) and `h`;
   the type's `|` adds nothing to anything.
8. **Strings and comments stay inert.** A function preceded by `(* if (* while
   *) || *)` whose body is `let s = "if \" then" in let c = '|' in …`, with an
   annotation `(x : 'a list)` → that function CCN 1, its comment line excluded
   from NLOC, and the next function still recognised.
9. **Other languages unchanged.** The full test suite after the reader lands →
   green, with no change to any existing language's results.

<a id="fsharp-support"></a>

### 2. Support F# analysis

**Identity:** SEED-029#fsharp-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As an F# maintainer, I can analyze F# source files with Lizard and see
function names and cyclomatic complexity so I can find functions to review.

**Evaluation:** Lizard discovers a representative `.fs` file and reports
separate named functions with the expected complexity for straight-line and
branching code.

**Refinement questions:** Settle initial coverage for indentation, members,
scripts, and signature files.
