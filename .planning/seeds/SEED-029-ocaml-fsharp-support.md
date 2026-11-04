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

### OCaml

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

### F#

Compared for F# against the same goal and the self-contained reader
constraint (ADR-0001, *Language reader*):

- **Recommended: a new `FSharpReader` over Pygments' F# lexer with an offside
  state machine.** Pygments 2.21.0 ships `FSharpLexer` (`*.fs`, `*.fsi`,
  `*.fsx`). Observed here: nested `(* (* *) *)` comments, `//` comments,
  escaped, verbatim `@"…"`, triple-quoted and `$"…"` strings, `'|'` chars and
  `'a` type variables all tokenize safely; `///` doc comments arrive as
  `String.Doc`; whitespace tokens carry each line's indentation (`'\n    '`);
  CRLF line ends and a UTF-8 BOM are normalized away. Gaps the reader corrects:
  `||`, `|>`, `<|`, `||>`, `|||`, `<>`, `:?>` arrive as single-character
  operator tokens, so adjacent operator characters are joined back into one
  token; `#if` directive tokens and `when` inside a generic constraint would
  otherwise reach the condition counter. The offside rule gives item
  boundaries: a binding ends at the next line whose first token, with brackets
  closed, is at or left of the binding's column; a scratch reader on the key
  examples produced every expected name, span, CCN and parameter count.
- Reuse the OCaml reader's heading-position rule: observed false for light
  syntax. In `let f x =` / `printfn "a"` / `let g y = y + 1` / `g x`, the
  OCaml reader reports `g` as a top-level function, because F# sequences
  statements by newline rather than `;`, so the token before a local `let`
  ends an expression. Indentation is the only signal, which is why the OCaml
  story built no shared ML base. The arm counting (`match` / `function` /
  `try` arms, or-patterns) and the Pygments fragment joining do transfer, and
  F# reuses them rather than copying them.
- Lizard's own regex tokenizer: the OCaml story's observed `(*` and `'a`
  failures apply unchanged to F#. Rejected.
- An external parser (FSharp.Compiler.Service, tree-sitter-fsharp): contradicts
  the self-contained reader architecture. Rejected.

Borrowed from Markdown nested lists, as the boundary rule: a list item owns
every following line indented past its marker; a line at or left of the marker
starts a new item or ends the list; a fenced block inside an item ignores
indentation until it closes; and a lazy continuation line may stay unindented
and still belong to the item. The mapping: list item ↔ binding, marker column ↔
the binding's first-token column, nested list ↔ bindings under a `type` or
`module` heading, fenced block ↔ an open bracket, lazy continuation ↔ an
undented `|` arm or infix line. Where the analogy breaks: Markdown's lazy
continuation accepts any text, but F# accepts only `|` and infix tokens there,
so an identifier at the binding's column is a new item (a script statement),
not a continuation; and a Markdown item has one marker, while an F# item may
start with several keywords or none.

Challenge: the purpose rests on complexity living in `let` functions and
members and on CCN ranking them usefully. The strongest doubt is that
object-style F# keeps logic in properties (`member this.Status = match …`),
which this story does not report, so a complex getter stays invisible; the
story keeps the OCaml rule that bindings without arguments are values, and
records property bodies as the first follow-up. The large-`match` doubt from
the OCaml story applies unchanged and keeps the same answer.

Stress test: two inputs changed the promise and are now required behavior:
`#if … #else … #endif` inside a function leaked an `if` decision, and `when`
in a generic constraint counted as a guard. `]` at column 0 closing a
multi-line list and `|` arms at column 0 are handled by the bracket and
continuation rules. An unbalanced bracket folds the rest of the file into one
binding; the file would not compile, so no recovery is promised.

Architecture: no new consequential concern. The reader is one new module in
`lizard_languages/`, registered like the others (ADR-0001). The arm counting
and Pygments fragment joining the OCaml reader introduced now have two
consumers; the plan places each in one shared module rather than copying it,
with no shared ML-family base class.

## Selected Stories

<a id="fsharp-support"></a>

### 2. Support F# analysis

**Identity:** SEED-029#fsharp-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/021-fsharp-support/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"69461578ff42a4564b0dc55c589d1d2503603b2702d320705523dcd6e59dc048","plan":"c363c7006162e5e30f441119ac2b8c3d5b0f4d61ed8c6fc442f255fff02eef3d"}}
```

**Goal:** F# maintainers point Lizard at their F# sources through the existing
CLI and Python API and receive each named function binding and method with
CCN, NLOC, token count, and parameter count, so they can find the functions
that need review. This answers the F# half of GitHub issue #303 beside the
delivered OCaml support.

**Scope:**

Required behavior:

- `.fs` and `.fsx` files are discovered by default, selectable with
  `-l fsharp`, listed in the `-l` help, and analyzable through `analyze_file`
  and `analyze_source_code`. The README language list gains F# with a
  `docs/fsharp-analysis.md` page like the OCaml one.
- Binding boundaries follow F#'s light syntax (the offside rule). A binding
  starts at a `let`, `let rec`, `and`, `member`, `static member`, `override`,
  or `default` that is the first code token of its line with all brackets
  closed, and ends on the last code line before the next line whose first code
  token, with brackets closed, is at a column less than or equal to the
  binding's own column, or at end of file. Blank lines, comment lines, and
  lines that begin with `|` or an infix operator such as `|>` or `+` never end
  a binding (F# permits those undentations). Bindings written directly under a
  `type`, `module`, or `namespace` heading are bindings at their own deeper
  column, so nested modules and type members are found.
- Functions: a `let` binding is a function when it takes arguments or its body
  starts with `fun` or `function`; a member is a function when it has a
  parameter list, whether `()`, a tuple such as `(a: int, b: int)`, or curried
  arguments. Each is reported under its own name (`Increment`, `g`), not
  `Counter.Increment` or `Outer.Inner.g`, as the OCaml story decided for
  module bindings.
- Local `let` and `use` bindings, `fun` lambdas and `function` bodies belong to
  the enclosing binding: their lines, tokens and decisions count there and they
  are not reported separately.
- Not reported as functions: value bindings (`let x = …`, `let mutable c = …`,
  `let a, b = …`), properties and auto-properties (`member _.Value = …`,
  `member val Name = …`), `abstract` declarations, and `type`, `module`,
  `namespace`, `open`, `exception` items. Attributes `[<…>]` and `#` directive
  lines belong to no function.
- Decisions (CCN): `if`, `elif`, `while`, `for`, `&&`, `||`, and each `when`
  pattern guard add one. A `match … with`, `match! … with`, or `function` with
  n arms adds n − 1, the rule the OCaml story and Rust `match` use; or-patterns
  (`| 0 | 1 ->`) add nothing. Each handler arm of `try … with` adds one;
  `try … finally` adds nothing. Not decisions: `then`, `else`, `do`, the
  pipeline and bitwise operators `|>`, `<|`, `||>`, `|||`, union-case `|` in a
  type definition, `->` in lambdas and type annotations, the `with` of a record
  copy, type augmentation or auto-property, `when` inside a generic constraint
  (`<'T when 'T : comparison>`), and `:?` type tests.
- Parameters: each curried argument counts one, including annotated `(x: t)`;
  each element of a tuple parameter list counts one. `()` counts zero, the
  .NET reading an F# maintainer expects for `Increment()`; this differs from
  the OCaml reader, which counts unit as one argument. Generic parameter lists
  `<'T …>` and return-type annotations `: t` add nothing.
- Strings and comments: `(* … *)` including nested comments, `//` and `///`
  comments, `"…"` strings with escapes, verbatim `@"…"`, triple-quoted and
  interpolated `$"…"` strings, and `'|'` character literals introduce neither
  decisions nor functions; `'a` type variables do not disturb tokenization.
  Comment lines, including `///` doc comments, are excluded from NLOC.
- Scripts: `.fsx` files are analyzed by the same rules; `#r`, `#load`, `#if`,
  `#else` and `#endif` lines add no decisions and no functions, so a
  conditionally compiled branch inside a function counts only its own code.
  A module-level statement such as `printfn …` at the binding's column ends
  the binding like any other item.
- Existing language analysis is unchanged: the full suite stays green.

Deferred promises (not built or verified by this story):

- `.fsi` signature files (they hold no function bodies) and files that turn
  light syntax off (`#light "off"`, `#indent "off"`, `begin … end` with `in`).
- Properties with bodies (`member this.Status = match …`), explicit
  `with get() … and set v …` accessors, and secondary constructors `new(…)` as
  functions; their decisions fold into the enclosing scope. Reporting complex
  property bodies is a possible follow-up.
- Active patterns `let (|Even|Odd|) n = …` and operator definitions
  `let (+.) a b = …`; their decisions fold into the enclosing scope.
- Type-qualified names (`Counter.Increment`), interface implementations and
  object expressions beyond what the common rule yields.
- Re-tokenizing the expressions inside interpolated strings (they stay inert).
- Lines undented below their binding's column other than `|` and infix
  operators (compiler warning FS0058 territory), and tab indentation, which
  light syntax rejects.
- Modified CCN (`-m`) treating a whole `match` as one, extension metrics
  beyond the core four, and the legacy `or` / `&` operators.

Rejection constraints: none. No product requirement calls for rejecting any
F# input.

Boundary assumptions:

- F# 6 or later light syntax indented with spaces, as `dotnet fantomas` or
  hand formatting produces, in files that compile; Lizard depends on no .NET
  toolchain.
- Pygments' F# lexer, already a Lizard dependency, supplies tokens; the reader
  joins its comment and string fragments and its split operators.

**Key examples:**

1. **Discovery and selection.** A directory holds `src/Lib.fs`, `src/Lib.fsi`,
   `Script.fsx` and `tool.py` → `lizard .` reports the `.fs`, `.fsx` and
   Python functions; `lizard -l fsharp .` reports only the F# ones; nothing is
   reported from `Lib.fsi`; the `-l` help lists `fsharp`.
2. **Straight-line and branching.** `let rec fact n = if n <= 1 then 1 else n
   * fact (n - 1)`, `let greet (name: string) (punct: string) = printfn "%s%s"
   name punct` and `let g a b = a || b` → `fact` CCN 2 with 1 parameter;
   `greet` CCN 1 with 2 parameters; `g` CCN 2; each NLOC 1.
3. **Match arms, guards, `elif`.** `let classify x =` over `match x with | 0 |
   1 -> "small" | n when n < 0 -> "neg" | n -> if n > 9 && n < 100 then "mid"
   elif n = 5 then "five" else "big"` → `classify` CCN 7: two extra arms,
   `when`, `if`, `&&`, `elif`; the or-pattern adds nothing. `let f = function
   Some x -> x | None -> 0` → function `f`, CCN 2, 0 parameters.
4. **Pipelines and bitwise bars are not arms.** `let g a b = match a with |
   true -> [a; b] |> List.map not |> List.head | false -> (1 ||| 2) = 3 || not
   b` → CCN 3: one extra arm and `||`.
5. **Exceptions.** `let read k = try find k with | :? System.ArgumentException
   -> None | Failure _ -> None` laid out over lines → CCN 3: two handler arms.
   `let safe k = try find k finally close ()` → CCN 1.
6. **Locals and values stay inside.** `let sum l =` followed by indented
   `printfn "start"`, `let rec go acc = function | [] -> acc | x :: t -> go
   (acc + x) t`, and `go 0 (List.filter (fun x -> x > 0) l)`, then `let next =
   1`, `let a, b = 1, 2`, `let table =` with an indented `let build n = [1..n]`
   and `build 10`, then `let after x = x` → functions `sum` (CCN 2, lines 1–6)
   and `after` only; no `go`, `next`, `a`, `table`, `build`, or anonymous
   function.
7. **Members.** `type Counter(start: int) =` with indented `let mutable count =
   start`, `let bump n = if n > 0 then n + 1 else n`, `member
   this.Increment() =` over three lines, `member _.Value = count`, `member
   this.Add(a: int, b: int) = if a > b then a else b`, `static member
   Create() = Counter(0)`, `override x.ToString() = ""`, `member val Name = ""
   with get, set`, `abstract member Foo: int -> int`, `default this.Foo n =
   if n = 0 then 1 else n` → functions `bump` (CCN 2, 1 parameter),
   `Increment` (its three lines, 0 parameters), `Add` (CCN 2, 2 parameters),
   `Create` (0 parameters), `ToString`, `Foo` (CCN 2, 1 parameter, the
   `default` line only); no `count`, `Value`, `Name`, or abstract `Foo`. A
   union `type Shape = | Circle of float | Square of float` with members
   `member this.Area = 1.0` and `member this.Scale k = match this with Circle
   r -> Circle (r * k) | Square s -> Square (s * k)` → `Scale` CCN 2; no
   `Area`; the union-case bars add nothing.
8. **Nested modules and groups.** `module Outer =` holding `let f x = x`,
   `module Inner =` holding `let g y = if y then 1 else 2`, then `let h z =
   z` back at `Outer`'s item column, then `let top w = w` at column 0 →
   `f`, `g` (CCN 2), `h`, `top`, each ending on its own line. `let rec even n
   = if n = 0 then true else odd (n - 1)` / `and odd n = if n = 0 then false
   else even (n - 1)` → `even` and `odd`, each CCN 2.
9. **Comments, strings, attributes.** Three lines `(* if (* while *) || *)`,
   `/// doc comment with if`, `// line comment with match`, then
   `[<EntryPoint>]` and `let main argv =` whose body binds `"if \" then" +
   @"c:\if" + """x if y""" + $"v {argv} if"`, `'|'`, and `let t : 'a list =
   []`, then `let h y = y` → `main` CCN 1 starting at its `let` line with
   NLOC 5; file NLOC excludes the three comment lines; `h` is still found.
10. **Permitted undentation.** `let f xs = [` with elements on indented lines
    and `]` at column 0, then `let rec eval = function` with `| 0 -> 0` and
    `| n -> eval (n - 1)` at column 0, then `let k = 1` → `f` spans through
    the `]` line; `eval` spans its two arm lines with CCN 2.
11. **Scripts and directives.** `Script.fsx` with `#r "nuget: Foo"`, `#load
    "x.fsx"`, `let f x =` whose body is `#if DEBUG` / `if x then 1 else 2` /
    `#else` / `3` / `#endif`, then `let g y = y` and `printfn "%d" (f true)`
    → `f` CCN 2, not 3; `g` ends before the `printfn` line.
12. **Generics and computation expressions.** `let f<'T when 'T : comparison>
    (x: 'T) : 'T = x` → CCN 1 with 1 parameter. `let run xs = async {` with
    `match! fetch xs with | Some v -> return v | None -> return 0 }` → `run`
    CCN 2.
13. **Other languages unchanged.** The full test suite after the reader lands
    → green, with no change to any existing language's results.
