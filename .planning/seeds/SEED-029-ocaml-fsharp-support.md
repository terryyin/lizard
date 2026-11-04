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
