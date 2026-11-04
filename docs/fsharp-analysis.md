# F# analysis

Lizard analyzes `.fs` and `.fsx` files, or `-l fsharp`. Signature files
(`.fsi`) are not analyzed, and files that turn light syntax off are not
supported. Pygments' F# lexer provides the tokens, so no .NET toolchain is
needed.

## Functions

Items follow F#'s light syntax (the offside rule). A line starts a new item
when its first code token is at or left of the current item's column and all
brackets are closed. Blank lines, comment lines, lines inside open brackets,
and lines that begin with `|` or an infix operator such as `|>` or `+` never
end an item. An item ends on the last code line before the next item, or at
the end of the file. Under a `type`, `module` or `namespace` heading, or an
`and` that continues a `type` group (`type A() = …` / `and B() = …`), a deeper
line starts a nested item at its own column, so type members and the bindings
of nested modules are found; anywhere else a deeper line belongs to the item
above it.

A binding starts with `let`, `let rec`, `and`, `member`, `static member`,
`override` or `default`, including after an attribute on the same line, as in
`[<Fact>] member _.Works() = …`. A `let` or `and` binding is a function when
it takes arguments, or when its body starts with `fun` or `function`. A member
is a function when it has a parameter list: `()`, a tuple such as
`(a: int, b: int)`, or curried arguments. Each function has its own name:
`member this.Increment() = …` is `Increment`, and `g` in a nested module is
`g`, not `Outer.Inner.g`.

Not reported as functions: value bindings such as `let x = …`,
`let mutable c = …` and `let a, b = …`; properties and auto-properties such
as `member _.Value = …` and `member val Name = "" with get, set`; `abstract`
declarations; `type`, `module`, `namespace`, `open` and `exception` items;
module-level statements such as `printfn …`. An attribute line such as
`[<EntryPoint>]` belongs to no function, and an attribute on the binding's
line is not counted in the function.

Local `let` and `use` bindings, `fun` lambdas and `function` bodies belong to
the function that encloses them: their lines, tokens and decisions count
there, and they are not reported separately.

## Scripts and directives

`.fsx` scripts follow the same rules. `#` directive lines such as `#r`,
`#load`, `#if`, `#else` and `#endif` are dropped before analysis: they add no
decisions and no lines, belong to no function, and do not end one even at
column 0. A conditionally compiled branch inside a function counts only its
own code.

## Complexity

`if`, `elif`, `while`, `for`, `&&` and `||` add one each.

A `match … with`, `match! … with` or `function` with n arms adds n − 1: the
first arm is the default path. Each handler arm of `try … with` adds one, and
`try … finally` adds nothing. Each `when` guard in an arm's pattern adds one.
An or-pattern such as `| 0 | 1 -> …` is one arm and adds nothing beyond it.
Arms are counted per bracket level, so a `function` inside a lambda's
parentheses counts its own arms. For example,

```fsharp
let classify x =
    match x with
    | 0 | 1 -> "small"
    | n when n < 0 -> "neg"
    | n -> if n > 9 && n < 100 then "mid" elif n = 5 then "five" else "big"
```

has CCN 7: two extra arms, the `when` guard, `if`, `&&` and `elif`.

Not decisions: `then`, `else`, `do`; the operators `|>`, `<|`, `||>` and
`|||`; `->` in lambdas and type annotations; the `with` of a record copy
`{ r with X = 1 }`, a type augmentation or an auto-property; `when` in a
generic constraint `<'T when 'T : comparison>`; `:?` type tests; and the
case bars of a union type definition.

## Parameters

Each curried argument before `=` is one parameter, including an annotated
`(x: t)`. Each element of a tuple such as `(a: int, b: int)` is one parameter.
`()` counts zero, unlike the OCaml reader. Generic parameter lists `<'T …>`
and a return-type annotation `: t` add nothing. For example,
`let greet (name: string) (punct: string) = …` has two parameters.

## Comments and strings

`(* … *)` comments, including nested ones, `//` and `///` comments, `"…"`
strings with escapes, verbatim `@"…"`, triple-quoted `"""…"""` and
interpolated `$"…"` strings, and `'|'` character literals add no decisions
and no functions. `'a` type variables do not disturb tokenization. Comment
lines, including `///` doc comments, are excluded from NLOC.

## Limitations

- Signature files (`.fsi`) are not analyzed, and files that turn light
  syntax off (`#light "off"`, `begin … end` with `in`) are not supported.
- Properties with bodies, explicit `with get() … and set v …` accessors and
  secondary constructors `new(…)` are not reported; their decisions count in
  the enclosing item.
- Active patterns `let (|Even|Odd|) n = …` and operator definitions
  `let (+.) a b = …` are not reported; their decisions count in the
  enclosing item.
- Names are not qualified by their type or module (`Counter.Increment`), and
  members inside interface implementations and object expressions are not
  reported separately.
- Expressions inside interpolated strings are not analyzed.
- Lines undented below their binding's column, other than `|` and infix
  lines, are not supported, and indentation is assumed to use spaces, not
  tabs.
- Modified CCN (`-m`) does not treat a `match` as one decision, and the
  legacy `or` / `&` operators are not counted.
