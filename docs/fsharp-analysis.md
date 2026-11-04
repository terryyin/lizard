# F# analysis

Lizard analyzes `.fs` and `.fsx` files, or `-l fsharp`. Signature files
(`.fsi`) and files that turn light syntax off are not analyzed. Pygments' F#
lexer provides the tokens, so no .NET toolchain is needed.

## Functions

Items follow F#'s light syntax (the offside rule). A line starts a new item
when its first code token is at or left of the current item's column and all
brackets are closed. Blank lines, comment lines, lines inside open brackets,
and lines that begin with `|` or an infix operator such as `|>` or `+` never
end an item. An item ends on the last code line before the next item, or at
the end of the file.

A `let` or `let rec` item is a function when it takes arguments, or when its
body starts with `fun` or `function`. The function has the binding's own name.
Value bindings such as `let x = …`, `let mutable c = …` and `let a, b = …` are
not reported. Any other item, such as an attribute line `[<EntryPoint>]` or a
module-level statement like `printfn …`, belongs to no function. An
attribute on the same line as its `let`, as in `[<Fact>] let t () = …`, also
belongs to no function: `t` is still reported, starting on that line, and the
attribute's tokens are not counted in it.

Local `let` bindings, `fun` lambdas and `function` bodies belong to the
function that encloses them: their lines, tokens and decisions count there,
and they are not reported separately.

## Complexity

`if`, `elif`, `while`, `for`, `&&` and `||` add one each. `then`, `else` and
the pipeline operators `|>` and `<|` add nothing.

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

- `match`, `function` and `try` arms and `when` guards are not counted yet.
- `member`, `override`, `default`, `and`, `type` and `module` lines are plain
  items: members, the `and` siblings of a `let rec … and …` group, and
  bindings indented under a `type`, `module` or `namespace` heading are not
  reported yet.
- `#if`, `#else`, `#endif`, `#r` and `#load` lines are not inert yet: a
  directive at a function's column ends the function, and the words on a
  directive line still count as decisions.
- Active patterns, operator definitions, properties and secondary
  constructors are not reported as functions.
- Interpolated string expressions are not analyzed, and lines undented below
  their binding's column, other than `|` and infix lines, are not supported.
  Indentation is assumed to use spaces.
