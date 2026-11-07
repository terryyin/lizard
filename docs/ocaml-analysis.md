# OCaml analysis

Lizard analyzes `.ml` files, or `-l ocaml`. Signature files (`.mli`),
`ocamllex` / `ocamlyacc` sources (`.mll`, `.mly`) and Reason syntax are not
analyzed. Pygments' OCaml lexer provides the tokens, so no OCaml toolchain is
needed.

## Functions

A top-level `let` or `let rec` binding is a function when it takes arguments,
or when its body starts with `fun` or `function`. The function has the
binding's own name. Each `and` sibling of a `let rec … and …` group is a
separate function under the same rule. Bindings inside `module Name = struct …
end` are functions too, reported under their own name rather than `M.g`.
Value bindings such as `let x = …`, `let () = …` and `let _ = …` are not
reported.

A function ends at the next structure item, at its struct's `end`, or at the
end of the file. The structure items are a heading `let` or `and`, `type`,
`module`, `open`, `include`, `exception`, `external` and `;;`. A function
that ends at `end` includes that line; any other function ends on the line
before the next item.

A `let` is in heading position when it is outside brackets and `begin … end`,
and the token before it ends an expression or is `;;`. A `let` that follows
`=`, `in`, `->`, `then`, `else`, `;` or an operator is a local `let … in`.
Local `let … in` bindings, `fun` and `function` belong to the function that
encloses them: their lines, tokens and decisions count there, and they are not
reported separately. The same applies to a local `let module … in`.

## Complexity

`if`, `while`, `for`, `&&`, `||` and each `when` guard add one. A `match … with`
or `function` with n arms adds n − 1, the same rule Lizard uses for Rust
`match`. A match and its equivalent `if … else` chain therefore score the
same. An or-pattern such as `| A | B ->` adds nothing. Each handler arm of
`try … with` adds one.

The following are not decisions: `else`, `then`, `begin`, the `|` of a type
definition, `->` in `fun` or in a type annotation, and the `with` of a record
update.

```ocaml
let classify x = match x with
  | 0 | 1 -> "small"
  | n when n < 0 -> "neg"
  | n -> if n > 9 && n < 100 then "mid" else "big"
```

`classify` has CCN 6: two extra arms, `when`, `if` and `&&`.

## Parameters

Each curried argument before `=` is one parameter. This includes labelled
`~x`, optional `?x` and `?(x = d)`, annotated `(x : t)` and unit `()`. For
example, `let greet ~name ?(punct = "!") () = …` has three parameters.

## Comments and strings

`(* … *)` comments, including nested ones, `"…"` strings with escapes, and
`'x'` character literals add no decisions and no functions. `'a` type
variables do not disturb tokenization. Comment lines are excluded from NLOC.

## Limitations

- The contents of `{|…|}` and `{id|…|id}` quoted strings are not guaranteed to
  be inert.
- `class` and `object` items are not structure-item boundaries, so a function
  just before a class absorbs the class's lines.
- Functors, first-class modules, objects and `method`s are only handled as far
  as the rules above reach.
- `match … with exception E ->` arms and the deprecated `or` and `&` operators
  are not counted.
- Modified CCN (`-m`) does not treat a whole `match` as one decision.
