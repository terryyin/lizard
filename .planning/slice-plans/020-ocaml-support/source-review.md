# Source review: SEED-029#ocaml-support

**Launch:** fresh general-purpose sub-agent, no conversation inheritance, launched 2026-11-04 by the execution coordinator (agent Yui-chan) before slice 1 acceptance.
**Input:** only the story section `.planning/seeds/SEED-029-ocaml-fsharp-support.md#ocaml-support` text from **Goal** through key example 9 (unchanged since `ba8d367`), with instructions to read no project files, change nothing, and return numbered individual commitments, qualifications, examples and exclusions. No plan, return, product or proof was supplied.
**Return:** the ids and commitment texts below, verbatim from the reviewer's return.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| G | Goal: OCaml maintainers point Lizard at their OCaml sources through the existing CLI and Python API and receive each named function binding with CCN, NLOC, token count, and parameter count, so they can find the functions that need review (OCaml half of GitHub issue #303; extends the many-language promise to the ML family). | slice 3 |  |
| 1.1 | `.ml` files are discovered by default. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| 1.2 | OCaml is selectable with `-l ocaml`. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| 1.3 | `ocaml` is listed in the `-l` help. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| 1.4 | `.ml` source is analyzable through `analyze_file`. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| 1.5 | `.ml` source is analyzable through `analyze_source_code`. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters (analyze_source_code a.ml) |
| 1.6 | The README language list gains OCaml. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_readme_lists_ocaml; README.rst '-  OCaml' |
| 2.1 | Each top-level `let` binding that takes arguments is one function. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 2.2 | Each top-level `let rec` binding that takes arguments is one function. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 2.3 | Each top-level `let` / `let rec` binding whose body starts with `fun` is one function. | slice 1 | observed: test/test_languages/testOCaml.py::test_binding_whose_body_starts_with_fun_is_a_function: add reported, 0 params |
| 2.4 | Each top-level `let` / `let rec` binding whose body starts with `function` is one function. | slice 2 |  |
| 2.5 | Each `and` sibling of a `let rec … and …` group is one function. | slice 3 |  |
| 2.6 | Bindings inside `module Name = struct … end` are functions under the same rule. | slice 3 |  |
| 2.7 | A function ends at the next structure item. The listed structure items are: | slice 3 |  |
| 2.7.1 | a `let` in heading position | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 2.7.2 | an `and` in heading position | slice 3 |  |
| 2.7.3 | `type` | slice 3 |  |
| 2.7.4 | `module` | slice 3 |  |
| 2.7.5 | `open` | slice 3 |  |
| 2.7.6 | `include` | slice 3 |  |
| 2.7.7 | `exception` | slice 3 |  |
| 2.7.8 | `external` | slice 3 |  |
| 2.7.9 | `;;` | slice 3 |  |
| 2.8 | A function ends at its struct's `end`. | slice 3 |  |
| 2.9 | A function ends at end of file. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 2.10 | Qualification: a `let` is in heading position (outside brackets and `begin … end`) when the token before it ends an expression or is `;;`. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 2.11 | Qualification: a `let` after any of the following is a local `let … in`, not a heading: | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.11.1 | `=` | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 2.11.2 | `in` | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: 'in let c' local, f only + h |
| 2.11.3 | `->` | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.11.4 | `then` | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.11.5 | `else` | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.11.6 | `;` | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.11.7 | an operator | slice 1 | observed: test/test_languages/testOCaml.py::test_let_after_non_ending_tokens_is_local: lets after ; + then else -> at depth 0 local; exactly [(f,1,5),(g,6,6),(last,7,7)] |
| 2.12 | Qualification: a `let` inside brackets or inside `begin … end` is not in heading position. | slice 1 | observed: test/test_languages/testOCaml.py::test_let_inside_brackets_and_begin_end_is_local: exactly [(f,1,5),(last,6,6)] |
| 3.1 | The reported name is the binding's own name (e.g. `fact`; `g` inside `module M`). | slice 3 |  |
| 4.1 | `let … in` local bindings belong to the enclosing function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 4.2 | `fun` belongs to the enclosing function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside (fun x -> inside sum, no anonymous); test_local_let_and_fun_tokens_count_in_enclosing_function |
| 4.3 | `function` belongs to the enclosing function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 4.4 | Their lines count in the enclosing function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 4.5 | Their tokens count in the enclosing function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_let_and_fun_tokens_count_in_enclosing_function: f.token_count 14 |
| 4.6 | Their decisions count in the enclosing function. | slice 2 |  |
| 4.7 | They are not reported separately. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| 5.1.1 | `if` adds one. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 5.1.2 | `while` adds one. | slice 1 | observed: test/test_languages/testOCaml.py::test_loops_are_decisions_and_begin_end_is_not: while + for + begin/end gives CCN 3 |
| 5.1.3 | `for` adds one. | slice 1 | observed: test/test_languages/testOCaml.py::test_loops_are_decisions_and_begin_end_is_not: while + for + begin/end gives CCN 3 |
| 5.1.4 | `&&` adds one. | slice 2 |  |
| 5.1.5 | `\|\|` adds one. | slice 1 | observed: test/test_languages/testOCaml.py::test_or_operator_is_a_decision: g CCN 2 |
| 5.1.6 | Each `when` guard adds one. | slice 2 |  |
| 5.2 | A `match … with` with n arms adds n − 1. | slice 2 |  |
| 5.3 | A `function` with n arms adds n − 1. | slice 2 |  |
| 5.4 | Qualification: this is the same rule as Rust `match`, so a match and its equivalent `if … else` chain score alike. | slice 2 |  |
| 5.5 | Or-patterns (`\| A \| B ->`) add nothing. | slice 2 |  |
| 5.6 | Each handler arm of `try … with` adds one, as `catch` does elsewhere. | slice 2 |  |
| 5.7 | These are not decisions: | slice 2 |  |
| 5.7.1 | `else` | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 5.7.2 | `then` | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 5.7.3 | `begin` | slice 1 | observed: test/test_languages/testOCaml.py::test_loops_are_decisions_and_begin_end_is_not: while + for + begin/end gives CCN 3 |
| 5.7.4 | type-definition `\|` | slice 3 |  |
| 5.7.5 | `->` in `fun` | slice 2 |  |
| 5.7.6 | `->` in type annotations | slice 2 |  |
| 6.1 | Each curried argument before `=` counts as one parameter. This includes: | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 6.1.1 | labelled `~x` | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 6.1.2 | optional `?x` | slice 1 | observed: test/test_languages/testOCaml.py::test_optional_argument_counts_one_parameter: opt 2 params |
| 6.1.3 | optional with default `?(x = d)` | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 6.1.4 | annotated `(x : t)` | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: f (x : 'a list) 1 param |
| 6.1.5 | unit `()` | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| 7.1 | `(* … *)` comments introduce neither decisions nor functions. | slice 1 | observed: test/test_languages/testOCaml.py::test_comment_inside_function_body_is_inert: f CCN 1, NLOC 2, one function; test_comments_and_strings_are_inert |
| 7.2 | Nested comments introduce neither decisions nor functions. | slice 1 | observed: test/test_languages/testOCaml.py::test_comment_inside_function_body_is_inert: f CCN 1, NLOC 2, one function; test_comments_and_strings_are_inert |
| 7.3 | `"…"` strings, including escapes, introduce neither decisions nor functions. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| 7.4 | `'x'` char literals introduce neither decisions nor functions. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| 7.5 | `'a` type variables do not disturb tokenisation. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| 7.6 | Comment lines are excluded from NLOC. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| 8.1 | Existing language analysis is unchanged: the full suite stays green. | slice 3 |  |
| X1 | Value bindings without arguments are not reported as functions. The listed forms: |  | EX1 |
| X1.1 | `let x = …` |  | EX1 |
| X1.2 | `let () = …` |  | EX1 |
| X1.3 | `let _ = …` |  | EX1 |
| X1.4 | Reporting a complex `let () =` entry block is a possible follow-up. |  | EX1 |
| X2.1 | Module-qualified names (`M.g`) are not promised. |  | EX1 |
| X2.2 | Functors are not promised beyond what the common rule yields. |  | EX1 |
| X2.3 | First-class modules are not promised beyond what the common rule yields. |  | EX1 |
| X2.4 | Objects are not promised beyond what the common rule yields. |  | EX1 |
| X2.5 | `method`s are not promised beyond what the common rule yields. |  | EX1 |
| X3.1 | Local `let … in` helpers are not reported as separate functions. |  | EX1 |
| X3.2 | `fun` literals are not reported as separate functions. |  | EX1 |
| X4.1 | `.mli` signature files are excluded (they hold no function bodies). |  | EX1 |
| X4.2 | `.mll` generator sources are excluded. |  | EX1 |
| X4.3 | `.mly` generator sources are excluded. |  | EX1 |
| X4.4 | Reason syntax is excluded. |  | EX1 |
| X5.1 | `{\|…\|}` quoted string contents are not promised to be inert (an observed Pygments gap). |  | EX1 |
| X5.2 | `{id\|…\|id}` quoted string contents are not promised to be inert. |  | EX1 |
| X6 | Modified CCN (`-m`) treating a whole `match` as one is not promised; it does not reach Rust `match` today either. |  | EX1 |
| X7.1 | `match … with exception E ->` arms are excluded. |  | EX1 |
| X7.2 | The deprecated `or` operator is excluded. |  | EX1 |
| X7.3 | The deprecated `&` operator is excluded. |  | EX1 |
| X7.4 | Extension metrics beyond the core four are excluded. |  | EX1 |
| R1 | None. No product requirement calls for rejecting any OCaml input. |  | EX2 |
| B1 | Current OCaml 4.14/5.x syntax, as formatted by hand or by `ocamlformat`. | slice 3 |  |
| B2 | Lizard depends on no OCaml toolchain. | slice 3 |  |
| B3 | Pygments' OCaml lexer, already a Lizard dependency, supplies the tokens. | slice 3 |  |
| E1 | Directory with `src/main.ml`, `src/main.mli`, `tool.py`: | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| E1.1 | `lizard .` → reports the `.ml` functions and the Python ones. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| E1.2 | `lizard -l ocaml .` → reports only the `.ml` functions. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| E1.3 | The `-l` help → lists `ocaml`. | slice 1 | observed: test/test_languages/testOCaml.py::TestOCamlCliDiscovery.test_language_filter_and_help (default run lists double@ and tool_f@, -l ocaml only double, no main.mli, -h contains ocaml) |
| E2 | `let rec fact n = if n <= 1 then 1 else n * fact (n - 1)` followed by `let greet ~name ?(punct = "!") () = print_endline (name ^ punct)`: | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| E2.1 | `fact` → CCN 2, 1 parameter, NLOC 1. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| E2.2 | `greet` → CCN 1, 3 parameters, NLOC 1. | slice 1 | observed: test/test_languages/testOCaml.py::test_branching_and_curried_parameters: fact (CCN 2, 1 param, NLOC 1), greet (CCN 1, 3 params, NLOC 1) |
| E3.1 | `let classify x = match x with \| 0 \| 1 -> "small" \| n when n < 0 -> "neg" \| n -> if n > 9 && n < 100 then "mid" else "big"` → `classify` CCN 6 (two extra arms, `when`, `if`, `&&`); the or-pattern adds nothing. | slice 2 |  |
| E3.2 | `let f = function Some x -> x \| None -> 0` → function `f`, CCN 2, 0 parameters. | slice 2 |  |
| E4 | `let g a b = match a with \| true -> a \|\| b \| false -> not b` → CCN 3 (one extra arm and `\|\|`); `\|\|` is not an arm. | slice 2 |  |
| E5 | `let read k = try find k with Not_found -> None \| Invalid_argument _ -> None` → CCN 3 (two handler arms). | slice 2 |  |
| E6 | `let sum l = let rec go acc = function [] -> acc \| x :: t -> go (acc + x) t in go 0 (List.filter (fun x -> x > 0) l)` over several lines, followed by `let next = 1` and `let after x = x`: | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| E6.1 | → function `sum`, CCN 2, covering all its lines. | slice 2 |  |
| E6.2 | → function `after`. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| E6.3 | → no `go` function, no anonymous function, no `next` function. | slice 1 | observed: test/test_languages/testOCaml.py::test_local_helpers_and_lambdas_stay_inside: exactly [(sum,1,6),(after,8,8)] |
| E7.1 | `let rec even n = if n = 0 then true else odd (n - 1) and odd n = if n = 0 then false else even (n - 1)` → `even` and `odd`, each CCN 2. | slice 3 |  |
| E7.2 | `module M = struct let g a b = a && b \|\| not a end` followed by `type t = A \| B` and `let h x = x`: | slice 3 |  |
| E7.2.1 | → `g`, CCN 3, ending at `end`. | slice 3 |  |
| E7.2.2 | → `h`. | slice 3 |  |
| E7.2.3 | → the type's `\|` adds nothing to anything. | slice 3 |  |
| E8 | A function preceded by `(* if (* while *) \|\| *)`, whose body is `let s = "if \" then" in let c = '\|' in …`, with an annotation `(x : 'a list)`: | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| E8.1 | → that function CCN 1. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| E8.2 | → its comment line excluded from NLOC. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| E8.3 | → the next function still recognised. | slice 1 | observed: test/test_languages/testOCaml.py::test_comments_and_strings_are_inert: file NLOC 4 of 5 lines, f CCN 1 / 1 param, next function h found |
| E9 | The full test suite after the reader lands → green, with no change to any existing language's results. | slice 3 |  |
