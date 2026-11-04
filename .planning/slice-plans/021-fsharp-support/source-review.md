# Source review: F# language support

Launch: fresh general-purpose reviewer (no conversation inheritance), 2026-11-04,
during planned execution of SEED-029#fsharp-support. Input: only lines 149–319
of `.planning/seeds/SEED-029-ocaml-fsharp-support.md` (the `#fsharp-support`
story section: Goal, Scope, deferred promises, rejection constraints, boundary
assumptions, key examples); no human amendments; no plan, product or proof.
Instruction: read no other project files, change nothing; return numbered
individual commitments with compound items separated, exclusions marked, and
no slice allocation or acceptance judgment. Commitment text below is the
reviewer's return, lightly shortened where the source example text is long.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| G1 | F# maintainers point Lizard at their F# sources through the existing CLI and Python API. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help + test_analyze_file_reads_an_fs_file |
| G2 | They receive each named function binding and method, each with CCN, NLOC, token count and parameter count. | slice 3 | observed: all F# test modules: every function reports CCN, NLOC, token count and parameter count via analyze_source_code and the CLI table |
| G3 | Purpose: so they can find the functions that need review. | slice 3 | observed: all F# test modules: every function reports CCN, NLOC, token count and parameter count via analyze_source_code and the CLI table |
| G4 | This answers the F# half of GitHub issue #303, beside the OCaml support already delivered. | slice 3 | observed: lizard_languages/fsharp.py registered beside lizard_languages/ocaml.py; test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| 1.1 | `.fs` files are discovered by default. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| 1.2 | `.fsx` files are discovered by default. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| 1.3 | F# can be selected with `-l fsharp`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| 1.4 | `fsharp` is listed in the `-l` help. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| 1.5 | F# can be analyzed through `analyze_file`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_analyze_file_reads_an_fs_file |
| 1.6 | F# can be analyzed through `analyze_source_code`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition (analyze_source_code via fsharp_helpers) |
| 1.7 | The README language list gains F#. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_readme_and_docs_describe_fsharp |
| 1.8 | A `docs/fsharp-analysis.md` page is added, like the OCaml one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_readme_and_docs_describe_fsharp |
| 2.1 | A binding starts at a `let`, `let rec`, `and`, `member`, `static member`, `override` or `default` that is the first code token of its line, with all brackets closed. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets, test_attributed_member_and_static_let; test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_rec_and_group |
| 2.2 | A binding ends on the last code line before the next line whose first code token, with brackets closed, is at a column less than or equal to the binding's own column, or at end of file. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_locals_lambdas_and_values_stay_inside, test_module_level_statement_ends_a_binding |
| 2.3 | Blank lines never end a binding. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_blank_and_comment_lines_do_not_end_a_binding |
| 2.4 | Comment lines never end a binding. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_blank_and_comment_lines_do_not_end_a_binding |
| 2.5 | Lines that begin with `\|` or with an infix operator such as `\|>` or `+` never end a binding (F# permits those undentations). | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_bar_and_infix_lines_continue_a_binding, test_brackets_suspend_the_offside_rule |
| 2.6 | Bindings written directly under a `type`, `module` or `namespace` heading are bindings at their own deeper column, so nested modules and type members are found. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_nested_modules, test_and_continuing_a_type_group_is_a_type; test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| 3.1 | A `let` binding is a function when it takes arguments. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| 3.2 | A `let` binding is a function when its body starts with `fun` or `function`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_fun_and_function_bodies_are_functions |
| 3.3 | A member is a function when it has a parameter list: `()`, a tuple such as `(a: int, b: int)`, or curried arguments. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets (Increment/Create/ToString 0, Add 2, Foo 1) |
| 3.4 | Each function is reported under its own name (`Increment`, `g`), not a qualified one (`Counter.Increment`, `Outer.Inner.g`). | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets (bare Increment); test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_nested_modules (bare g) |
| 4.1 | Local `let` bindings belong to the enclosing binding (lines, tokens, decisions counted there; not reported separately). | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_decisions_of_locals_and_lambdas_count_in_the_binding |
| 4.2 | Local `use` bindings belong to the enclosing binding. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_declarations_are_not_functions (use r inside f, no r) |
| 4.3 | `fun` lambdas belong to the enclosing binding. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_decisions_of_locals_and_lambdas_count_in_the_binding |
| 4.4 | `function` bodies belong to the enclosing binding. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_decisions_of_locals_and_lambdas_count_in_the_binding |
| 5.1 | Value bindings (`let x = …`, `let mutable c = …`, `let a, b = …`) are not reported as functions. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_value_bindings_are_not_functions |
| 5.2 | Properties and auto-properties (`member _.Value = …`, `member val Name = …`) are not reported as functions. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets (no Value, Name), test_lambda_valued_member_is_a_property |
| 5.3 | `abstract` declarations are not reported as functions. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets (single Foo at the default line) |
| 5.4 | `type`, `module`, `namespace`, `open` and `exception` items are not reported as functions. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_declarations_are_not_functions, test_and_continuing_a_type_group_is_a_type (no B) |
| 5.5 | Attributes `[<…>]` belong to no function. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert, test_attribute_on_the_binding_line_belongs_to_no_function |
| 5.6 | `#` directive lines belong to no function. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| 6.1 | `if` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.2 | `elif` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.3 | `while` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.4 | `for` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.5 | `&&` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.6 | `\|\|` adds one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_each_control_flow_keyword_and_operator_adds_one |
| 6.7 | Each `when` pattern guard adds one. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_match_arms_guards_and_elif |
| 6.8 | A `match … with`, `match! … with` or `function` with n arms adds n − 1. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_match_arms_guards_and_elif, test_function_arms_on_one_line, test_match_bang_in_a_computation_expression |
| 6.9 | Or-patterns add nothing. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_or_patterns_add_nothing |
| 6.10 | Each handler arm of `try … with` adds one. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_try_handler_arms_each_add_one, test_single_try_handler_adds_one |
| 6.11 | `try … finally` adds nothing. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_try_handler_arms_each_add_one |
| 7.1 | `then`, `else` and `do` are not decisions. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_then_else_do_add_nothing |
| 7.2 | The pipeline and bitwise operators `\|>`, `<\|`, `\|\|>` and `\|\|\|` are not decisions. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_pipelines_and_bitwise_bars_are_not_arms, test_other_pipelines_add_nothing |
| 7.3 | Union-case `\|` in a type definition is not a decision. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_union_members_and_case_bars |
| 7.4 | `->` in lambdas and in type annotations is not a decision. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_constraint_when_and_type_arrows_add_nothing |
| 7.5 | The `with` of a record copy, a type augmentation or an auto-property is not a decision. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_record_copy_with_and_type_tests_add_nothing, test_with_of_augmentation_and_auto_property_starts_no_arms |
| 7.6 | `when` inside a generic constraint is not a decision. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_constraint_when_and_type_arrows_add_nothing |
| 7.7 | `:?` type tests are not decisions. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_record_copy_with_and_type_tests_add_nothing, test_try_handler_arms_each_add_one |
| 8.1 | Each curried argument counts one, including an annotated `(x: t)`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| 8.2 | Each element of a tuple parameter list counts one. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_tuple_elements_each_count_one |
| 8.3 | `()` counts zero (differs from the OCaml reader). | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_unit_parameter_counts_zero |
| 8.4 | Generic parameter lists `<'T …>` add nothing. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_constraint_when_and_type_arrows_add_nothing (E12 f 1 param) |
| 8.5 | Return-type annotations `: t` add nothing. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_return_annotation_and_generics_add_no_parameters |
| 9.1 | `(* … *)` comments, including nested comments, introduce neither decisions nor functions. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.2 | `//` comments are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.3 | `///` comments are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.4 | `"…"` strings with escapes are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.5 | Verbatim `@"…"` strings are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.6 | Triple-quoted strings are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.7 | Interpolated `$"…"` strings are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.8 | `'\|'` character literals are inert. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.9 | `'a` type variables do not disturb tokenization. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| 9.10 | Comment lines are excluded from NLOC, including `///` doc comments. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comment_lines_are_out_of_file_nloc |
| 10.1 | `.fsx` files are analyzed by the same rules. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert (Script.fsx); test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help (Script.fsx f@3-7, g@9-9) |
| 10.2 | `#r`, `#load`, `#if`, `#else` and `#endif` lines add no decisions. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| 10.3 | Those directive lines add no functions. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| 10.4 | A conditionally compiled branch inside a function counts only its own code. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| 10.5 | A module-level statement such as `printfn …` at the binding's column ends the binding. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert (g ends before printfn); test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_module_level_statement_ends_a_binding |
| 11 | Existing language analysis is unchanged: the full suite stays green. | slice 3 | observed: nix develop -c make: 3009 passed, 7 skipped (full suite incl. all other languages) |
| X1 | EXCLUSION: `.fsi` signature files. |  | O1 |
| X2 | EXCLUSION: Files that turn light syntax off. |  | O1 |
| X3 | EXCLUSION: Properties with bodies reported as functions. |  | O1 |
| X4 | EXCLUSION: Explicit `with get() … and set v …` accessors reported as functions. |  | O1 |
| X5 | EXCLUSION: Secondary constructors `new(…)` reported as functions. |  | O1 |
| X6 | Note: reporting complex property bodies is a possible follow-up. |  | O1 |
| X7 | EXCLUSION: Active patterns. |  | O1 |
| X8 | EXCLUSION: Operator definitions. |  | O1 |
| X9 | EXCLUSION: Type-qualified names. |  | O1 |
| X10 | EXCLUSION: Interface implementations beyond the common rule. |  | O1 |
| X11 | EXCLUSION: Object expressions beyond the common rule. |  | O1 |
| X12 | EXCLUSION: Re-tokenizing expressions inside interpolated strings. |  | O1 |
| X13 | EXCLUSION: Undentation other than `\|` and infix operators. |  | O1 |
| X14 | EXCLUSION: Tab indentation. |  | O1 |
| X15 | EXCLUSION: Modified CCN (`-m`) treating a whole `match` as one. |  | O1 |
| X16 | EXCLUSION: Extension metrics beyond the core four. |  | O1 |
| X17 | EXCLUSION: Legacy `or` and `&` operators. |  | O1 |
| R0 | No rejection constraints. | slice 3 | observed: no F# input is rejected: reader has no error path (lizard_languages/fsharp.py) |
| B1 | Input is F# 6+ light syntax indented with spaces. | slice 1 | observed: test/test_languages/testFSharp.py::all slice-1 fixtures are space-indented compilable light syntax |
| B2 | Input files compile. | slice 1 | observed: test/test_languages/testFSharp.py::all slice-1 fixtures are space-indented compilable light syntax |
| B3 | Lizard depends on no .NET toolchain. | slice 1 | observed: test/test_languages/testFSharp.py::lizard_languages/fsharp_tokens.py imports only pygments (no .NET) |
| B4 | Pygments' F# lexer supplies the tokens. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| B5 | The reader joins the lexer's comment and string fragments and its split operators. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| E1.1 | `lizard .` reports the `.fs`, `.fsx` and Python functions. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| E1.2 | `lizard -l fsharp .` reports only the F# ones. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| E1.3 | Nothing is reported from `Lib.fsi`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| E1.4 | The `-l` help lists `fsharp`. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCliDiscovery.test_language_filter_and_help |
| E2.1 | `fact` has CCN 2. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E2.2 | `fact` has 1 parameter. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E2.3 | `greet` has CCN 1. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E2.4 | `greet` has 2 parameters. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E2.5 | `g` has CCN 2. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E2.6 | Each function has NLOC 1. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpFunctionRecognition.test_straight_line_and_branching |
| E3.1 | `classify` has CCN 7: two extra arms, `when`, `if`, `&&` and `elif`. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_match_arms_guards_and_elif |
| E3.2 | The or-pattern adds nothing. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_match_arms_guards_and_elif |
| E3.3 | `let f = function Some x -> x \| None -> 0` gives a function `f`. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_function_arms_on_one_line |
| E3.4 | That `f` has CCN 2. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_function_arms_on_one_line |
| E3.5 | That `f` has 0 parameters. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_function_arms_on_one_line |
| E4.1 | Pipelines and bitwise bars example: CCN 3. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_pipelines_and_bitwise_bars_are_not_arms |
| E5.1 | `read` with two `try` handler arms has CCN 3. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_try_handler_arms_each_add_one |
| E5.2 | `safe` with `try … finally` has CCN 1. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_try_handler_arms_each_add_one |
| E6.1 | The only functions are `sum` and `after`. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_locals_lambdas_and_values_stay_inside |
| E6.2 | `sum` has CCN 2. | slice 2 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_locals_lambdas_and_values_stay_inside |
| E6.3 | `sum` spans lines 1–6. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_locals_lambdas_and_values_stay_inside |
| E6.4 | No `go`, `next`, `a`, `table`, `build` or anonymous function is reported. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_locals_lambdas_and_values_stay_inside |
| E7.1 | `bump` has CCN 2 and 1 parameter. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.2 | `Increment` spans its three lines and has 0 parameters. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.3 | `Add` has CCN 2 and 2 parameters. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.4 | `Create` has 0 parameters. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.5 | `ToString` is reported as a function. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.6 | `Foo` has CCN 2 and 1 parameter, and covers only the `default` line. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.7 | No `count`, `Value`, `Name` or abstract `Foo` is reported. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_type_members_and_type_level_lets |
| E7.8 | Union `Shape` member `Scale` has CCN 2. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_union_members_and_case_bars |
| E7.9 | No `Area` is reported. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_union_members_and_case_bars |
| E7.10 | The union-case bars add nothing. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpMembers.test_union_members_and_case_bars |
| E8.1 | Functions `f`, `g`, `h` and `top` are reported. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_nested_modules |
| E8.2 | `g` has CCN 2. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_nested_modules |
| E8.3 | Each of `f`, `g`, `h` and `top` ends on its own line. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_nested_modules |
| E8.4 | `let rec even … and odd …` gives `even` and `odd`. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_rec_and_group |
| E8.5 | `even` and `odd` each have CCN 2. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpNestedItems.test_rec_and_group |
| E9.1 | `main` has CCN 1. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| E9.2 | `main` starts at its `let` line, not at the attribute. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| E9.3 | `main` has NLOC 5. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| E9.4 | File NLOC excludes the three comment lines. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comment_lines_are_out_of_file_nloc |
| E9.5 | `h` is still found. | slice 1 | observed: test/test_languages/testFSharp.py::TestFSharpCommentsAndStrings.test_comments_strings_and_attributes_are_inert |
| E10.1 | `f` spans through the `]` line. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_brackets_suspend_the_offside_rule |
| E10.2 | `eval` spans its two arm lines. | slice 1 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_brackets_suspend_the_offside_rule |
| E10.3 | `eval` has CCN 2. | slice 2 | observed: test/test_languages/testFSharpItemBoundaries.py::TestFSharpItemBoundaries.test_brackets_suspend_the_offside_rule |
| E11.1 | `f` has CCN 2, not 3. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| E11.2 | `g` ends before the `printfn` line. | slice 3 | observed: test/test_languages/testFSharpMembers.py::TestFSharpDirectives.test_directive_lines_are_inert |
| E12.1 | `let f<'T when 'T : comparison> (x: 'T) : 'T = x` has CCN 1. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_constraint_when_and_type_arrows_add_nothing |
| E12.2 | That `f` has 1 parameter. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_constraint_when_and_type_arrows_add_nothing |
| E12.3 | `run` with `match!` has CCN 2. | slice 2 | observed: test/test_languages/testFSharpArmDecisions.py::TestFSharpArmDecisions.test_match_bang_in_a_computation_expression |
| E13.1 | After the reader lands, the full test suite is green. | slice 3 | observed: nix develop -c make: 3009 passed, 7 skipped (full suite incl. all other languages) |
| E13.2 | No existing language's results change. | slice 3 | observed: nix develop -c make: 3009 passed, 7 skipped (full suite incl. all other languages) |
