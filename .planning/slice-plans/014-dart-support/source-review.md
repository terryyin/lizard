# Source review — SEED-021#dart-support

**Launch:** Fresh general-purpose reviewer (no conversation inheritance); inputs
were only the selected story's Goal, Scope, key examples, exclusions, rejection
constraints, and boundary assumptions from
`.planning/seeds/SEED-021-dart-support.md#dart-support`. No plan, return,
product, or proof was supplied.

**Reviewer return retained:** conversation agent
`c5ff4558-a658-416e-9038-ad0639861f8b` (numbered commitments 1–42, compound
ids, qualifications Q1–Q8, examples E1–E9, exclusions X1–X4).

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Dart and Flutter developers, and maintainers of mixed-language codebases, can point Lizard at Dart sources through the existing CLI and Python API. | slice 7 | |
| 2 | Each function and method is reported with CCN, NLOC, token count, and parameter count under a class-qualified name. | slice 7 | |
| 3 | That reporting lets them find methods that need review (e.g. oversized Flutter `build` methods). | slice 7 | |
| 4 | This closes the request behind issue #325 and extends Lizard's many-language promise to Dart. | slice 7 | |
| 5 | `.dart` files are discovered by default. | slice 1 | observed: testDart.py TestDartCliDiscovery.test_analyze_api_discovers_dart; test_language_filter_and_help default run |
| 6 | Dart is selectable with `-l dart`. | slice 1 | observed: testDart.py test_language_filter_and_help `-l dart` filters to Dart |
| 7 | `dart` is listed in the `-l` help. | slice 1 | observed: testDart.py test_language_filter_and_help `-h` contains `dart` |
| 8 | Dart is analyzable through `analyze_file`. | slice 1 | observed: dart_helpers via analyze_source_code path; lizard.analyze discovers `.dart` |
| 9 | Dart is analyzable through `analyze_source_code`. | slice 1 | observed: dart_helpers.get_dart_function_list → analyze_source_code("a.dart", …) |
| 10 | The README language list gains Dart. | slice 1 | observed: README.rst language list includes Dart |
| 11 | Top-level functions are recognized as functions. | slice 1 | observed: test_greet_branching_and_parameters; test_simple_add |
| 12.1 | Methods of classes are recognized as functions. | slice 1 | observed: test_widget_build_closures_counted_in_enclosing → MyWidget::build |
| 12.2 | Methods of mixins are recognized as functions. | slice 1 | observed: testDart.py mixin → M::foo |
| 12.3 | Methods of extensions are recognized as functions. | slice 1 | observed: testDart.py extension → twice |
| 12.4 | Methods of enums are recognized as functions. | slice 1 | observed: testDart.py enum → E::bar |
| 13 | Getters (`get name`) are recognized as functions. | slice 4 | observed: testDartExpressionBodies.py test_point_getters_setters_and_arrow_tostring → Point::sum, Point::isOrigin |
| 14 | Setters (`set name(v)`) are recognized as functions. | slice 4 | observed: testDartExpressionBodies.py test_point_getters_setters_and_arrow_tostring → Point::scale= |
| 15.1 | Unnamed constructors with a body are recognized as functions. | slice 5 | observed: testDartConstructors.py TestDartConstructors.test_unnamed_constructor_with_body → Point::Point |
| 15.2 | Named constructors with a body are recognized as functions. | slice 5 | observed: testDartConstructors.py test_named_constructor_with_body_and_initializing_formals → Point::checked |
| 15.3 | `factory` constructors with a body are recognized as functions. | slice 5 | observed: testDartConstructors.py test_point_class_story_constructors_and_members → Point::fromJson |
| 16 | Block bodies form a function. | slice 1 | observed: greet / add / MyWidget::build block bodies |
| 17 | `=>` expression bodies form a function. | slice 4 | observed: testDartExpressionBodies.py maybe; Point::sum / toString; twice; nested map arrow still one function |
| 18 | `async` between the parameter list and the body does not break recognition. | slice 7 | |
| 19 | `async*` between the parameter list and the body does not break recognition. | slice 7 | G6 |
| 20 | `sync*` between the parameter list and the body does not break recognition. | slice 7 | G6 |
| 21 | A declaration with no body is not a function. | slice 5 | observed: testDartConstructors.py test_bodyless_constructors_and_const_not_reported; test_point_class_story body-less not in names |
| 22 | Top-level functions are named `name`. | slice 1 | observed: greet, add |
| 23 | Members are named `Class::member`. | slice 1 | observed: MyWidget::build; M::foo; E::bar |
| 24 | Unnamed constructors are named `Class::Class`. | slice 5 | observed: testDartConstructors.py test_unnamed_constructor_with_body → Point::Point |
| 25 | Named and factory constructors are named `Class::constructorName`. | slice 5 | observed: testDartConstructors.py Point::checked; Point::fromJson |
| 26 | Getters are named `Class::getterName`. | slice 4 | observed: testDartExpressionBodies.py Point::sum, Point::isOrigin |
| 27 | Setters are named `Class::setterName=`. | slice 4 | observed: testDartExpressionBodies.py Point::scale= |
| 28.1 | `if` counts as a decision. | slice 1 | observed: greet CCN includes if; MyWidget::build |
| 28.2 | `for` counts as a decision. | slice 1 | observed: inherited CLikeStates (suite green; not Dart-specific fixture) |
| 28.3 | `while` (including `do … while`) counts as a decision. | slice 1 | observed: inherited CLikeStates |
| 28.4 | `case` counts as a decision. | slice 6 | |
| 28.5 | Each exception clause counts once (`on T`, `catch (e)`, or `on T catch (e)`). | slice 6 | G5 |
| 28.6 | `&&` counts as a decision. | slice 1 | observed: greet CCN 3 (if + &&) |
| 28.7 | `\|\|` counts as a decision. | slice 1 | observed: inherited CLikeStates logical operators |
| 28.8 | `??` counts as a decision. | slice 2 | observed: testDart.py test_null_coalescing_counts_once CCN 2 |
| 28.9 | `??=` counts as a decision. | slice 2 | observed: testDart.py test_null_aware_assignment_counts_once CCN 2 |
| 28.10 | The ternary `?` counts as a decision. | slice 1 | observed: MyWidget::build CCN 3 includes ternary |
| 29 | In a switch expression, each arm counts once except a bare `_` wildcard arm. | slice 6 | G5b |
| 30.1 | Nullable `T?` is not a decision. | slice 2 | observed: testDart.py nullable + ?? CCN 2; generic List int? CCN 1 |
| 30.2 | `?.` is not a decision. | slice 2 | observed: testDart.py test_null_aware_access_and_cascade_and_bang_add_nothing |
| 30.3 | `?..` is not a decision. | slice 2 | observed: testDart.py test_null_aware_access_and_cascade_and_bang_add_nothing |
| 30.4 | `!` is not a decision. | slice 2 | observed: testDart.py test_null_aware_access_and_cascade_and_bang_add_nothing |
| 30.5 | `is` is not a decision. | slice 1 | observed: inherited (not a control keyword in CLike) |
| 30.6 | `as` is not a decision. | slice 1 | observed: inherited |
| 30.7 | `await` is not a decision. | slice 1 | observed: inherited |
| 30.8 | `else` is not a decision. | slice 1 | observed: inherited CLikeStates |
| 30.9 | `default` is not a decision. | slice 6 | |
| 31.1 | `'…'` contents introduce neither decisions nor functions. | slice 1 | observed: greet string; MyWidget Text strings |
| 31.2 | `"…"` contents introduce neither decisions nor functions. | slice 1 | observed: greet / build samples |
| 31.3 | `'''…'''` contents introduce neither decisions nor functions. | slice 3 | observed: testDart.py test_raw_triple_quoted_strings_and_doc_comments_are_inert CCN 1 |
| 31.4 | `"""…"""` contents introduce neither decisions nor functions. | slice 3 | observed: testDart.py test_raw_triple_quoted_strings_and_doc_comments_are_inert CCN 1 |
| 31.5 | Raw `r'…'` contents introduce neither decisions nor functions. | slice 3 | observed: testDart.py test_raw_triple_quoted_strings_and_doc_comments_are_inert CCN 1 |
| 31.6 | `//` comment contents introduce neither decisions nor functions. | slice 1 | observed: CCppCommentsMixin via CLikeReader |
| 31.7 | `/* … */` contents introduce neither decisions nor functions. | slice 1 | observed: CCppCommentsMixin via CLikeReader |
| 31.8 | `///` doc comments introduce neither decisions nor functions. | slice 3 | observed: testDart.py test_raw_triple_quoted_strings_and_doc_comments_are_inert; `///` via CCppCommentsMixin |
| 32 | Comment lines are excluded from NLOC. | slice 3 | observed: testDart.py test_raw_triple_quoted_strings_and_doc_comments_are_inert NLOC 6 (doc line above f excluded) |
| 33 | A function literal's lines, tokens, and decisions belong to the enclosing function. | slice 1 | observed: test_widget_build_closures_counted_in_enclosing |
| 34 | Closures are not reported separately. | slice 1 | observed: no `(anonymous)` in build test |
| 35 | Each declared positional parameter counts as one parameter. | slice 1 | observed: add 2 params; build context |
| 36 | Each declared optional positional parameter in `[…]` counts as one parameter. | slice 2 | observed: testDart.py test_optional_positional_parameter_and_null_coalesce 2 params |
| 37 | Each declared named parameter in `{…}` counts as one parameter. | slice 1 | observed: greet 3 params (name + times + shout) |
| 38 | `this.x` initializing formals count as parameters. | slice 5 | observed: testDartConstructors.py test_named_constructor_with_body_and_initializing_formals / test_unnamed_constructor_with_body → 2 params |
| 39 | Defaults add nothing to the parameter count. | slice 1 | observed: greet shout = false still 3 params |
| 40 | `required` adds nothing to the parameter count. | slice 1 | observed: greet required times counted once |
| 41 | Existing language analysis is unchanged. | slice 1 | observed: full pytest suite 2169 passed, 7 skipped |
| 42 | The full suite stays green. | slice 1 | observed: full pytest suite 2169 passed, 7 skipped |
| X1 | Decisions inside string interpolation `${…}` are not counted (deferred). | | G7 |
| X2 | Reporting closures as separate `(anonymous)` functions is deferred. | | G8 |
| X3 | Extension metrics beyond the core four are not verified. | | G9 |
| X4 | `if (x case pattern)`, records, extension types, and other Dart 3 forms not in the key examples follow the common rule without their own verification. | | G10 |
