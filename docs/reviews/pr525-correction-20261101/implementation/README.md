# Slice 1 implementation evidence

Checkout: `/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries`.
Branch: `codex/correct-pr525-binding-boundaries`, starting HEAD
`1ba5cb5e619af3d04ad8eb8f4d3325c300660d75`. Admission:
`db54a2bab5fe28475982720f043479e99bcafc03`.
Identity: `SEED-023#correct-pr525-binding-boundaries`.

This return contains uncommitted implementation, tests, changelog and evidence.
The coordinator owns acceptance, independent refactor, full pytest/make,
performance qualification, contributor history, publication and merge.
No timing, commit, push, plan-status change or independent lint was performed.
The coordinator's PLAN.md/evidence edits are separate ownership.

## Implementation and metric contract

- `TypeScriptStates.next` and `_replay_token` dispatch an already-read token
  through `CodeStateMachine` without feeding its binding candidate twice.
- `JavaScriptBindings.in_expression` tolerates an exhausted frame stack;
  `${` opens an expression bracket using the existing bracket owner.
- A `]` returns a binding reader only when its actual frame stack is exhausted.
- Binding/object readers end an expression arrow at its enclosing comma, using
  `_expression_body`. Global arrows retain their current generic-call behavior.
- Ordinary/member/constructor/optional argument groups, including calls inside
  object values, share `_read_binding_candidate`; there is no parallel bracket
  counter, whole-stream buffer, generic parser or per-token wrapper.
- Object/function/signature mixins keep their responsibilities. ADR-0001,
  `docs/adrs/0001-ubiquitous-language-accepted.md`, remains the meaning of CCN,
  function boundaries, parameters and other public metrics. ADR-0000 and the ADR
  index were consulted; no architecture change or exception was needed.

## Discriminating baseline and corrections

The original diff was read with:

```sh
git diff 01afe9c228ba2a3dc5253259d8415d2ef47f0b71 63798156566ea4ab100048dc1eb21a7366cde380 --
git show 5f7d58fbad166bf05e1510162831bbc03cbabd96:docs/reviews/pr525-issue524-20261101/reproduce.py > /tmp/pr525-binding-reproduce.py
nix develop -c python /tmp/pr525-binding-reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --out docs/reviews/pr525-correction-20261101/implementation/current.json
nix develop -c python -m pytest test/test_languages/testJavaScriptBindingBoundaries.py -q > docs/reviews/pr525-correction-20261101/implementation/red.txt 2>&1
```

Before any production edit, normal public replay had 49/77 authored rows passing,
69/128 total, no crashes. Initial new tests produced exit 1: 74 failed, 56 passed.
Seven failures were an authoring mistake: `NS` was initially confused with
`nonstrict`. That mistake is retained in `red.txt` and `first-correction.txt`;
the final test separately asserts NS CCN 3/nesting 1 and nonstrict CCN 2.
The other 67 failures are valid red observations, including the callback at
lines 1–2/CCN 1 instead of lines 1–4/CCN 2 and object-arrow output order.

`argument-reader-current.json` uses the immutable frozen-current archive extracted
to `/tmp/pr525-binding-frozen-current`. It records member/constructor/optional
call ordering before the argument-reader correction. `argument-reader-red.txt`
then records 28 failing / 147 passing tests on the intermediate candidate,
before routing those groups through their binding owner.

`affected-initial.txt` records one introduced regression: a global expression
arrow with `foo<A,\n B>(x)` ended at line 1. The final restricted comma condition
preserves lines 1–2. Its observing assertions are the existing
`TestExpressionArrowContinuation.test_generic_arguments_split_after_comma_keep_their_line`
and new `test_global_arrow_generic_call_keeps_continued_expression`.

Normal and NS final public replays (`correction.json`, `correction-ns.json`)
each have 77/77 authored rows and 118/128 total passing, with no crashes.
`current-ns.json` retains the frozen-current NS baseline (49/77, 69/128).
The exact commands were:

```sh
nix develop -c python /tmp/pr525-binding-reproduce.py --checkout /tmp/pr525-binding-frozen-current --extension NS --out docs/reviews/pr525-correction-20261101/implementation/current-ns.json
nix develop -c python /tmp/pr525-binding-reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --out docs/reviews/pr525-correction-20261101/implementation/correction.json
nix develop -c python /tmp/pr525-binding-reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --extension NS --out docs/reviews/pr525-correction-20261101/implementation/correction-ns.json
```

## Terminal focused proof

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testArrowObjectBoundaries.py test/test_languages/testOptionalOperators.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testES6.py test/test_languages/testVue*.py -q > docs/reviews/pr525-correction-20261101/implementation/affected-final.txt 2>&1
```

Exit 0: **897 passed, 5 existing skips**. All affected language suites were
selected, not a `-k` subset. This includes reader hook/custom state dispatch,
formal slots/names/counts, function types, return annotations, generic calls,
ternary colons, templates/interpolation, lexical tokens, object/arrow grouping,
ASI and Vue source offsets. The five skips are existing platform/unsupported
cases, not newly suppressed proof.

```sh
nix develop -c python -m pytest test/test_languages/testJavaScriptBindingBoundaries.py test/testApplication.py test/test_options.py test/testLanguages.py test/test_analyzer.py test/testExtension.py test/testJavaScriptNestedStructures.py test/testNestedStructures.py test/testNestedStructureScopes.py test/test_extensions/testNonStrict.py test/test_extensions/testCognitiveComplexity.py test/test_extensions/testFanInFanOut.py test/test_extensions/testHalstead.py test/test_extensions/testHalsteadClassifier.py test/test_extensions/testHalsteadMetrics.py test/test_extensions/testDuplicatedParamList.py test/test_website.py -q > docs/reviews/pr525-correction-20261101/implementation/consumers-final.txt 2>&1
git diff --check
```

Both exit 0. Consumer pytest: **499 passed, 2 existing skips**. No required
verification command remains running. Full pytest/make and runtime measurement
are deliberately owned by the coordinator's next boundary.

The new cohesive module has 168 lines and 217 selected cases. Its setup is real
source passed to `analyze_file`/`FileAnalyzer`, a real temporary file for the CLI,
or a Flask test client reaching the real analyzer; no parsing mocks. Assertions:

| Promise | Observer in `test/test_languages/testJavaScriptBindingBoundaries.py` |
| --- | --- |
| Eleven authored cases, names/CCN/spans/formals, JS/TS/JSX/TSX/MJS/CJS/Vue | `AUTHORED`, `test_authored_binding_boundaries`, `summaries` |
| Callback lines 1–4/CCN 2; following `after(y)` lines 5–5/CCN 2/formal y; chained/optional calls | `test_callback_keeps_decisions_and_following_formals` |
| Object-array function and successor own their CCN/spans/formals | `test_object_array_keeps_decisions_and_following_formals` |
| Nested template substitutions preserve callbacks and members | `test_template_substitutions_keep_members_and_enclosing_callback` |
| Typed/generic/ternary arrows and signatures | `test_typed_generic_and_ternary_arrows_keep_signature_owners`, `test_global_arrow_generic_call_keeps_continued_expression` |
| Original crash examples/default initializers retain owners | `AUTHORED`, `test_default_initializer_and_comma_keep_expression_arrow_owners` |
| Ordinary/member/constructor/optional calls and calls in object values | `test_argument_readers_finish_arrow_before_next_object` |
| NS metric and nonstrict CCN | `test_metric_extensions_keep_callback_decisions` |
| Actual CLI rows: names/CCN/parameter count/spans | `test_cli_reports_complete_callback_and_following_metrics` |
| Actual HTTP JSON: names/CCN/parameter count/spans, all suffixes | `test_http_api_exposes_complete_callback_and_following_metrics` |

`summaries` additionally asserts parameter count equals the expected named
formal count; these examples have no empty destructuring slots. Existing formal
slot suites independently cover that distinction and downstream metric outputs.

## Consumer trace and remaining limitations

```sh
nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --base HEAD --term JavaScriptBindings --term TypeScriptStates --term expression_arrows --term max_nested_structures --term analyze_source_code > docs/reviews/pr525-correction-20261101/implementation/consumer-trace.md
nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --path lizard_languages/javascript_bindings.py --path lizard_languages/typescript_states.py --path lizard_languages/javascript_objects.py --term JavaScriptBindings --term TypeScriptStates --term expression_arrows --term max_nested_structures --term analyze_source_code > docs/reviews/pr525-correction-20261101/implementation/consumer-trace-product.md
```

Both exit 0. `consumer-dispositions.md` accounts for all 317 paths located by
either trace (38 run, 279 excluded with reasons). Actual HTTP `index.py` is
covered through its API and complete cheap suite. Historical review artifacts,
workflow guidance, unrelated language readers and incidental string matches
create no additional product promises.

The ten remaining replay mismatches are unchanged: label-body arrow omission in
seven suffixes and typed class-field phantom function `d` in TS/TSX/Vue. They
are not called passing. The former deep-object-array control now passes, but
no broader deep-object support is promised.

`generic-controls.json` compares frozen-current and corrected TS/TSX observations:
named/global generic arrows preserve lines 1–2; anonymous ordinary/member/
constructor call arrows with multiline generic arguments still end at line 1
in both revisions. This pre-existing generic limitation is explicitly uncorrected;
the scope does not introduce a generic parser. No new unresolved regression was
observed. No performance, full-suite, hosted-CI or publication pass is claimed.

Captured text logs are normalized only for trailing whitespace and final blank
lines under the Git whitespace gate. `raw-captured-logs.json` retains the exact
original bytes as JSON strings with SHA256 values; no failure/assertion content
was changed and no proof was rerun for this evidence encoding.
