# Slice 2 return for G7: proven call groups

Identity: `SEED-023#correct-pr525-binding-boundaries`.
Checkout: `/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries`.
Branch: `codex/correct-pr525-binding-boundaries`; HEAD remains
`4a2081598ac8d70debafe64d672d65fd05857944`.

This continues the same slice after the coordinator's strict paired argument
workload failure: slowdown 5.5544%, 95% interval [5.0636%, 6.0476%]. The prior
full API/NS passes do not cancel that failure. All `cost/`, `refactor2/`,
`verification/` and `measurements/attempt1/` evidence is untouched. G7 still
requires the coordinator's new paired qualification; this return makes no
slowdown verdict.

## Entry, immutable inputs and profile decision

The coordinator supplied exclusive foreground Nix ownership. No other owned
foreground or heavy verification was running. Entry host `Territory.local` has
16 logical CPUs; load was 9.05/9.77/9.65, with 1,240,047 inactive pages,
118,553 free pages, zero throttled pages and zero swap-ins/outs. Unknown
unrelated work was left running. `entry.json` retains the actual host/memory
observation and entry production hashes.

The bounded decision was one serial profile at a time, 40 repeats of the frozen
40-file `argument-consumers` corpus, observing public analysis followed by
`parameters` and `parameter_count`. Each command completed in under one second.
These diagnostic profiles observe call work, not qualified timing. The fixed
small workload remained applicable for the post-edit profile. Failures,
contention or changed production/inputs would require reconsideration before
another expensive attempt. The coordinator continues to own all paired timing.

Before any production edit, all 40 input hashes matched `argument-inputs.json`.
The new pre-edit profile's entire production hash map also equals the
coordinator's `evidence/final-profile.json` map, tying it to the failed candidate.
The four `*.before.py` files retain that actual entry state, including the
independent refactor's consolidated positive formal-eligibility branch.

```sh
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/.cache/pr525-correction-qualified-20261101/current --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/argument-consumers --out docs/reviews/pr525-correction-20261101/cost-return/current-arguments-profile.json --repeat 40
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/argument-consumers --out docs/reviews/pr525-correction-20261101/cost-return/pre-edit-arguments-profile.json --repeat 40
```

Both exited 0 before edits. The harness and all corpus selections were reused
unchanged. Final correspondence verifies the original 131 affected/45 parent-safe/
100 broad sets, complete 171/85 qualification sets, argument 40, frozen current
and original-parent source exports and their archives still match their manifests.
`final-correspondence.json` records every check and the manifest/harness hashes.

## Change and causal evidence

The failed candidate still gave proven call groups a full `JavaScriptBindings`
tracker even after suppressing provisional parameter capture. The subgroup
profile shows 87,360 binding-token reads versus frozen current's 62,240.

`JavaScriptFunctionsMixin._read_parenthesis_group` now evaluates the existing
`parameter_group` eligibility once for routing. Possible arrow/signature formals
retain the existing binding candidate. Proven calls reuse the ordinary nested
syntax reader, with explicit `_call_group` context so their argument commas
close expression arrows before subsequent arguments. Objects use the same helper.
The redundant `track_parameters` option is removed: binding candidates are again
the actual/provisional binding owner. No per-token wrapper, parallel bracket
counter, buffering layer, PR-specific switch or new syntax parser was added.

The independent refactor's positive formal branch is preserved.
`typescript_states.py` is **245 lines**, within its 250-line limit. Existing
`__call__`/replay behavior, root/custom-reader protocols, named/anonymous/typed/
destructured/default/ternary formals and metric meanings retain their owners.
ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) remains the naming and
metric contract; no architectural boundary or exception changes.

| Signal, 40 sources × 40 repeats | Frozen current | Failed candidate at entry | New candidate |
| --- | ---: | ---: | ---: |
| Binding `read_token` calls | 62,240 | 87,360 | 52,160 |
| `_read_binding_candidate` calls | 5,760 | 7,200 | 4,000 |
| `JavaScriptBindings.__init__` calls | 8,960 | 10,400 | 7,200 |
| `BindingFrame.__init__` calls | 9,120 | 12,000 | 7,200 |
| `_read_parameter` calls | 9,760 | 6,400 | 6,400 |
| `CodeStateMachine.__call__` total calls | 168,800 | 168,800 | 168,800 |
| Binding `read_token` cumulative seconds | 0.04368 | 0.05322 | 0.03640 |
| Total cProfile seconds | 0.65898 | 0.67887 | 0.62267 |

This establishes removal of 3,200 unnecessary binding-candidate groups and
35,200 binding-token reads with unchanged provisional-formal reads and total
syntax-dispatch calls. The unpaired instrumented seconds do not establish a
3% acceptance bound. `profile-comparison.json` retains the causal counts/times;
full profiles retain Python/import locations, production hashes and source rows.

```sh
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/argument-consumers --out docs/reviews/pr525-correction-20261101/cost-return/post-edit-arguments-profile.json --repeat 40
```

Exit 0. No production edit followed this profile. `candidate-hashes.json` and
`final-correspondence.json` match its complete production hash map. Changes from
the refactored entry state are retained exactly in `changes-from-entry.json`.
The initial progress message's 5,280 candidate count was a transcription error;
the retained profile and the correct table above report 4,000.

## Terminal proof, assertions and setup

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testArrowObjectBoundaries.py test/test_languages/testOptionalOperators.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testES6.py test/test_languages/testVue*.py -q > docs/reviews/pr525-correction-20261101/cost-return/affected-final.txt 2>&1
```

Exit 0: **897 passed, 5 existing skips**, complete modules without a `-k` filter.
The first implementation also passed this command at output `affected-first.txt`;
the final command follows the final whitespace/readability edits and profile.
All setup below uses real sources through `analyze_file`/`FileAnalyzer`, with no
parsing mocks. Observing assertions:

- `testJavaScriptBindingBoundaries.py::test_argument_readers_finish_arrow_before_next_object`
  asserts the anonymous argument, defaulted object arrow and successor
  names/CCN/spans/formals for ordinary/member/constructor/optional calls in both
  statement and object contexts across all seven suffixes. This observes the
  new ordinary call-reader comma context at the public analyzer boundary.
- That module's `test_authored_binding_boundaries`,
  `test_callback_keeps_decisions_and_following_formals`,
  `test_object_array_keeps_decisions_and_following_formals` and
  `test_template_substitutions_keep_members_and_enclosing_callback` assert the
  eleven original examples, nested subscript/template owners, callback CCN/spans
  and following functions/formals.
- `test_default_initializer_and_comma_keep_expression_arrow_owners` asserts
  default initializer and call CCN 5, names `x,y`, object-member `z` and
  successor `tail`; `test_typed_generic_and_ternary_arrows_keep_signature_owners`
  asserts typed/generic/ternary formals and successor `z`; the global generic
  continuation assertion retains lines 1–2.
- `testJavaScriptArrowParameters.py::test_anonymous_and_named_bindings_agree`,
  `test_reported_arrow_forms`, `test_defaults_and_wrapper_call` and
  `testJavaScriptFormalSlots.py::test_named_and_anonymous_empty_patterns_occupy_slots`,
  `test_nested_and_ternary_defaults_keep_complexity_owners`,
  `test_generic_and_object_return_annotations_preserve_formals` assert names,
  parameter slots/counts and default/ternary complexity through real readers.
- `testTypeScriptReaderDispatch.py` asserts custom/inherited/instance hooks,
  emitted tokens, parallel-state/EOF behavior and configured root binding
  defaults/previous token. `testVueSourceLineNumbers.py` observes wrapper offsets.
- Actual CSV/checkstyle/HTML parameter values, warning filtering and normalized
  duplicate-list keys are asserted in `testJavaScriptArrowParameters.py` and
  `testJavaScriptFormalSlots.py`, using real analyzed functions and formatters.

```sh
nix develop -c python -m pytest test/test_languages/testJavaScriptBindingBoundaries.py test/testApplication.py test/test_options.py test/testLanguages.py test/test_analyzer.py test/testExtension.py test/testJavaScriptNestedStructures.py test/testNestedStructures.py test/testNestedStructureScopes.py test/test_extensions/testNonStrict.py test/test_extensions/testCognitiveComplexity.py test/test_extensions/testFanInFanOut.py test/test_extensions/testHalstead.py test/test_extensions/testHalsteadClassifier.py test/test_extensions/testHalsteadMetrics.py test/test_extensions/testDuplicatedParamList.py test/test_website.py -q > docs/reviews/pr525-correction-20261101/cost-return/consumers-final.txt 2>&1
```

Exit 0: **499 passed, 2 existing skips** on the same final production bytes.
`test_metric_extensions_keep_callback_decisions` sets up real sources with
`FileAnalyzer(get_extensions(...))` and asserts NS CCN 3/nesting 1, nonstrict CCN
2 and successor metrics. The same module's CLI test writes a real temporary
source and asserts subprocess CSV names/CCN/parameter counts/spans. Its HTTP
test uses the real Flask test client and asserts JSON names/CCN/parameter counts/
spans across all suffixes. Complete named consumer modules also observe options,
warnings, analyzer thresholds, reader selection and metric extensions.

```sh
nix develop -c python -m pytest test/testOutput.py test/testCommentOptions.py test/test_website_smoke.py -q > docs/reviews/pr525-correction-20261101/cost-return/located-consumers-final.txt 2>&1
```

Exit 0: **42 passed** on final product bytes. Output tests exercise real
formatters; comment-option tests assert forgiveness metadata;
`test_application_check_fetches_built_assets_original_logos_and_real_analysis`
uses the built Flask application and asserts CCN 3/NLOC 6/parameter count 1.
No assertion was weakened, no new skip added and no new/exposed semantic
regression was observed. Existing G1–G6 evidence and G2–G4 limitations remain
unchanged. Full pytest/make must be rerun by the coordinator for these new bytes.

## Trace, hashes and remaining owner

```sh
nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --path lizard_languages/javascript_bindings.py --path lizard_languages/javascript_functions.py --path lizard_languages/typescript_states.py --path lizard_languages/javascript_objects.py --term _call_group --term _read_parenthesis_group --term parameter_group --term full_parameters --term parameter_count --term expression_arrows > docs/reviews/pr525-correction-20261101/cost-return/consumer-trace-product.md
git diff --check
```

Both exited 0. `consumer-dispositions.md` accounts for all **295** located
consumer paths: **29 run, 266 excluded**, each with its observation/setup or
reason. Exclusions are historical evidence/workflow text, independent language
readers with unchanged shared `FunctionInfo`, and the unchanged frontend
renderer whose HTTP field/value contract is directly observed. Rendered browser
UI behavior was not tested.

`raw-captured-logs.json` retains exact captured text and SHA256 before stripping
only trailing whitespace/final blank lines from text copies. All prior evidence
and measurements remain intact. The only product files changed by this return:

| File | Final SHA256 |
| --- | --- |
| `lizard_languages/javascript_bindings.py` | `9d3303f0669f9c48d76dd6e55f0d201d7fbc8240eaacd8b4ad0b623a96eea509` |
| `lizard_languages/javascript_functions.py` | `1f0bbeb262f0ff24ce6ed84b2527488bf92b4317dda314caf73814c97e189f03` |
| `lizard_languages/javascript_objects.py` | `0b8a201fc4d80d79423a1533c46aa10cf33b2bec834cc694b265accbe362e3cb` |
| `lizard_languages/typescript_states.py` | `3f810420630f7d3d8f515965ebdbddcf3aeefeef0b18b116f2d8f8d0bbbb50ae` |

Coordinator-owned gaps remain: fresh independent aggregate refactor, new strict
paired affected argument/API/parameter/NS and broad CLI qualification, applicable
original-parent/cumulative comparisons, full pytest/make, and delivery/hosting.
G7's acceptance remains pending that timing result. No commit, push, plan/source
review edit, slice-done mark, independent refactor pass or lint run was performed.
The coordinator's observer remains untouched. Foreground Nix is released at
handoff with no owned command running.
