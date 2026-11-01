# Slice 2 profiling and binding-dispatch cost

Identity: `SEED-023#correct-pr525-binding-boundaries`, implementation checkout
`/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries`, branch
`codex/correct-pr525-binding-boundaries`, starting HEAD
`4a2081598ac8d70debafe64d672d65fd05857944`.

This is the delegated profiling/cost return. Changes are uncommitted. The
coordinator owns independent aggregate refactoring, paired qualification,
full pytest/make, acceptance, delivery, contributor publication and merge.
No paired benchmarks, full suite, make, commit, push, independent lint or
slice-status changes were performed here.

## Frozen inputs and bounded profiling decision

Host: `Territory.local`, 16 logical CPUs. Before profiles, load was approximately
7.9/10.7/10.5; `vm_stat` showed reclaimable inactive memory and no throttled
pages. Unrelated Open Dough Playwright/browser workers were visible and left
untouched. No competing owned foreground Nix or heavy proof was running.
The selected bound was one serial cProfile observation at a time, 40 repeats
of the same 131 affected sources, including public `parameters` and
`parameter_count` reads. Each profile terminated in under three seconds.
Final profile load was lower; the preceding observation was 4.0/7.4/9.1.
These observations establish call work, not an acceptance slowdown bound.
Contention, changed inputs or failed proof required reconsideration before
another expensive run. The fixed bounded profiles stayed applicable during
the local repair below; all attempts are retained.

```sh
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/prepare_measurements.py --cache /Users/terryyin/.cache/pr525-correction-qualified-20261101
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/.cache/pr525-correction-qualified-20261101/current --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/affected --out docs/reviews/pr525-correction-20261101/cost/current-profile.json --repeat 40
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/affected --out docs/reviews/pr525-correction-20261101/cost/pre-edit-profile.json --repeat 40
```

All exited 0 before production edits. The prepare helper required no correction.
It writes its manifest to `evidence/workload-manifest.json`, rather than the
cache root. The cache did not exist beforehand. It exported frozen current
`6d288d4cdcb85bc50ab4c8a887196177607eeb7d` and original parent
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71` from the supplied unchanged archives,
and froze 131 affected, 45 parent-safe and 100 broad files. No selection changed.
`frozen-input-checks.json` retains successful source/archive/input digest checks,
host data and harness digests. The manifest SHA256 is
`43b6a32af29c27fc6f277a00aa065ccca80d99bd320abfccba3aba96e531d9af`.

## Causal observation and smallest change

The corrected aggregate added binding candidates for call argument groups,
which need bracket/default ownership but cannot themselves be arrow formals.
`_read_binding_candidate` previously always allocated a provisional parameter
list for every `)` group and enabled `JavaScriptBindings._read_parameter`.

The existing `javascript_functions.parameter_group(prefix)` already decides
whether a group can introduce an arrow signature for return-annotation parsing.
The final change passes this same eligibility from existing runtime/object
dispatch owners to `_read_binding_candidate`. A new default-true
`track_parameters` argument suppresses only the provisional list/callback on
ineligible groups. Named formals, catch bindings and optimistic grouped-function
handling retain their existing paths/defaults. Bracket/default/complexity work
still uses the same binding reader. No counter, stream buffer, per-token wrapper,
circular import or generic parser was added.

| Profile signal, 131 sources × 40 | Frozen current | Corrected before edit | Final candidate |
| --- | ---: | ---: | ---: |
| `JavaScriptBindings.read_token` calls | 161,000 | 168,080 | 168,080 |
| `_read_binding_candidate` calls | 21,520 | 22,960 | 22,960 |
| `_read_parameter` calls | 67,560 | 71,120 | 1,800 |
| `at_root` calls within bindings | 9,560 | 10,120 | 40 |
| Binding `read_token` cumulative seconds | 0.1333 | 0.1466 | 0.0849 |
| Total instrumented seconds | 1.5481 | 1.6175 | 1.3832 |

The decisive observation is 69,320 fewer provisional parameter reads on the
same corrected token/candidate counts. The recorded seconds are unpaired
instrumented diagnostics and do not qualify any workload below 3%.
`profile-comparison.json` retains all profile comparisons; each full profile
retains imported checkout, Python version, production hashes, calls and times.
`candidate-hashes.json` matches the final profile and current product bytes.

```sh
nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --corpus /Users/terryyin/.cache/pr525-correction-qualified-20261101/affected --out docs/reviews/pr525-correction-20261101/cost/post-edit-profile.json --repeat 40
```

Exit 0 on the final candidate. No production edit followed that profile.

## Retained rejected attempt and repair

After an initial eligibility-rule implementation passed the 897-test affected
suite, one attempted specialization treated the differing-function-name branch
as a proven call and passed `track_parameters=False`. Existing ternary arrows
also reach this branch: `const pick = flag ? (a, b) => a : null` lost `a,b`.
The existing assertions caught six affected-suite failures and three overlapping
consumer failures. `rejected-specialization-affected.txt`,
`rejected-specialization-consumers.txt`, `rejected-specialization-profile.json`
and `rejected-specialization.json` preserve that invalid attempt and exact patch.
The rejected profile was first written at the literal `post-edit-profile.json`
path above, then renamed when its semantic failure became known.

Restoring `parameter_group` on that branch produced 897/499 passing tests;
`restored-specialization-*` retains those passing logs/profile. For the final
form, every argument group in dispatch and object reading uses the existing
eligibility function, including member/constructor groups. The complete affected
and consumer commands then passed again on final bytes. No test/assertion was
weakened. This disposes of the introduced G6 loss; the unchanged G1–G4
compatibility/exclusion dispositions remain in the plan and slice 1 evidence.

## Terminal focused proof and observers

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testArrowObjectBoundaries.py test/test_languages/testOptionalOperators.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testES6.py test/test_languages/testVue*.py -q > docs/reviews/pr525-correction-20261101/cost/affected-final.txt 2>&1
```

Exit 0: **897 passed, 5 existing skips** on final product bytes. This selects
complete modules, without `-k`. Setup is real source to `analyze_file` or
`FileAnalyzer`; no parsing mocks. Relevant observing assertions:

- `testJavaScriptArrowParameters.py::test_anonymous_and_named_bindings_agree`,
  `test_reported_arrow_forms`, `test_defaults_and_wrapper_call`: real named,
  anonymous, member-call and ternary arrows keep parameter names/counts and
  default CCN.
- `testJavaScriptFormalSlots.py::test_named_and_anonymous_empty_patterns_occupy_slots`,
  `test_callback_type_annotation_has_no_runtime_function`,
  `test_generic_and_object_return_annotations_preserve_formals`,
  `test_nested_and_ternary_defaults_keep_complexity_owners`: destructuring,
  empty slots, typed/generic/return signatures, defaults and ternary ownership.
- `testJavaScriptBindingBoundaries.py::test_authored_binding_boundaries`,
  `test_callback_keeps_decisions_and_following_formals`,
  `test_argument_readers_finish_arrow_before_next_object`,
  `test_typed_generic_and_ternary_arrows_keep_signature_owners`: original authored
  outputs plus corrected callback spans/CCN/formals, ordinary/member/optional/
  constructor and object-value readers, successor metrics across seven suffixes.
- `testTypeScriptReaderDispatch.py` asserts root binding owner configuration,
  custom decision hooks/parallel states, EOF, emitted tokens and public analysis.
  `testVueSourceLineNumbers.py` observes wrapper offsets.
- `testJavaScriptArrowParameters.py::test_arrow_parameter_metric_consumers` and
  formal-slot equivalents assert actual CSV/checkstyle/HTML parameter values
  and warning filtering. Duplicate-list tests assert normalized alias keys.
  CLI tests create real temporary sources and assert subprocess rows/exit codes.

```sh
nix develop -c python -m pytest test/test_languages/testJavaScriptBindingBoundaries.py test/testApplication.py test/test_options.py test/testLanguages.py test/test_analyzer.py test/testExtension.py test/testJavaScriptNestedStructures.py test/testNestedStructures.py test/testNestedStructureScopes.py test/test_extensions/testNonStrict.py test/test_extensions/testCognitiveComplexity.py test/test_extensions/testFanInFanOut.py test/test_extensions/testHalstead.py test/test_extensions/testHalsteadClassifier.py test/test_extensions/testHalsteadMetrics.py test/test_extensions/testDuplicatedParamList.py test/test_website.py -q > docs/reviews/pr525-correction-20261101/cost/consumers-final.txt 2>&1
```

Exit 0: **499 passed, 2 existing skips** on final bytes. The binding module's
`test_metric_extensions_keep_callback_decisions` asserts NS CCN 3/nesting 1,
nonstrict CCN 2 and successor metrics, using `FileAnalyzer(get_extensions(...))`
with real source. `test_http_api_exposes_complete_callback_and_following_metrics`
uses the real Flask test client and asserts JSON names/CCN/parameter counts/spans
for all suffixes. `test_cli_reports_complete_callback_and_following_metrics`
asserts real CSV rows and spans. Complete consumer modules additionally exercise
CLI options, analyzer thresholds, reader selection and metric extensions.

```sh
nix develop -c python -m pytest test/testOutput.py test/testCommentOptions.py test/test_website_smoke.py -q > docs/reviews/pr525-correction-20261101/cost/located-consumers-final.txt 2>&1
```

Exit 0: **42 passed**. This ran after restoring ternary eligibility, before the
final equivalent known-call cleanup. Final `parameter_group('.')` and
`parameter_group('new')` are both false, so those consumers' observations remain
applicable; final affected, CLI, HTTP and parameter-output proofs also pass.
Output assertions use real formatters; comment forgiveness checks metadata;
`test_application_check_fetches_built_assets_original_logos_and_real_analysis`
uses the built Flask application and asserts CCN 3/NLOC 6/parameter count 1.

## Consumer trace and limits

```sh
nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --base HEAD --term track_parameters --term parameter_group --term full_parameters --term parameter_count --term parameters > docs/reviews/pr525-correction-20261101/cost/consumer-trace.md
nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries --path lizard_languages/javascript_bindings.py --path lizard_languages/typescript_states.py --path lizard_languages/javascript_objects.py --term track_parameters --term parameter_group --term full_parameters --term parameter_count --term parameters > docs/reviews/pr525-correction-20261101/cost/consumer-trace-product.md
git diff --check
```

The trace commands exited 0. `consumer-dispositions.md` accounts for all 473
unique located paths: 33 run, 2 reused, 438 excluded, with observing assertions,
setup and reasons. Most matches are historical review evidence or unrelated
language readers sharing parameter vocabulary. `FunctionInfo` and those readers
were not changed. The frontend renderer was not tested as a rendered UI; its
unchanged HTTP field/value contract is observed through the real API tests.

Exact raw text logs and hashes are retained as JSON strings in
`raw-captured-logs.json`; the text copies remove only trailing whitespace/final
blank lines for Git whitespace checks. Failure/assertion content is unchanged.

ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) constrains the public
metric meanings; the index and ADR-0000 were also read. This local change keeps
those meanings and existing owners, with no architecture exception.

Outstanding coordinator gates: independent aggregate refactor, all paired
affected API/parameter and NS, broad CLI, parent-safe and cumulative comparisons
where applicable, strict 95% upper slowdown bound below 3%, full pytest and make,
then exact-head hosted CI/publication/merge. No performance acceptance is claimed.
