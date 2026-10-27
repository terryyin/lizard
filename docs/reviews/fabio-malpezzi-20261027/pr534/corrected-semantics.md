# PR534 correction: untyped branch boundaries

SEED-015 slice 2, observed on 2026-10-27. Baseline:
`ce9dcee0de55bbacea98fb202bfdc33669474c62`. The uncommitted correction records
the ternary depth only when a new expression body opens. A colon closes that
function only at the recorded depth. Curried arrows that create no function
cannot overwrite the enclosing arrow's owner. The public facade, tokenizer,
binding and type-annotation contracts are unchanged in this slice.

## Public observations

`test/test_languages/testJavaScriptTernaryArrows.py` supplies source code as
the precondition and compares public function names, start/end lines and CCN:

- `test_branch_colons_and_internal_colons` observes nested ternaries, curried
  branches, curried internal-colon controls, grouped nested arrows,
  parenthesized object branches, internal ternaries, block branches and plain
  ternaries in JS/TS/JSX/TSX/CJS/MJS and actual Vue script blocks. It also
  checks enclosing and following function metrics and Vue line offsets.
- `test_reported_two_parenthesized_branches` observes the reported `choose`
  example in JS/TS/JSX/TSX/Vue.
- `test_jsx_attribute_contains_separate_branch_arrows` observes both arrows
  in an actual JSX/TSX attribute and the enclosing `view` function.
- `test_property_colon_and_computed_key_keep_their_ownership` observes the
  promised first `f` branch and surrounding functions without asserting the
  deferred comma sibling result. Its computed-key control preserves the
  current target's public result.

```sh
PIP_QUIET=1 nix develop -c python -m pytest -q test/test_languages/testJavaScriptTernaryArrows.py
```

Before production edits: **46 failed, 28 passed**, with mismatched arrow
counts/CCN (for example nested `f` CCN 3 instead of 1). After the correction:
**74 passed**. The module is below 250 lines; existing tests are unchanged.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput.py test/testApplication.py test/test_options.py
```

Results: **467 passed, 5 skipped**, then **78 passed**. Existing
`testES6.py::Test_ES6_multiple_arrow_patterns::test_nested_arrows` retains
the one-function curried contract. Existing arrow-line and object-boundary
suites preserve the already-landed neighboring behavior; no new promise is
claimed for those stories.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --inputs docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json
```

Baseline main replay: 92 cases, 49 failures; supplemental: 4 cases, no failures.
Candidate main replay: 92 cases, only `typed_arrows.ts`, `typed_arrows.tsx`,
`typed_object_return.ts`, `typed_object_return.tsx`, and `vue_typed_arrows.vue`
still fail. These five remain owned by slice 3. Every other frozen row passes,
including the type-assertion rows and deferred rows already helped by the
landed object correction. Candidate supplemental: 4 cases, no failures.
The reproducer checks explicit expected rows, public `analyze_file` and
independent `FileAnalyzer` agreement, and actual imported module paths.

A real CLI observation used `python lizard.py --csv <cache>/reported.js`
under the same Nix interpreter. Parsed CSV name/start/end/CCN tuples were
`(anonymous),2,2,1`; `(anonymous),2,2,2`; `choose,1,4,2`; exit 0. The source is
the frozen reported example. Raw CSV and command observation are retained
with the replay evidence below.

## Consumer trace and evidence retention

```sh
PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --base HEAD --term _expression_ternary_depth --term _ternary_depth --term _arrow_function --term 'ternary branch' --term lizard_languages/javascript_functions --term javascript_functions --term lizard_languages/typescript_states --term typescript_states
```

The raw generated record is in the review cache at
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-slice2/consumers.md`.
The same directory retains `baseline.jsonl`, `baseline-supplemental.jsonl`,
`candidate.jsonl`, `candidate-supplemental.jsonl`, `red.log`, `reported.js`
and `cli.csv`. Logs were copied after observation, without repeating proof.

| Reached consumer | Disposition and observation |
| --- | --- |
| `javascript_bindings.py::_push_arrow_function` | Run: new curried/internal/grouped regressions and existing `test_nested_arrows`; a reused function keeps the same expression-body snapshot |
| `typescript.py::TypeScriptReader` | Run: focused shared-reader suite and frozen `analyze_file`/`FileAnalyzer` replay; `parallel_states` composes the changed state owner |
| JavaScriptReader, including CJS/MJS aliases | Run: new public tuples through every alias; existing JS/ES6 suites |
| TSXReader, including JSX attributes | Run: new actual attribute regression, JSX/TSX suites and frozen rows; tokenizer still forwards expressions to the shared state |
| VueReader script filtering | Run: new wrapped source/line-offset assertions, existing Vue suite and frozen Vue rows |
| Listed `testES6.py`, `testJavaScriptArrowLines.py`, `testJavaScriptFunctions.py`, `testJSX.py`, `testTypeScriptFunctions.py` | Run: focused suites; inspected existing object-arrow count, curried count, alias/name, typed-arrow and JSX observations |
| Public CLI, output and core integration | Run: real CSV observation and 78 reached integration/object tests; FileAnalyzer equality in both frozen replays |
| Historical review JSON/logs/manifests/patches and copied consumer traces | Excluded: immutable observations of named historical snapshots, with no current runtime dependency or test registration; the candidate's contract is observed by current tests/replays |
| Historical one-off review/profile/reconstruction scripts | Excluded: snapshot-specific research tools outside product/test entry points; no helper contract or import path was changed |
| Active source, plan and `DearDough.md` matches | Reused: source/plan read before work; documented nested-arrow warning matches the preserved owner rule; no guide text, interface or workflow behavior changed |

No other language reader imports the changed owners. Shared token generation,
regex/comment handling and metric counters were not changed. Performance,
final full-suite/make, hosted CI and contributor delivery remain later work.

## Typed signatures and runtime controls (slice 3)

Baseline: `93de60ca34af94f3c30c163eaf615ad95ba471f7`. Return annotations
are confirmed before counters consume their tokens. The existing
`TypeScriptTypeAnnotationStates` owns type recognition; a tee copy of the
filtered token iterator supplies lookahead without replaying analysis tokens.
The confirmed colon remains a string token and enters the existing annotation
owner before runtime ternary dispatch. Calls are excluded as parameter groups.
For ambiguous nonempty groups, lookahead must leave the separators owed to
the canonical active runtime state's pending conditional depth; empty parameter
groups are unambiguous. No second persistent ternary counter was introduced.
The provisional parameter-annotation flags were removed when this complete
rule made them redundant.

The existing small public module adds these observations:

- `test_typed_branch_annotations_are_not_runtime_colons`: parameter/return
  annotations, untyped and empty parameters, object return types, assertions,
  and nested typed branches in TS/TSX/Vue; explicit names/lines/CCN, including
  outer and following functions. Initial typed red: 6 failed/90 passed;
  G2 untyped/empty red after the partial correction: 6 failed/100 passed.
- `test_parenthesized_runtime_value_keeps_its_branch_separator`: a runtime
  grouped value followed by either a bare or parenthesized alternate arrow in
  JS/TS/JSX/TSX; these must remain runtime separators.
- `test_function_type_alias_and_return_keep_the_real_body`: genuine function
  types returning boolean, string and template literal types, with real body
  and following-function metrics in TS/TSX/Vue.
- `test_signature_lookahead_preserves_lexical_tokens_and_metrics`: active
  lookahead across an opaque regex or template, and an internal body ternary.
- `test_nested_runtime_separator_keeps_following_type_annotation`: nested
  call/group values plus a following typed `cb` initializer. The first probe
  failed all six TS/TSX/Vue controls by making `cb` anonymous; the active-depth
  correction restores the baseline result. Source supplies only preconditions.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py test/testOutputCSV.py test/testApplication.py test/test_options.py test/test_extensions/testCognitiveComplexity.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testOptionalOperators.py
```

Final results: **523 passed/5 skipped**, **160 passed**, **32 passed**.
The new module stays below 250 lines; all unrelated existing tests are unchanged.
Existing exact-token, regex/template range, JSX attribute, Vue script-filter,
optional-operator and function-type controls are selected by these commands.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --inputs docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --inputs docs/reviews/fabio-malpezzi-20261027/pr534/signature-inputs.json
```

All **92 + 4 + 32** rows pass, with public/FileAnalyzer equality and import-path
assertions. `signature-inputs.json` is a labelled addition of the new G2/G3,
object-type and active-lookahead lexical controls across their tested consumers.
It is not part of an accepted timing claim yet; slice 4 must freeze/hash it
alongside the original corpus, leaving the original manifest unchanged.

From the selected Nix environment, `.venv/bin/python lizard.py --csv
/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-slice3/signature_untyped_return.ts`
exited 0. Parsed name/start/end/CCN tuples equal the two anonymous arrows
(1/2), `outer` (2) and `after` (1) in that labelled case. Six isolated real
public probes on an archive of the baseline and the final candidate also match
names, lines, CCN, NLOC, token and parameter counts for the G3 controls.

## Slice 3 consumer trace and retained proof

The installed `consumer-trace.mjs --checkout <execution checkout> --base HEAD`
was run with explicit `TypeScriptReader.preprocess`, `preprocess`, `code_tokens`,
`return_annotation_depth`, `ReturnTypeColon`, `return_type_colon`,
`parameter_group`, `_consume_type_annotation`, `TypeScriptTypeAnnotationStates`,
`return annotation`, all changed owner module/import paths, and the reached
`get_ts_function_list`, `get_js_function_list`, `INTERACTIVE_WIDGET` helpers.
The complete literal command and generated record, with every disposition,
are retained in the cache directory below.

| Reached boundary | Disposition and observation |
| --- | --- |
| Registry, JavaScriptReader/CJS/MJS, TypeScriptReader, TSXReader/JSX and VueReader | Run: shared-reader suite, frozen and labelled public/FileAnalyzer replays; wrapped Vue and actual JSX controls |
| Existing type annotation owner and signature/runtime state composition | Run: typed/nested/control tuples and genuine function-type tests; effect-free lookahead reuses the same type reader |
| Core preprocessing/extension loading and public CLI/CSV | Run: 160 integration checks, real typed CLI CSV and all public replays; G3 baseline/candidate full metrics match |
| Cognitive profile/extension | Run: cognitive suite, including existing JavaScript and optional-chain/nullish observations; token compatibility preserved |
| Optional operators and widget/helper consumers | Run: 32 optional checks and full JS/TS suites; widget fixtures are exercised through their real consumer tests |
| Other languages' preprocessing and C preprocessor extension | Excluded: independent readers/processors do not use TypeScriptReader's preprocessing or the signature helper |
| Historical review records/reconstruction/profile scripts | Excluded: immutable snapshot evidence outside current product/test entry points, with no changed helper contract |
| Active source/plan/guidance references | Reused: source commitments and execution context remain authoritative; no workflow/API wording changed |

Raw red/green/focused/integration/optional/replay logs, `consumers.md`, baseline
archive `baseline-93de60ca`, nested before/after full metric probes, CLI CSV and
source-generation/probe scripts are retained at
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-slice3`.
G1/G2/G3 now have passing observable proof. Performance, full-suite/make,
hosted CI and contributor publication/merge/acknowledgement remain later work.
