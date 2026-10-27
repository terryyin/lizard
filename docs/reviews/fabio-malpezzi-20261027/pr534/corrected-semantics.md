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
