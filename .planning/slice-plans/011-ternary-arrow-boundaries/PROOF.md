# Ternary arrow boundaries: retained proof

**Identity:** SEED-015#ternary-arrow-boundaries
**Plan:** [Executable plan](PLAN.md)

Retained preparation observations, accepted evidence and reusable proof
commands. Current statuses, decisions and obligations remain in the plan.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ`,
revision `1db5933b66e4602f607c6ae40998984dadd5bab5`, observed 2026-10-27.

- Ran `PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout <checkout>`.
  92 cases; failures include every promised row: `reported.*`, `authored.*`,
  `nested_ternary.*`, `nested_arrows.*`, `nested_arrow_internal.*`,
  `grouped_nested_arrow.*`, `branch_object.*`, `typed_arrows.ts/tsx`,
  `type_assertion.ts/tsx`, `typed_object_return.ts/tsx`, `jsx_attribute.*`
  and `vue_reported`/`vue_nested_ternary`/`vue_nested_arrows`/
  `vue_branch_object`/`vue_typed_arrows`. `internal_ternary`,
  `block_branches`, `plain_ternary` and the literal function-type controls
  pass (preserve them). Deferred rows (`object_property`, `computed_key`,
  `object_inside_ternary`, `inherited_*`) also fail and are not promised.
- Ran the same with `--inputs .../supplemental-inputs.json`: 4 cases, no
  failures. The curried internal-colon control is green on target and must stay
  green; it discriminates the raw #534 regression.
- A scratch script through `analyze_file.analyze_source_code` showed
  `{f: x => x, next: z => z && c}` reports only `f` CCN 2: the comma-sibling
  absorption is independent of ternaries, confirming its exclusion.
- Inspected `TypeScriptStates._state_global` lines 64–70 (`?`/`:` depth) and
  `JavaScriptFunctionsMixin._expecting_func_opening_bracket` (return-type `:`).
  Confirms the owners named above.
- Hash-checked `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py`
  as SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`
  and the cache `manifest.json` as
  `113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5`, equal to
  the checked-in manifest. From the cache `corpus` directory, ran
  `printf '1\n' | python ../paired.py worker <checkout> 0 api` through Nix →
  `{"seconds": …, "files": 92}`; and the CLI workload's literal command
  `python lizard.py <corpus>/lizard.py <corpus>/lizard_core <corpus>/lizard_languages <corpus>/lizard_ext`
  from the checkout → exit 1 (expected threshold warning) with full output.
  Frozen baseline tree exists at `/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline`
  (archive of `2e21e404`, not a Git checkout). This settles replay
  reachability, not any candidate's timing. Reconstruct via the PR534 README
  recipe if cache files disappear.
- `gh pr view 534 --repo terryyin/lizard` returned `OPEN`, head
  `f4074ae860c7999cc37dd081a6f7ebfa777adc83` on
  `FabioMalpezzi:pr/js-arrow-function-in-a-ternary-branch`, base `master`,
  `maintainerCanModify: true`. Readable metadata is not accepted publication;
  slice 1's early probe owns permission and route.

## Slice 1 acceptance

Slice 1 acceptance: a fresh no-inheritance source-only reviewer was launched as
`/root/source_review` with the story's Goal, all key examples, Scope and
exclusions, completion and original delivery contract text; it read no files.
Its launch/input and complete returned commitments are retained in this
execution conversation. The return separates each public name/span/CCN,
colon owner, consumer, curried control, typed control, performance comparison,
delivery gate and exclusion; it introduced no narrower rejection rule.

Comparison: given the intended contribution and contributor route, the source
requires full patch/discussion/review/check inspection and eventual
history-preserving contributor delivery. `correction-route.md` observes the
unchanged contribution, empty discussion/reviews, historical checks, current
permission and successful authenticated preview. This proves route feasibility;
actual acceptance remains slice 5. All public behavior examples and their
reader consumers remain slices 2–3, strict performance comparisons slice 4,
and full tests/current CI/final head-base-tree/merge/acknowledgement slice 5.
The source's named exclusions remain excluded. No missing slice-1 observation
was identified. Independent `/root/slice1_refactor` returned
`## REFACTOR COMPLETE`: no edits, 57-line evidence document, whitespace clean;
accepted observation locations unchanged, no redundant tests.

## Proof commands and conventions

Use `lizard-language-support` for reader/test changes. Public `analyze_file` /
`analyze_source_code` results own semantic proof; mock only filesystem
boundaries. Inspect every reached consumer and record run/reused/excluded with
reasons. Independent post-change refactoring runs through the execution
workflow before delivery; keep new tests (and any retained author test) in a
cohesive small module below 250 lines while leaving unrelated large test files
byte-identical.

Focused command after relevant edits:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
```

Also rerun the retained reproducer (main and supplemental inputs) against the
candidate checkout. `AGENTS.md` requires full `nix develop -c python -m pytest`
before completion; the source contract additionally requires
`nix develop -c make` and final hosted CI. Run Nix commands sequentially
because the shell hook changes `.venv`; keep owned heavy measurement serial.
Refresh current CI applicability in execution.

## Slice 2 acceptance

Slice 2 evidence: [corrected-semantics.md](../../../docs/reviews/fabio-malpezzi-20261027/pr534/corrected-semantics.md).
Fresh no-inheritance `/root/source_review2` read only the original seed and
delivery contract; its actual launch and complete individual commitments are
retained in this conversation. Compared with those promises, the reported,
nested, internal, grouped, object-branch, JSX-attribute and Vue journeys now
produce the source's names/spans/CCN through public analysis. The 105-line new
test module went 46 failed/28 passed to 74 passed; original tests are unchanged.
Inspected the two production changes and the module's setup/tuple assertions;
the snapshot is written only when an expression body opens and its matching
colon ends the function. Frozen public/FileAnalyzer replay now has five typed
failures (G1), all other 87 cases and four supplemental controls pass. The
landed neighboring behavior remains covered; no excluded promise was expanded.
Focused shared readers: 467 passed/5 skipped; reached object/core/CLI/options:
78 passed, plus actual CLI CSV reported tuples. Performance is slice 4 and
final full checks/contributor delivery are slice 5.

Independent `/root/slice2_refactor` returned `## REFACTOR COMPLETE`: no
production or test edits; retained observations moved here along cohesive
documentation seams. Plan guard and whitespace checks pass; the refactor
consumer trace has all dispositions, and semantic proof remains reusable.
