# Ternary arrow boundaries: retained proof

**Execution:** SEED-015#ternary-arrow-boundaries
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
actual acceptance remains slice 6. All public behavior examples and their
reader consumers remain slices 2–3, strict performance comparisons slice 5,
and full tests/current CI/final head-base-tree/merge/acknowledgement slice 6.
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
78 passed, plus actual CLI CSV reported tuples. Performance is slice 5 and
final full checks/contributor delivery are slice 6.

Independent `/root/slice2_refactor` returned `## REFACTOR COMPLETE`: no
production or test edits; retained observations moved here along cohesive
documentation seams. Plan guard and whitespace checks pass; the refactor
consumer trace has all dispositions, and semantic proof remains reusable.

## Slice 3 investigation

`/root/source_review3` freshly derived individual commitments from only the
original seed/contract and the explicit selected-correction authority. Its
launch and complete return are retained in the conversation. Partial
`/root/slice3_typed` correction makes the five frozen typed rows green and
passes 96 new-module cases and 495 focused cases/5 skipped. Parameter-type
evidence alone leaves G2, so no acceptance/completion is claimed. Runtime
parenthesized-value and genuine function-type controls remain green.
`/root/annotation_pfe` owns bounded existing-solution research before further
machinery; preserve partial edits. No source promise has been narrowed.

PFE finding: TypeScript annotation states already own type consumption and
are effect-free; provisional grouped bindings already have an owner. Go's
signature lookahead demonstrates preserving analysis tokens while carrying
metadata. Header-only recognition cannot decide G2; state-only body replay
would distort counters. The smallest supported next route is metric-safe
lookahead using the existing annotation owner and conditional confirmation.
The [official TypeScript 5.9.3 parser](https://raw.githubusercontent.com/microsoft/TypeScript/v5.9.3/src/compiler/parser.ts)
explicitly accepts empty groups and confirms ambiguous return annotations by
a following conditional separator. An isolated compiler probe observed no
syntax diagnostics and two ArrowFunction branches for both G2 forms, and
ParenthesizedExpression/ArrowFunction for the runtime-value controls. This
validates G2 rather than narrowing it. Command: `PIP_QUIET=1 nix develop -c npm
exec --yes --package typescript@5.9.3 -- node /tmp/lizard-seed015-typescript-syntax.cjs`.
The initial probe had a wrong module path (bin rather than lib); corrected
the probe path and obtained the stated terminal result, without repo changes.

G3 discovery: coordinator inspection identified that one later root colon
could belong to an outer conditional. Six TS/TSX/Vue nested call/group
regressions with a following typed initializer fail on the first lookahead.
The selected correction queries canonical active-state ternary depth and
group eligibility; no parallel lexical ternary counter or metric replay.
Keep the original token/line/CCN stream and one annotation recognizer, shared
through preprocessing for JS/TS/JSX/TSX/Vue. Empty groups are unambiguous
signatures. G1/G2/G3 all require current public proof before accepting slice 3.

Fresh trunk fetch during slice 3: `9979f106a955aef2460ed6fe7ebb5782b755bcad`.
Changes after `0ef95abb` are planning/review records only; analyzer sources
remain identical. The source's only change replaces the spent SEED-014 link
with its PR533 merge `a2d2ab7`; this confirms the reused neighboring behavior
and changes no promise, so the source-only reviews remain applicable.

## Slice 3 acceptance

Given genuine return annotations (typed, untyped or empty parameters), expected
separate branch arrows and surrounding/following names/spans/CCN are observed
in the public tuple assertions and all 92 frozen + 4 supplemental + 32 labelled
replays. Runtime grouped/call values preserve their separators and the following
typed initializer; six baseline/candidate probes also match NLOC, tokens and
parameters. Genuine function-type bodies, curried arrows, regex/templates,
JSX attributes and Vue offsets remain observed. Inspected the source-only
comparison, changed reader/helper and new test setup/assertions against the
literal proof/trace in `corrected-semantics.md`: 523 shared-reader/object cases
pass/5 skip, 160 core/CLI/CSV/cognitive cases pass, 32 optional cases pass.
No counter replay or second persistent ternary counter; the same annotation
state owns type recognition. G1/G2/G3 are proved. Original exclusions remain
exclusions; performance and final hosted delivery remain slices 5–6.

Independent `/root/slice3_refactor` returned `## REFACTOR COMPLETE`: reused
`JavaScriptBindings.BRACKETS` and consolidated Vue setup only. Inspected both
edits: sources and explicit expected tuples unchanged. After edits, 523 shared
tests pass/5 skip and all 128 replays pass. Other accepted core/CLI/optional
and full-metric observations remain applicable. Its retained refactor trace
has 311 populated dispositions; all current files are at most 250 lines and
whitespace checks are clean. No outstanding refactor gate remains.

## Remaining-plan refinement

The initial plan separated route, untyped ownership, typed annotation,
performance and hosted delivery; its cumulative direction remains valid.
Initial current/API upper bound +9.567% invalidates the assumption that the
semantically qualified revision already fits the budget. Other initial upper
bounds pass: current/CLI +0.729%, frozen/API -2.699%, frozen/CLI +1.311%.
An unchanged-output 5,000-analysis profile has preprocessing own time
0.0631s versus 0.1017s; this implicates bookkeeping but does not allocate the
whole measured difference. No noise retry is justified by this tight failure.

Split the remaining performance loop into coherent owner simplification
(Structure 4) and exact-committed-revision qualification (Behavior 5), then
retain hosted delivery as 6. Completed 1–3 and their proof are unchanged.
All source promises and strict gates remain; G4 receives its proof in 5.
No numeric slice limit was supplied, no overrun erased, and six cohesive
slices need no story resplit. Existing-owner PFE/ADRs still apply. Readiness
reassessment through the shared recorder follows the rewrite before delivery.

The shared recorder reassessed the unchanged refined story and six-slice plan
as ready (document `cf80763c`, plan `6ac6e2cb`); Taken membership and execution
authority were unchanged. Remaining gates stay explicitly owned by 5–6.

Literal timing protocol: `paired.py --baseline <checkout> --candidate <candidate>
--workload api|cli --pairs 20 --seconds 1.5 --out <result>`, from the isolated
cache corpus under selected-checkout Nix. Freeze exact revisions, untouched
original harness/manifest and labelled additions; verify imports/public metrics.
Inspect headroom, serialize owned heavy work, retain all raw pairs and stats.

## CI observation

Original Codex coordinator `akiho-seed015-a5bbe368`, cell 14, session 75355,
directory `/tmp/dough-ci-501/watch-UAZZGU`, PID 85484: terminal `finished`,
process absent; its sole delivered event is `CI_MONITOR_UNAVAILABLE` after
GitHub workflow-run acquisition failed (`gh failed`). Do not restart this
finished observer. Its coverage is unavailable, not a failed test verdict.
Direct GitHub check retrieval for 74bc899 shows all 14 Ubuntu/Windows ×
Python 3.8–3.14 builds plus changes successful; web skipped as inapplicable:
[run 40525520055](https://github.com/terryyin/lizard/actions/runs/40525520055).
This does not repair native coverage. A genuine coordinator handoff may close
the old lifecycle and establish its own observation for later publications;
retain G5 until final exact-head CI/completion obligations are proved.

The delivered unavailable event was acknowledged through sequence 1. A bounded
native acquisition probe of the exact failed workflow-run route now succeeds
and confirms all three published revisions' successful runs. The old terminal
observer stays closed; the continuation coordinator owns a new lifecycle.

Structure 4: original-only `/root/source_review4` supplied commitments. Inspected
diff/assertions: 715 pass/5 skip, all 128 rows and seven fields match 74bc899;
296 trace dispositions complete. `/root/continuation_coordinator/structure4_refactor`
returned `## REFACTOR COMPLETE`, no edits, 26 files clean. Delivered 8dac7b8.

Structure 5: original-only `/root/source_review5` launch/full return are retained
in the parent execution conversation; no plan/product/proof input. Compared its
individual commitments with the diff, source setup and assertions in
[context-optimization.md](../../../docs/reviews/fabio-malpezzi-20261027/pr534/context-optimization.md):
753 pass/5 skip; 128 public/FileAnalyzer rows and seven fields match 8dac;
100-file CLI is byte-identical. New runtime/trivia journeys retain source names,
spans and CCN. Inspected the canonical type evidence, demand-buffer lifecycle,
explicit tuples and verifier assertions; 285 trace dispositions are populated.
Independent refactor clarified names/comments; 162 regressions and 128 replays
pass. Later obligation owners remain in the active plan; diagnostics grant no timing acceptance.
