# Reconcile accepted signature and parameter owners

History base: coordinator-owned merge `824e260075ec74ae119e3af0af5b9fe0a60c4a1a`,
parents `3a1ed8ae69314f070118ffc05d317149db7eecf8` and current target
`1bc7cbd64bef352300195c66888139bdf8d8d8e0` (PR536). The installed backlog
merge adapter accepted this local merge; published history was preserved.
The observations below exercise the **uncommitted working product**, including
the guard removal and comment consolidation below, not untouched merge bytes.
`current-owner-product-hashes.json` freezes all 104 product source hashes;
`current-owner-proof.json` retains the runtime patch hash against that history
base. Exact committed-revision throughput qualification remains slice 7.

## Existing owners and decision

PR536 centralizes named/provisional formal capture in `JavaScriptBindings`,
including `BindingSummary.full_parameters` and `read_token(report=False)`.
The existing type reader owns grouped function-return types. The clean merge
retains these owners and SEED-015's ternary-depth rule, completed-group type
evidence and demand-driven lookahead. No parser, binding tracker or metric
counter was added. Accepted ADR-0000 and ADR-0001 remain applicable; no new
architectural direction or exception was needed.

Bounded inspection found one redundant provisional guard: PR536's
`_resolve_pending_bindings` now retains the summary on any `:`, including the
str-compatible `ReturnTypeColon`. Remove the caller's duplicate marker exemption;
the canonical resolver preserves formals and defaults, and marker dispatch still
delegates annotations before runtime colons. Keep `_typed_parameters`: the
binding reader's `_in_parameter_type` resets at a formal separator and records
the current formal, while the group evidence records any earlier consumed type.
`full_parameters` intentionally omits type syntax and cannot replace that fact.
Six comment lines were consolidated to keep TypeScriptStates at 250 lines;
there was no further state or algorithm change.

## Observed supplier change and exact expectation

The initial focused command ended 1: 828 passed, five skipped, eight failed.
All failures were the new supplemental runtime-value fixture containing
`(x = (z => ({a: z}))(d))` before its ternary separator. The old fixture expected
only the alternate arrow. Immutable three-version public probes establish:

- 3a1: alternate anonymous arrow at 2–2 CCN 2; outer 1–4 CCN 2; after 5–5 CCN 1.
- Current 1bc and merged working product: initializer anonymous arrow at 2–2
  CCN 1, then the same alternate/outer/after. All seven fields and parameter
  names match current for all four JS/TS/JSX/TSX × two alternate forms.

This is a real arrow inside an assignment initializer, not a nested curried
arrow in an already open expression body. The coordinator accepted this
current supplier behavior. The supplemental fixture now asserts the complete
current list explicitly, including initializer CCN 1 and alternate CCN 2.
Original curried assertions and all 128 frozen expected rows are unchanged.
The failed log and exact before/current/merged probes remain in the cache;
this was not dismissed by a passing retry or a weaker predicate.

## Proof and observation locations

Before proof, Territory.local had 16 CPUs and load 6.845/7.441/6.752; the
retained `host-before.json` includes VM/process observations. The coordinator
confirmed no competing owned heavy work; other native worktree activity was
unknown. One serial focused semantic suite plus replay/CLI observations is
modest on this host. Reconsider on contention or unexplained failure; no
profiling or statistical measurement was launched. Setup uses this checkout's
locked Nix Python; the recurring pip platformdirs warning is retained setup
noise, with numeric command results below.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput*.py test/testApplication.py test/test_options.py test/testBasicFunctionInfo.py test/test_extensions/testCognitiveComplexity.py test/test_extensions/testDuplicatedParamList.py test/test_languages/testOptionalOperators.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-current-1bc7cbd/focused-final.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_website.py test/test_website_smoke.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-current-1bc7cbd/website-final.log 2>&1
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/current-owner-verify.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/current-owner-integrity.py
```

Every final command terminated 0. Focused result: 842 passed, five unchanged
skips; website result: 41 passed. `focused-final.log`/`website-final.log`, the
first failed log and intermediate corrected run remain separate. Source code
strings, filenames, real temporary files and thresholds provide setup; no
parser or analyzed result is mocked. Inspected observing assertions include:

- `testJavaScriptTernaryArrows.py`: branch/internal/curried and typed tuple
  assertions, nested runtime separators/following initializer, trivia and the
  complete supplemental initializer list. The module is 239 lines.
- `testJavaScriptArrowParameters.py`: anonymous/named patterns, sigils,
  wrappers/defaults and reported typed forms; real CSV/checkstyle/HTML,
  parameter warnings, duplicate-list keys, CLI exit/columns and website JSON.
- `testJavaScriptFormalSlots.py`: empty formal slots, receiver/trailing
  exclusions, callback types, grouped function returns, generic/object types,
  default CCN owners and the same count/output/duplicate/CLI/JSON boundaries.
- `testBasicFunctionInfo.py`, shared parameter-name suites and output suites:
  public model/count conventions, established names, CSV/HTML/checkstyle data.
- Website tests: real Flask/HTTP public metrics and static asset smoke; fixture
  assets only supply startup. `Playground.jsx` still renders each `fn[field]`;
  no new browser-rendering or deployed-site assertion is claimed.

`current-owner-verify.py` invokes the unchanged `qualification-verify.py` for
actual working imports and exact 3a1/current archives. All 128 expected rows
pass on the working product, with independent FileAnalyzer agreement. Before
versus merged has identical function counts and names/spans/CCN/NLOC/tokens.
The sole differences are 218 parameter counts: 208 from 0→1 and ten from 0→2,
the formals now captured by PR536. The 43 distinct source strings use simple
named formals (two for the reported branches); the unchanged canonical formal
tests above exercise those conventions. Every difference is retained in
`current-owner-metric-differences.json`, with case/function/value provenance.

The unchanged 100-file broad corpus runs through before/current/working CLI.
All return the expected warning exit 1 and identical complete output SHA256
`49e9a7922e48f71ac1702f2da38939d93c24b4339d0d3618dfdfdebe1287150b`.
No original input, expected row, manifest, profile or timing result was changed.

## Consumer correspondence and remaining work

The installed trace uses base 3a1 and supplied `BindingSummary`, `FunctionInfo`,
`_append_parameter`, `read_token`, `full_parameters`, `parameter_count`,
`parameters`, `return_type_colon`, `ReturnTypeColon`, `_typed_parameters`,
`TypeScriptReader` and `preprocess`. Every located row has a disposition in
`seed015-current-1bc7cbd/consumers-final.md`; its count/hash are in the proof JSON.
Affected JS/TS/JSX/TSX/Vue/CJS/MJS, API/FileAnalyzer, core, output, cognitive,
duplicate-list, CLI and HTTP consumers have the current observations above.

Other-language/model/extension proof reuses PR536's retained full-suite
1944 passed/7 skipped, terminal exit 0 in `candidate-5-semantic.log`, with
setup/assertions described in `pr536/slice4-proof.md`. All 103 supplier product
manifest files match except the three shared JS/TS owners tested here; the
added signature helper is tested by the same shared path. Reused tests are
byte-identical to supplier `f6d5b09`. Dependencies and lock files are unchanged.
Historical research is excluded as history, and lexical documentation-link or
package-extension matches do not create runtime dependencies. No reached
consumer is left unproved or without a disposition.

The cache is `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-current-1bc7cbd`.
Independent refactoring/delivery remain with the coordinator, all four strict
performance gates with 7, and final full pytest/make/hosted/contributor duties
with 8. No throughput acceptance is claimed. All owned commands are terminal;
no extra verification watch was created.

## Source comparison and semantic acceptance

Fresh original-only reviewer `/root/continuation_coordinator/source6_review`
received only the seed/contract source; its actual launch and complete return
remain in the execution conversation. No plan/product/proof was supplied.
Compared its individual journeys with the current diff, public assertions and
verifier setup: the original 128 names/spans/CCN and curried/type/body controls
remain observed. Parameter-only changes preserve the accepted supplier owner;
the eight added initializer functions match current, with enclosing/following
functions unchanged. Inspected actual source hashes, canonical colon resolver,
tuple assertions, FileAnalyzer/import checks, output/HTTP consumers and the
supplier's numeric full-suite receipt. No promise narrowed; G7 is proved.
Four strict timing gates and missing prior terminal-receipt recovery remain
with 7, final hosted/contributor duties with 8. Independent
`structure6_refactor` returned `## REFACTOR COMPLETE`: no edits or tests;
104 product hashes, 499 trace dispositions and all 10 changed files checked.

Before qualification, freeze all 38 added query/trivia controls prospectively.
Keep the four original 128-API/100-file-CLI gates independent; additionally
require augmented 166-input API upper bounds below 3% versus current/frozen.
All six bounds must pass. This includes relevant supplemental cases without
averaging away the original affected workload. Preserve all old experiments.
