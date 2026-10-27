# Preserve arrow object expressions and typed function bodies

**Identity:** SEED-013#arrow-object-boundaries
**Source:** [Refined story](../../seeds/SEED-013-arrow-object-boundaries.md#arrow-object-boundaries)

## Outcome and boundaries

Users analyzing JavaScript, TypeScript, JSX/TSX and Vue script content get
correct public function names, start/end lines and CCN when an expression-bodied
arrow's body is an object (including after `in`, `void`, `delete`, `typeof`,
`await`, `>` and `>=`), and when a function's return type is itself a function
type. Following functions stay separate. Block-bodied arrows, parenthesized and
nested objects, logical/nullish controls, JSX attribute arrows and the author's
boolean return-type control remain correct.

The source's key examples and
[PR531 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md)
are the acceptance input. The frozen
[reproduce.py](../../../docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py)
checks explicit `[name, start_line, end_line, CCN]` for 64 retained inputs.

Excluded commitments: multiline expression-continuation (SEED-014), ternary-colon
arrow ends (SEED-015), arrow parameter bindings (SEED-016), ENS-only performance,
a full JS/TS grammar, and new language support. No sibling is a formal blocking
supplier. No product rejection rule follows merely from an unlisted valid form.
This is the technical follow-up after a review that exceeded its 15-minute
correction cutoff; that cutoff is not a time limit for this plan. No
project-specific numeric slice limit was supplied.

Preparation only: all slices below are planned. This record neither Takes the
story nor authorizes implementation, contributor writes, merge or comments.

## Existing owners and architectural direction

PFE: change the existing expression-arrow and brace-dispatch ownership in
`lizard_languages/javascript_functions.py` (`JavaScriptFunctionsMixin`) and
`lizard_languages/typescript_states.py` (`TypeScriptStates`), coordinated with
`TypeScriptTypeAnnotationStates` in `typescript_declarations.py` so a typed
signature's real `{` body is not treated as an object operand. TypeScriptReader
composes those owners; TSX and Vue script reuse TypeScriptStates / TypeScript
tokenization; JavaScript inherits the same path; `.cjs`/`.mjs` alias the same
reader. Do not add a parallel brace classifier or PR-specific punctuation list.

The raw proposal's first-character heuristic (`_opens_an_object`) is the rejected
shape: it breaks string/template function-type returns and leaves word/comparison
object operands wrong. Prefer one coherent runtime-arrow versus function-type
context that preserves binding and object ownership already present in those
mixins.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for CCN, language reader and function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no new North Star topic
or ADR is warranted. No architectural exception is proposed.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/preserve-arrow-object-expressions-and-typed-func`,
revision `874d2847e7d7c7bcc46d9b1343ba3e6400c29256`, observed 2026-10-27.

- Ran `nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-object-expressions-and-typed-func`.
  Summary: 64 cases, 44 failures matching the retained target defect set
  (reported object arrows, operator object operands, nullish/JSX attribute
  cases, Vue reported/`in`). Function-type string/template/boolean and
  block/parenthesized/nested controls pass on this checkout. This public
  consumer demonstrates the missing object-boundary behavior; existing green
  suites alone do not prove the correction. Add discriminating red/green cases
  in slice 2; do not claim all existing guards independently discriminate it.
- Inspected `JavaScriptFunctionsMixin._arrow_function` /
  `_expecting_func_opening_bracket` and `TypeScriptStates._state_global` brace
  dispatch (`started_function` always opens a nested function body today) plus
  `TypeScriptTypeAnnotationStates` returning from function-type parentheses.
  Confirms the shared owners named above; the raw patch's `_expression_body`
  flag alone is insufficient without coherent type-versus-runtime context.
- Hash-checked
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py` as
  SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`
  (immutable historical harness). Manifest SHA256
  `a224a3afcfa549f1d1d686be5416439d293105941894e8850a700777adb62411` matches
  the checked-in `manifest.json`. Ran
  `printf '1\n' | PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py worker /Users/terryyin/git/lizard/.worktrees/preserve-arrow-object-expressions-and-typed-func 0 api`
  → `{"seconds": …, "files": 64}` against the retained corpus. This settles
  replay reachability for the affected API workload, not correctness or timing
  acceptance of any future candidate. Reconstruct from Git via the PR531 README
  recipe if cache files disappear; no cache path is a delivery dependency.
- `gh pr view 531 --repo terryyin/lizard` returned `OPEN` at head
  `47aa35735cccba962e507edb179eae1154ab6ad2`, targeting master (base
  `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`). Read-only metadata is not
  accepted publication; slice 1's early probe owns permission and route.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Object expression bodies and operator object operands | Slice 2: new public API regressions fail before correction and pass after it in JS/TS/JSX/TSX and Vue |
| Function-type return annotations keep real bodies | Slice 2: string, template and boolean controls retain anonymous runtime arrows, filter spans and following `after` |
| Controls and shared consumers | Slice 2: block/parenthesized/nested/logical/nullish/JSX attribute cases; focused consumer suites |
| Strict affected and broad throughput | Slice 3: fixed-corpus warmed paired measurements, each upper 95% slowdown bound strictly below 3% versus current target and frozen baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 4: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Use `lizard-language-support` for reader/test changes. Public `analyze_file` /
`analyze_source_code` results own semantic proof; token tests can support
representation contracts but do not replace it. Mock only filesystem
boundaries. Inspect every reached consumer and record run/reused/excluded with
reasons. Independent post-change refactoring runs through the execution
workflow before delivery; preserve capability names and keep author tests in a
cohesive small module while leaving unrelated large test files unchanged.

Focused command after relevant edits:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
```

Also rerun the retained reproducer against the candidate checkout. `AGENTS.md`
requires full `nix develop -c python -m pytest` before completion. The source
additionally requires `nix develop -c make` and final hosted CI. Run Nix
commands sequentially because the shell hook changes `.venv`; keep owned heavy
measurement serial. Current CI applicability must be refreshed in execution;
the seed's historical 14-job matrix is not a permanent substitute for actual
required checks.

## Ordered slices

### 1. Establish the contributor correction route
Type: Behavior
Status: done
Proof: refreshed PR/head/base, contributor-edit permission and authenticated
publication-route observations; preserve exact refusal if unavailable.

Behavior: given an explicit execution instruction for this correction, refresh
PR531's entire patch/discussion/checks and current target, verify the recorded
head/branch still names the intended contribution, and establish the authorized
contributor-head delivery route before dependent implementation. Read-only
metadata was observed above; credential/owner-held publication is an early probe.
Use a non-mutating authenticated push preview where available. A preview is
not accepted publication; slice 4 owns the real update. If the PR is closed,
superseded, changed incompatibly or cannot receive corrections, stop and return
the concrete disposition/destination decision to Terry. Do not substitute an
unrelated trunk patch. No implementation slice depends on unobserved permission.

Boundedness: one external delivery feasibility observation; it leaves the
product unchanged. A failure stops slices 2–4 and revises the plan before edits.

### 2. Preserve object-arrow and typed-body boundaries through shared readers
Type: Behavior
Status: done
Proof: red/green public regressions for the source examples and controls, plus
the focused consumer command and retained 64-case reproducer exiting clean.

Behavior: given an expression-bodied arrow whose body is an object (ternary
objects, nullish/logical objects, JSX attribute objects) or an object operand
after `in`/`void`/`delete`/`typeof`/`await`/`>`/`>=`, analysis ends the arrow
on the expression's last line with correct CCN and keeps the enclosing and
following functions separate. Given a function whose return type is a function
type (string, template literal, or boolean control), the typed `=>` does not
steal the real body: runtime arrows stay anonymous, `filter`/`after` spans and
CCN match the source expectations.

Change the shared mixin/state owners together so runtime-arrow versus
function-type context is coherent; do not ship the raw first-character brace
heuristic. Preserve binding/object ownership and the public facade. Add
discriminating public cases in a cohesive small module; leave unrelated large
existing test files byte-identical where practical. Trace JS, TS, JSX/TSX and
Vue script consumers (`.cjs`/`.mjs` reuse). Apply independent refactoring before
commit and rerun affected proof. Reconcile awareness of PR533/PR534's authored
dependence on expression-body state without delivering those stories' outcomes.

Boundedness: one object-versus-typed-body recognition rule with coupled readers
and one semantic regression loop. Splitting by extension would leave conflicting
rules or conceal typed-body regressions. No standalone preparatory abstraction
slice is needed. A correct local result is a safe stopping point; it remains
unmerged pending performance and final delivery gates.

### 3. Meet the strict performance budget without weakening semantics
Type: Behavior
Status: done
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, running affected and broader
fixed-source workloads shows each upper paired 95% slowdown bound strictly below
3% versus current target and the frozen `2e21e404c1ab9f4e0b49acc80816e816629d0670`
starting baseline. A broad pass cannot hide an affected failure. Preserve the
raw proposal's inconclusive affected interval [2.0154%, 4.8117%] as diagnosis,
not candidate evidence.

Replay the historical harness/corpus through the verified PR531 README recipe
(or the hash-checked cache copy when still present). Freeze additional new
regression inputs before timing; label and hash additions, retaining the
original 164-hash manifest unchanged. Confirm imported checkout paths and
public metric tuples before timing. Use the same Nix interpreter, at least 20
alternating warmed pairs, batches at least one second, and record
headroom/load, all durations, medians, variability and intervals. Run literal
`paired.py --baseline <frozen-checkout> --candidate <candidate-checkout> --workload api --out <result>`
and the corresponding `cli` workload through Nix. Retain exact resolved
commands in execution evidence; scratch or cache paths alone are insufficient.

Profile only when needed, then simplify the shared decision rather than adding
per-case fast paths. Re-run slice 2's affected semantic proof after optimization.
Noise or an upper bound of 3% or more is unresolved/failing; fix a prospective
quiet retry protocol rather than timing repeatedly for a pass. Refresh target
changes and remeasure only invalidated comparisons. This independent performance
loop may take longer than implementation; no invented timeout applies.

### 4. Deliver the qualified correction through PR531
Type: Behavior
Status: planned
Proof: final contributor-head SHA and accepted push, green current required CI,
exact-head guarded merge receipt, and one acknowledgement URL.

Behavior: given the corrected/refactored candidate with semantic and performance
proof, publish necessary corrections to the contributor branch without force or
lost history, then verify final head/base/tree and all required checks before
an exact-head guarded hosted merge. Run full pytest and make on the final
corrected revision. Any code/upstream change invalidates affected evidence;
recheck before merging. Refusal keeps the correction recoverable and unmerged.

Follow the source's [complete-cycle delivery and performance contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md)
for final checks and retained evidence; its review-only 15-minute triage rule
does not re-defer this selected technical correction. Hosted merge is not a
Dough Land operation. Execution must preserve this contributor-first delivery
constraint when choosing its publication mode; preparation's trunk landing
contains only planning records. After confirmed merge, the executing workflow
must have the source's explicit acknowledgement authority before posting one
thank-you that mentions material corrections; check for a prior receipt to
avoid duplicates. Retain commands, revisions, metrics, timings and URLs in the
existing review evidence/discussion, then complete the story's ordinary wrap-up.

Boundedness: one qualified contribution reaching its intended users and author,
with separate hosted wait time. Pending CI, merge or acknowledgement leaves
this slice unfinished, not delegated to multiline/ternary/parameter siblings.

## Plan review

Slice-plan refinement was not needed: slices separate a permitted early external
probe, one coupled semantic rule, its independent measured performance loop, and
hosted delivery. The cumulative design keeps one expression-arrow/brace owner
pair and rejects the raw first-character heuristic. No remaining slice-specific
concern was found; contributor publication availability is bounded by the early
probe rather than assumed from readable metadata. No human decision is currently
pending.

## Execution progress

### Slice 1 accepted proof (contributor route)

Promise: authorized contributor delivery route remains viable.

Observed (2026-10-27 execution):
- PR531 OPEN, not draft, MERGEABLE/clean; author FabioMalpezzi; title still
  object-in-arrow theme.
- Head `FabioMalpezzi/lizard:pr/js-object-in-an-arrow-function-body` at
  `47aa35735cccba962e507edb179eae1154ab6ad2` (matches seed); `maintainerCanModify`
  true; patch still `_expression_body` / `_opens_an_object` in
  `javascript_functions.py`, `typescript_states.py`, `testES6.py`.
- Base ref OID still recorded `01afe9c…`; live `origin/master` tip
  `db53a9a…` (ahead). Slice 4 must refresh against current target when publishing.
- Authenticated as `terryyin`; non-mutating dry-run write probes to the
  contributor head accepted (same-SHA up-to-date; different-SHA non-FF reject;
  throwaway-ref dry-run new branch). Head tip unchanged after probes.
- Authorized route for slice 4: push corrections onto that contributor head
  under `maintainerCanModify`. Preview is not accepted publication.
- Product unchanged. Story obligations: none opened.

Learnings: fork API `push:false` alone is not a refusal when
`maintainerCanModify` is true — dry-run write probes decide. Sibling OPEN PRs
#533/#534 claim to build on #531 (context only).

### Slice 2 accepted proof (object-arrow / typed-body)

Promises: object expression bodies and operator object operands; function-type
return annotations keep real bodies; controls and shared consumers.

Implementation (uncommitted until this delivery):
- `javascript_functions.py`: set `_expression_body = token != '{'` on runtime
  arrows (including already-pushed paren-param forms); clear on pop/abandon.
- `typescript_states.py`: `{` opens a nested body only when
  `started_function and not _expression_body`; otherwise `read_object()`.
- `typescript_declarations.py`: `_after_function_type_params` keeps
  `(...) => ReturnType` inside the annotation so the signature's real `{` is a
  function body. Simple-type `=>` still returns for runtime arrows
  (`(): void => {}`).
- New `test/test_languages/testArrowObjectBoundaries.py` cohesive public API
  module. Large `testES6.py` left untouched. No `_opens_an_object` heuristic.

Proof (coordinator + refactor reconfirmation):
- `nix develop -c python -m pytest test/test_languages/testArrowObjectBoundaries.py`
  → 6 passed (JS/TS/JSX/TSX/Vue object arrows; operators; string/template/
  boolean typed returns; block/paren/JSX controls).
- Focused consumer suite (JS/TS/ES6/JSX/TSX/Vue + new module) → 363 passed,
  5 skipped.
- `reproduce.py --checkout <execution>` → 64 cases, failures: [], exit 0.
- Consumer-trace: TypeScriptReader/JavaScriptReader/TSX/Vue run; `.cjs`/`.mjs`
  reused; other-language expression-body names excluded.

Refactor learning: gating `_expression_body` on "newly created" left paren-param
ternary object arrows wrong; always set from body token instead.

Story obligations: none opened. Performance (slice 3) and PR531 delivery
(slice 4) remain.

### Slice 3 accepted proof (performance budget)

Promise: each upper paired 95% slowdown bound strictly below 3% versus current
target and frozen baseline `2e21e404…`.

Candidate `4714fc25…`. Current target refreshed to `901068b8…`. Frozen
baseline checkout verified. Harness SHA `3064d8d7…7bf5`; manifest
`a224a3af…2411` (164 hashes unchanged); no corpus addition.

| Comparison | Paired 95% CI | Upper | Gate |
| --- | --- | --- | --- |
| API vs master | [-0.685%, 0.491%] | 0.491% | pass |
| CLI vs master | [-1.153%, 0.652%] | 0.652% | pass |
| API vs frozen | [-0.238%, 1.578%] | 1.578% | pass |
| CLI vs frozen | [-0.438%, 1.003%] | 1.003% | pass |

Evidence (this delivery): `docs/reviews/fabio-malpezzi-20261027/pr531/corrected-vs-{master,frozen}-{api,cli}.json` (+ logs), `corrected-slice3-{summary.json,prelaunch.txt,timeline.log}`. Historical `531-api.json`/`531-cli.json` untouched. Raw proposal interval retained as diagnosis only. No product-code change; no optimization required. Semantic proof reconfirmed after timing.
