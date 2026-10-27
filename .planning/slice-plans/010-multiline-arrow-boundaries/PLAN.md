# Preserve multiline arrow expressions and surrounding statement metrics

**Identity:** SEED-014#multiline-arrow-boundaries
**Source:** [Refined story](../../seeds/SEED-014-multiline-arrow-boundaries.md#multiline-arrow-boundaries)

## Outcome and boundaries

Users analyzing JavaScript, TypeScript, JSX/TSX and Vue script content get
correct public function names, start/end lines, CCN, NLOC and token ownership
when an expression-bodied arrow spans multiple lines or sits beside ASI and
following statements. Valid next-line starters and previous-line-ending
operators keep the expression open; genuine next statements transfer metrics
to the enclosing function. Following methods and functions stay separate.
Parenthesized expressions, comments, case labels, block bodies and semicolon
controls remain correct.

The source's key examples and
[PR533 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md)
are the acceptance input. The frozen
[reproduce.py](../../../docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py)
checks explicit names, start/end lines and CCN (plus NLOC/token count for the
reported and semicolon controls) across the retained corpus.

Excluded commitments: object-expression and typed function-body outcomes
(SEED-013), ternary-colon arrow ends (SEED-015), arrow parameter bindings
(SEED-016), ENS-only performance, a full JS/TS grammar, and new language
support. No sibling is a formal blocking supplier. No product rejection rule
follows merely from an unlisted valid form. This is the technical follow-up
after a review that exceeded its 15-minute correction cutoff; that cutoff is
not a time limit for this plan. No project-specific numeric slice limit was
supplied.

Preparation only: all slices below are planned. This record neither Takes the
story nor authorizes implementation, contributor writes, merge or comments.

## Existing owners and architectural direction

PFE: change the existing expression-arrow and newline/semicolon boundary
ownership in `lizard_languages/javascript_functions.py`
(`JavaScriptFunctionsMixin`) and `lizard_languages/typescript_states.py`
(`TypeScriptStates`). Today `_state_global` ends a started function on every
newline or `;`, which mis-owns case-returned arrows, class-field ASI and next
statement metrics. TypeScriptReader composes those owners; TSX and Vue script
reuse TypeScriptStates / TypeScript tokenization; JavaScript inherits the same
path; `.cjs`/`.mjs` alias the same reader. Do not add a parallel metric owner
or PR-specific parser switch.

The raw proposal's incomplete next-line continuation tables and unconditional
per-token state snapshots are the rejected shapes: they break valid `*`, `<`,
`>`, `**`, `(`, `[` and tagged-template continuations and fail the affected
performance gate. Prefer one coherent expression-continuation versus statement
boundary that preserves binding, object and declaration ownership already
present in those mixins, without costly bookkeeping on every token.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for CCN, NLOC, language reader and function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no new North Star topic
or ADR is warranted. No architectural exception is proposed.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/preserve-multiline-arrow-expressions-and-surroun`,
revision `ea814c0bdae4a803deab5e9c11d353869b3835fe`, observed 2026-10-27.

- Ran `nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/preserve-multiline-arrow-expressions-and-surroun`.
  Summary: 150 cases; story-owned failures include `reported.*`,
  `async_field.*`, `no_semicolon.*`, `postfix_new_statement.*`,
  `condition_next_statement.*`, `blank_comment.*` and Vue
  `reported`/`async_field`. Target already passes the listed continuation
  controls (`multiplication`, `call_continuation`, and siblings). Remaining
  failures are inherited PR531 object/typed-body cases deferred to SEED-013.
  This public consumer demonstrates the missing multiline/ASI behavior;
  existing green suites alone do not prove the correction. Add discriminating
  red/green cases in slice 2; do not claim all existing guards independently
  discriminate it.
- Inspected `TypeScriptStates._state_global` newline/`;` pop of
  `started_function` and `JavaScriptFunctionsMixin._arrow_function` /
  `_expecting_func_opening_bracket`. Confirms the shared owners named above;
  coherent continuation-versus-statement context is missing, not a second
  reader.
- Hash-checked
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py` as
  SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`
  (immutable historical harness). Manifest SHA256
  `61d9152bb62c6e9c43eb4d0ce77cc3f65f12f4988506c02a85cf2eedca393ab8` matches
  the checked-in `manifest.json`. Ran
  `printf '1\n' | PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py worker /Users/terryyin/git/lizard/.worktrees/preserve-multiline-arrow-expressions-and-surroun 0 api`
  → `{"seconds": …, "files": 150}` against the retained corpus. This settles
  replay reachability for the affected API workload, not correctness or timing
  acceptance of any future candidate. Reconstruct from Git via the PR533 README
  recipe if cache files disappear; no cache path is a delivery dependency.
- `gh pr view 533 --repo terryyin/lizard` returned `OPEN` at head
  `15423852c04aed302f684395fe9522251d88a2f2`, targeting master (base
  `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`). Read-only metadata is not
  accepted publication; slice 1's early probe owns permission and route.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Case/ASI arrow ends and next-statement metric ownership | Slice 2: new public API regressions fail before correction and pass after it in JS/TS/JSX/TSX and Vue |
| Valid multiline continuations stay intact | Slice 2: `*`, `<`, `>`, `**`, call, subscript, tagged-template, operator-ends-line, parenthesized, comment and block-body controls |
| Controls and shared consumers | Slice 2: focused consumer suites and retained story-owned reproducer cases exiting clean |
| Strict affected and broad throughput | Slice 3: fixed-corpus warmed paired measurements, each upper 95% slowdown bound strictly below 3% versus current target and frozen baseline `2e21e404c1ab9f4e0b49acc80816e816629d0670` |
| Final corrected revision accepted and acknowledged | Slice 4: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Use `lizard-language-support` for reader/test changes. Public `analyze_file` /
`analyze_source_code` results own semantic proof; token tests can support
representation contracts but do not replace it. Mock only filesystem
boundaries. Inspect every reached consumer and record run/reused/excluded with
reasons. Independent post-change refactoring runs through the execution
workflow before delivery; preserve capability names and keep author tests in a
cohesive small module below 250 lines while leaving unrelated large test files
unchanged.

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
PR533's entire patch/discussion/checks and current target, verify the recorded
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

### 2. Preserve multiline arrow and next-statement metrics through shared readers
Type: Behavior
Status: done
Proof: red/green public regressions for the source examples and controls, plus
the focused consumer command and story-owned retained reproducer cases exiting
clean.

Behavior: given case-returned one-line arrows, a multiline ternary expression
body, a semicolon-free class field beside a following method, ASI after a
complete expression, postfix `++` as a new statement, and conditions belonging
to the next statement, analysis ends each expression-bodied arrow on its
expression's last line with correct CCN, NLOC and token ownership and assigns
the next statement's metrics to the enclosing function. Given a valid
continuation (`*`, `<`, `>`, `**`, call `(…)`, subscript `[…]`, tagged
template, or previous-line-ending operator), the arrow spans both lines.
Parenthesized, comment, case-label and block-body controls keep working; Vue
script offsets stay correct.

Change the shared mixin/state owners together so expression-continuation versus
statement-boundary context is coherent; do not ship the raw incomplete
continuation tables or unconditional per-token snapshots. Preserve binding,
object and declaration ownership and the public facade. Preserve genuine
function-type return annotations where this path already does; reconcile any
separately accepted SEED-013 object-boundary fix when integrating, without
delivering that story's deferred outcomes. Add discriminating public cases in a
cohesive small module below 250 lines; leave unrelated large existing test
files byte-identical where practical. Trace JS, TS, JSX/TSX and Vue script
consumers (`.cjs`/`.mjs` reuse). Apply independent refactoring before commit
and rerun affected proof.

Boundedness: one continuation-versus-statement recognition rule with coupled
readers and one semantic regression loop. Splitting by extension would leave
conflicting rules or conceal ASI/continuation regressions. No standalone
preparatory abstraction slice is needed. A correct local result is a safe
stopping point; it remains unmerged pending performance and final delivery
gates.

### 3. Meet the strict performance budget without weakening semantics
Type: Behavior
Status: done
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, running affected and broader
fixed-source workloads shows each upper paired 95% slowdown bound strictly below
3% versus current target and the frozen `2e21e404c1ab9f4e0b49acc80816e816629d0670`
starting baseline. A broad pass cannot hide an affected failure. Preserve the
raw proposal's failing affected interval [5.2695%, 11.8135%] as diagnosis, not
candidate evidence.

Replay the historical harness/corpus through the verified PR533 README recipe
(or the hash-checked cache copy when still present). Freeze additional new
regression inputs before timing; label and hash additions, retaining the
original 250-hash manifest unchanged. Confirm imported checkout paths and
public metric tuples before timing. Use the same Nix interpreter, at least 20
alternating warmed pairs, batches at least one second, and record
headroom/load, all durations, medians, variability and intervals. Run literal
`paired.py --baseline <frozen-checkout> --candidate <candidate-checkout> --workload api --out <result>`
and the corresponding `cli` workload through Nix. Retain exact resolved
commands in execution evidence; scratch or cache paths alone are insufficient.

Profile only when needed, then simplify the shared decision rather than adding
per-case fast paths or restoring unconditional per-token bookkeeping. Re-run
slice 2's affected semantic proof after optimization. Noise or an upper bound
of 3% or more is unresolved/failing; fix a prospective quiet retry protocol
rather than timing repeatedly for a pass. Refresh target changes and remeasure
only invalidated comparisons. This independent performance loop may take longer
than implementation; no invented timeout applies.

### 4. Deliver the qualified correction through PR533
Type: Behavior
Status: done
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
this slice unfinished, not delegated to object/ternary/parameter siblings.

## Execution evidence

Execution: story-branch `claude/preserve-multiline-arrow-expressions-and-surroun`
in this worktree; claim published at `428bb5932c912e6913deac2585de392dcd1d36ce`
(origin). Agent: Rina-chan.

**Slice 1 (2026-10-27, no product change):** `gh pr view 533` → OPEN, head
`pr/js-arrow-function-ends-with-its-line` @ `15423852c04aed302f684395fe9522251d88a2f2`
(FabioMalpezzi/lizard, cross-repository), `maintainerCanModify: true`; `gh`
authenticated as `terryyin` (repository owner). Fetching the contributor ref
returned the same SHA; `git push --dry-run` to it reported up to date. Route is
viable. Head still contains PR531's head `47aa357` (SEED-013's source): merging
PR533 unchanged would make GitHub mark PR531 merged. Slice 4 must settle that
ordering with SEED-013's concurrent execution before the hosted merge.

**Slice 2 acceptance:** independent source-only reviewer (fresh agent, input
only the story section) returned commitments C1–C63/D1–D4/X1–X5. Mapping:
C12–C15 → `test_case_returned_arrows_and_multiline_ternary_body` (JS/TS/JSX/TSX)
and `TestVueScriptArrowBoundaries.test_reported_example_keeps_script_offsets`;
C17–C19 → `test_class_field_arrow_beside_following_method` (+Vue);
C20–C23 → `test_asi_matches_semicolon_except_the_semicolon_token` (4 vs 3 tokens);
C27 → postfix `++` test; C6 → `test_next_statement_condition_belongs_to_enclosing_function`;
C24–C26, C28 → `TestExpressionArrowContinuation` (`* < > ** - instanceof in`,
call, subscript, template, `.`, `?.`, `?:`, operator-ending line, generic split);
C29–C32 → parenthesized, blank/comment, closing-brace and block-body tests;
C40 → four extensions + Vue, CLI `.mjs` observed by the implementer;
C45/C46 → new `test/test_languages/testJavaScriptArrowBoundaries.py` (22 tests,
<250 lines), no existing test file changed. Red before: 16/22 fail on a
`428bb59` export (6 continuation guards pass). C47–C53 belong to slices 3–4.
Coordinator probes (base vs candidate) found and returned a generic-split comma
regression and a missing same-line object member; both corrected and tested.
Curried `(a) => (b) => a\n + b` now owns 11 tokens (baseline 10 was wrong).
Reproducer `nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py --checkout <worktree>`:
70 → 44 failures, all `inherited_*` object-literal bodies (SEED-013), output
byte-identical to baseline. Focused command: 379 passed, 5 skipped. Full
`nix develop -c python -m pytest`: 1818 passed, 7 skipped.
Design: `TypeScriptReader.preprocess` offers each line's first code token to the
innermost open expression arrow (`context.expression_arrows`) before any counter
charges it; `end_expression_before` applies an ASI rule (`ends_expression` /
`starts_statement`) without a continuation allowlist; `,` ends object-member
arrows. Implementer's informal timing ≈ +3% median (noisy, not the harness):
slice 3 must treat it as a real risk.
Independent refactor (`## REFACTOR COMPLETE`): moved the ASI predicates and
`end_expression_before` into `JavaScriptFunctionsMixin` beside the arrow
registration (behavior unchanged; focused 379 passed, reproducer 44 inherited).
Coordinator final: full pytest 1818 passed, 7 skipped; `nix develop -c make` exit 0.

**Slice 3 (performance) evidence:** trunk advanced to `ac2b454` (SEED-013 /
PR531 merged), so the candidate first merged it (`b5a25db`): trunk's
`_expression_body` and this story's open-arrow registration became one flag
closed through `_close_expression_body`; `case`/`default` label colons stay
runtime syntax (trunk's function-type annotation reading otherwise swallowed a
case-returned arrow; trunk alone fails that). Reproducer after merge: 0/150
failures (trunk alone: 42, all SEED-014 cases). PR531 merging also settles
slice 4's ordering concern: PR533's `47aa357` is already on trunk.
Harness `pr533/paired.py` SHA256 `3064d8d7…bf5`, manifest `61d9152b…a8d`, all
250 corpus hashes re-verified. Snapshots (`git archive`) under
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/seed014/`: `target-ac2b454`,
`frozen-2e21e40`, `candidate-<sha>`; result JSONs `<sha>-vs-<base>-<workload>.json`
keep every duration. Command per comparison (host Territory.local, Python 3.9.11,
Nix `.venv`, run serially):
`PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py --baseline <base> --candidate <candidate> --workload {api,cli} --pairs 20 --seconds 1.3 --out <json>`.

| Candidate | API vs ac2b454 | CLI vs ac2b454 | API vs 2e21e40 | CLI vs 2e21e40 |
| --- | --- | --- | --- | --- |
| b5a25db | +2.62%, CI [1.85, 3.03] fail | CI [-0.36, 1.17] | +2.86%, CI [2.61, 3.28] fail | CI [-0.46, 1.19] |
| 658e7d5 | +2.35%, CI [1.71, 3.10] fail | CI [-3.08, 0.66] | +1.73%, CI [1.32, 2.95] | CI [-12.7, 5.36] unresolved (load 10.2) |
| 8973153 | −5.45%, CI [−6.73, −4.06] pass | −1.04%, CI [−0.82, 2.44] pass | −4.18%, CI [−5.53, −4.10] pass | +1.95%, CI [0.70, 2.67] pass |

Load during the 8973153 runs 3.7–5.5 (16 CPUs). Profile diagnosis: the
remaining added cost (~32 more functions and ~45 more state machines per pass)
is the more correct parse; it is more than offset by trunk-shared per-token
savings (direct `CodeStateMachine.__call__`, guarded pending-binding,
declaration and object-member handlers, Vue script filtering as a reader hook).
Each optimization kept analysis output byte-identical (150 corpus files and 727
recorded test snippets) and the reproducer at 0 failures. No retry was used to
seek a pass; each measured candidate is a different revision.

**Slice 3 renewal after trunk `c3f7175` (SEED-012 regex context, PR528):**
clean merge `3117601`; full pytest 1839 passed, reproducer 0/150, authored 5
pass, probes unchanged (line-start `/` continues as division per ASI). Gate for
`3117601` (load 3.4–6.7): API vs c3f7175 −5.98%, CI [−6.28, −5.13]; CLI vs
c3f7175 +0.45%, CI [−0.37, 1.26]; API vs 2e21e40 −8.62%, CI [−8.69, −7.74];
CLI vs 2e21e40 +1.37%, CI [0.47, 1.82] — all pass.

**Slice 4 delivery:** contributor candidate `b6074d8` (merge of master
`c3f7175` into PR head `15423852`, analyzer files byte-identical to `3117601`,
`testES6.py` identical to master, author's five tests moved verbatim to
`test/test_languages/testJavaScriptArrowLines.py`). Local in that tree: full
pytest 1844 passed, 7 skipped; `make` exit 0; reproducer 0/150. Pushed without
force (`1542385..b6074d8`) after Terry authorized the external push (the
session classifier had refused it first). Hosted run `40509297062` on exact
head `b6074d8`: all 14 `build` jobs (ubuntu-24.04 and windows-latest × Python
3.8–3.14) and `changes` passed; `web` skipped by its path filter. Master
unchanged at `c3f7175`; PR `MERGEABLE`/`CLEAN`. Guarded merge
`gh pr merge 533 --merge --match-head-commit b6074d83e197127b2ffdef61d3f6dd20134d0567`
(run after Terry's explicit authorization) → merge commit
`a2d2ab778d03cbb9ea96d243ab7533105d304eb2`. Single acknowledgement (no prior
comments): https://github.com/terryyin/lizard/pull/533#issuecomment-6443273326.
Story branch then merged trunk (`a2d2ab7`, plus unrelated website-release
work); analyzer files unchanged by that merge; full pytest passed.

## Execution complete

Product advice: no correction plan needed. The aggregate change (`822bd05`,
`b5a25db`, `658e7d5`, `8973153`, `3117601`, delivered via PR533 `a2d2ab7`)
meets the story's reported cases, continuation controls, consumers, proof and
strict performance gate; recorded gaps G1–G3 keep their dispositions.
Recommend a separate Vue story: a `<template>` before `<script>` shifts every
reported line (G3; independent of arrows). Keep `_ternary_depth` naming and
statement-end reset with SEED-015, which already plans to rework it. The comma
ambiguity outside objects (G1) stays a grammar-level limitation, not a queued
correction. Process findings: DD-002 occurrence, DD-009, DD-010 in `DearDough.md`.

## Story obligations

### G1. Comma before a line break outside objects keeps baseline ownership
Reported: slice 2 — "Outside objects, an arrow followed by a trailing comma still gives the next line's first token to the arrow."
Story clause: "Public analysis ends each expression-bodied arrow on its expression's last line"
Disposition: excluded "no full JavaScript/TypeScript grammar"

### G2. Bare-parameter arrow start line follows its body
Reported: slice 2 — "For `x =>` the start line comes from the first body token"
Story clause: "names, lines, CCN, NLOC and token counts"
Disposition: excluded "arrow parameter-binding work remains with"

### G3. Vue template before script shifts reported lines (discovered in slice 2 acceptance)
Reported: slice 2 — "Newlines outside `<script>` are dropped, so a `<template>` placed before the script shifts reported lines."
Story clause: "Users measuring JavaScript, TypeScript, JSX/TSX and Vue code"
Disposition: no user cost "Public analysis ends each expression-bodied arrow on its expression's last line": coordinator probe gave `a` 2–4 instead of 5–7 identically on base `428bb59` and candidate for a plain function; it is independent of arrows and unchanged by this work, so the arrow goal adds no cost (separate Vue follow-up recommended at wrap-up)

## Plan review

Slice-plan refinement was not needed: slices separate a permitted early external
probe, one coupled semantic rule, its independent measured performance loop, and
hosted delivery. The cumulative design keeps one expression-arrow/newline owner
pair and rejects the raw incomplete continuation tables and per-token
snapshots. No remaining slice-specific concern was found; contributor
publication availability is bounded by the early probe rather than assumed from
readable metadata. No human decision is currently pending.
