# DearDough Process Findings

Source maintenance on 2026-10-28 retained collected evidence before shortening at Open Dough `32b5ae0b4414d755bcde4ef6cd822774d7450771:docs/maintainer/findings/`. The complete post-local source is `1fab8f40c7577f53b7736c79ea3f91b67a1fd0ca:DearDough.md`; linked source records share that revision. Unknown execution releases and local facets remain qualified.

The existing DearDough log currently holds both shared and project findings.
Project-owned entries DD-006, DD-007, DD-008, DD-011, DD-012 and DD-016 stay here
with their original evidence; DD-009 is a host-policy observation, DD-010 is a
successful practice, and DD-013 is qualified command-output practice. DD-004
concerns application of this project's performance acceptance gate; it remains
qualified and is not evidence of a shared performance mechanism defect.

Local maintenance (2026-11-01) retires DD-003’s repeated shell setup:
`be8d3c273fd475a0cedc4d1e9da58ff596c6db0c` adopted a dependency stamp;
controlled cold/warm/overlapping-warm/changed-input/failure entries supplied
direct outcomes. Current `shell.nix` retains the mechanism. All eight original
executions and their exact provenance, including the pre-adoption 0.3.73 run,
remain recoverable at `eebecc3e196a26f6f6b6a41df66ef97025a3361a:DearDough.md`.
This is bounded retirement of the demonstrated repeat-install problem, with
no latency claim or new watch. The other local incidents have repaired seams,
usable probes or unresolved host/attribution limits, and justify no new story.
Existing Taken work, direction and queue order stay unchanged.

Local maintenance (2026-11-03) classifies rows added since then and the
unclassified DD-014/DD-015. DD-398 is shared: the contributor delivery recipe
publishes the whole execution branch; PR548's selective staging is one
execution's mitigation. DD-013's SEED-025 row has no project facet, because
`website/frontend/reports/` is already gitignored. DD-010 and DD-015 remain
practice confirmations. DD-014 is project-owned: `lizard-process-pr` section 5
(since `f0887ec`, before the occurrence) already blocks unresolved required
checks. No repair was adopted. Later hosted merges PR525 (`b97f795`) and PR548
(`2416c37`) waited for all 32 exact-head check runs to succeed, so DD-014 stays
open without a new story. No retirement, story or queue change.

Local maintenance (2026-11-07) restores the `SEED-029#ocaml-support` coordinator-probe occurrence (`e1f37ed46`, whose commit message named DD-010) from DD-012 to DD-010 (`29d0f1b74cbca7c06882212196eeb4b019a4d519:DearDough.md`), and classifies DD-399 through DD-403. DD-399 (source-only review timing and plan-narrowed formatter proof), DD-401 (Darwin `mktemp` template and `git rm` `stagedPaths` in `fresh-increment`), and DD-402 (marking a merge-gated hosted-CI commitment `observed:` before the receipt/merge head) concern shared Open Dough guidance. DD-400 is a shared practice confirmation for `progress: true` on hosted-proof slices (ODF-275). DD-403 is a project-local performance-probe practice confirmation. No retirement, new local story, or queue change.

Local evidence compaction recovery: `6cd0c9531a3428fc922d5a58b9b3e3299a01b16a:DearDough.md` preserves all original occurrence rows and assessment history.

<a id="dd-001--codex-stream-can-yield-before-exposing-observer-identity"></a>

## ODF-376 — Codex stream can yield before exposing observer identity

Former local code: DD-001.

The prescribed Codex binding prints the observer directory only in its initial
tool result. If checkout setup or provider discovery outlasts that yield, a later
`CI_OBSERVER` line is parsed without exposing the identity to the coordinator.
Consider emitting identity when it becomes available, within the existing binding.

### Occurrences

Distinct executions, original timestamps/releases/tools, observed effects and qualified inferences are [retained at ODF-376](https://github.com/terryyin/open-dough/blob/32b5ae0b4414d755bcde4ef6cd822774d7450771/docs/maintainer/findings/ODF-376.md); rereading adds no occurrence.

<a id="dd-002--hosted-proof-slices-have-no-guarded-increment-that-stays-unfinished"></a>

## ODF-275 — Hosted-proof slices have no guarded increment that stays unfinished

Former local code: DD-002.

A slice whose own proof needs a hosted CI receipt of its publication must be
published before it can be accepted. Guarded fresh delivery always marks the
selected slice `done` and correctly refuses while that slice's receiving
obligation is open, so the coordinator fell back to a manual agent commit plus
existing-candidate `deliver`, then a later plan-only fresh increment to mark it
done. Consider a documented fresh-increment path that publishes without the
status transition.

### Occurrences

Distinct executions, original timestamps/releases/tools, observed effects and qualified inferences are [retained at ODF-275](https://github.com/terryyin/open-dough/blob/32b5ae0b4414d755bcde4ef6cd822774d7450771/docs/maintainer/findings/ODF-275.md); rereading adds no occurrence.

## DD-004 — PR merge preceded a missing affected-workload performance gate

Apply the affected-workload gate before exact-head merge. For `SEED-010#review-fabio-malpezzi-prs` (Codex, execution time/release unknown, guidance baseline `2e21e404`), PR527 merged at `e871ae448` before the nested-function, async, match, continuation and class controls in `af27c910:.planning/slice-plans/004-fabio-malpezzi-pr-reviews/527-review.md` and plan obligation G1 were settled. The later 22 controls passed (upper 95% slowdown bound −0.6803%, `docs/reviews/fabio-malpezzi-20261027/controls/`). The project's workload-selection gap preceded merge and remains a distinct, qualified finding.

## ODF-397 — Native Git bypassed the installed backlog merge adapter

Former local code: DD-005.

Resolve the actual checkout's installed adapter before every merge touching the
backlog or done records, including clean fast-forwards and contributor branches.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-397](https://github.com/terryyin/open-dough/blob/692bf5162b86fca9cbebc1c57c98efd227198e9d/docs/maintainer/findings/ODF-397.md); source recovery remains in this project’s pre-compaction revision.

## DD-006 — `python -c` import probes contaminate via argv0

Use a real script for checkout-isolated import probes. In `SEED-013#arrow-object-boundaries` (Cursor auto, 0.3.71, event time unknown), `lizard.py` resolved `abspath(sys.argv[0])` against cwd under `python -c`, causing false import-path failures. The harness's real `paired.py` workers were sound. See agent `89783fe2-57ff-41aa-b641-0cef9d98622b` and `docs/reviews/fabio-malpezzi-20261027/pr531/corrected-slice3-prelaunch.txt` (2026-10-27T11:47:50Z).

## DD-007 — Expression-body flag gated on “newly created” missed paren-param arrows

Derive arrow expression-body context from the body token on every `_arrow_function` path. In `SEED-013#arrow-object-boundaries` (Cursor auto, 0.3.71, event time unknown), `if created:` skipped arrows already pushed for `(...)` parameters. Refactor agent `c1df6a2a-bff5-455b-a7a1-638d227d1f52` fixed it before delivery (plan 009 slice 2, `testArrowObjectBoundaries.py` paren-param ternaries, proof 6/363 and reproducer 64).

## DD-008 — Fork `push:false` alone is not a contributor-route refusal

Combine `maintainer_can_modify` and a divergent-SHA dry-run push before rejecting a fork route. For `SEED-012#regex-literal-context` (2026-10-27T18:40:00+08:00, Cursor auto, 0.3.71 via `01afe9c`), PR528's fork said `push:false`, MCM was true and only the divergent dry-run proved access. No false delivery stop followed. See agent `7093d051-eac5-4592-b08e-cdb07e4ea249`, plan 008 slice 1.

## DD-009 — Plan-authorized contributor writes refused by host permission classifier

A plan slice (hosted contributor delivery) authorized pushing corrections to a
fork PR head, a guarded hosted merge and one acknowledgement, but the host's
auto-mode permission classifier refused the fork push ("Create Public Surface")
and later the merge ("Merge Without Review") until the human re-authorized each
in conversation. Consider obtaining explicit human authorization for every
outward write a plan slice needs at execution start (or a project permission
rule), so the slice does not stop twice mid-delivery.

### Occurrences
- Execution: SEED-014#multiline-arrow-boundaries
  - Timestamp: unknown (2026-10-27, after slice 3 gate on 3117601 and before PR533 merge a2d2ab7)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: plan `.planning/slice-plans/010-multiline-arrow-boundaries/PLAN.md` at `3cdc283` (slice 4 delivery note); dashboard unfinished handoff receipt 48eaccfb-ca45-4cd5-a7aa-dfecbbf15443; push `1542385..b6074d8` and merge `a2d2ab7` only after user replies "I authorize you to do it" / "you are authorized to run it".
  - Observed effect: two human round trips and a stopped CI observer during an otherwise ready delivery; no correctness impact.
  - Inference: plan text and launch instruction are not treated as authorization by the host's classifier; an up-front confirmation would avoid the stops.

## DD-010 — Coordinator base-versus-candidate ambiguity probes caught gaps green tests missed

Coordinator base/candidate probes found gaps that green parser tests missed. Keep directed opposite-role controls through semantic acceptance before expensive qualification.

- `SEED-014#multiline-arrow-boundaries` (2026-10-27, time unknown, Claude Code Opus 5.5, 0.3.71 via `01afe9c`): generic comma continuations and one-line object members prompted two return rounds and discriminating tests before `822bd05` (`3cdc283`, plan 010 slice 2; `testJavaScriptArrowBoundaries.py`).
- `SEED-024#resolve-issue524-label-types` (2026-11-02 UTC, time unknown, Codex, 0.3.80 at `a10a744`, 0.3.81 after `0e352b7`): contextual calls, operator handoff and export-default callbacks became regressions before merge (`61e23de`, plan 016 G14–G17; `7fac94f:test/test_languages/testTypeScriptAssertionRoles.py`). Final 490 cases preserved 200 earlier cases, with old timing retained only for its own bytes.
- `SEED-029#fsharp-support` (2026-11-04, time unknown, Claude Code Opus 5.5, 0.3.82 via `210ae36`): same-line attributes and `type A … and B` false function/missing member needed two discriminating test rounds before `65c552a`/`a20ab79`. Story clauses at `b0c42aa` covered the latter even without a literal example.
- `SEED-029#ocaml-support` (2026-11-04, time unknown, same tool/release as F#): depth-zero `then`/`else`/`->` and literal one-line module examples corrected proof at `e8ecf8a`/`59ddc71` without product edits. The original depth-nested test also satisfied the bracket rule.

## DD-011 — Integrity checks must separate measured inputs from acceptance records

Separate measured-input invariants from evolving acceptance records and exclude output manifests from their own inputs. During `SEED-015#ternary-arrow-boundaries` (Codex, release unknown, guidance at `6f506711`, 2026-10-27T19:07–19:12Z observation window), qualification9_refactor found both integrity-check mistakes and fixed them before delivery. Only the owned PLAN path was permitted. Four frozen drivers, 120 timing pairs and observations stayed intact without reruns. See `e3956fa` root-performance/integrity and cache `seed015-root-737aeaf7/refactor-pre-integrity.json`.

## DD-012 — Absolute CLI corpus paths bind historical output hashes

Freeze literal CLI arguments and compare actual outputs before timing a relocated corpus. In `SEED-015#ternary-arrow-boundaries` (Codex, release unknown, guidance at `6f506711`, 2026-10-27 UTC/28 Singapore), the first EC2 freeze exited 1 before timing because the older hash embedded `seed015-optimized-8dac7b80/corpus`. Exact prefix correspondence explained the old bytes. Setup2 compared three actual identical outputs and retained failed setup/source hashes without weakening metrics. See `737aeaf` final-setup-failure/final-performance, G8, and later root-freeze/root-performance.

## ODF-264 — Unbounded discovery and CI payloads waste coordinator context

Former local code: DD-013.

Narrow source searches and project public CI responses to needed fields before
emitting them. Check-run objects repeat app permissions and endpoints for each
job; retaining raw data need not print it into coordinator context.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-264](https://github.com/terryyin/open-dough/blob/a29f625255a3368eb721249c01df96e0f98edfd4/docs/maintainer/findings/ODF-264.md); source recovery remains in this project’s pre-compaction revision.

## DD-014 — Empty `gh pr checks` is not a green merge gate

An empty `gh pr checks --watch` list after a push leaves the exact-head gate open. In `SEED-010#review-lexical-js-prs` (2026-10-28T05:56:52Z, Cursor, 0.3.71), PR546 merged at `559b8c8` for corrected head `395ae04` before the 14-job matrix appeared. Thanks waited for green run 40640907400 (conversation `9e3d53de-1aaa-43d4-a4c0-161f4d937215`, segment_000, turns ~224–230).

Project guidance `lizard-process-pr` §5 already required checks at `f0887ec`, so no repair was adopted. Later PR525 (`b97f795`) and PR548 (`2416c37`) waited for all 32 exact-head checks. Keep the application lapse open without new work.

## DD-015 — Interrupted Task recovery by inspecting uncommitted checkout

When a delegated Task is interrupted, treat the slice as incomplete, inspect the
owned checkout's uncommitted diff and plan (and remote) before accepting any
claim, and resume only remaining work without marking done on the interrupt.

### Occurrences
- Execution: SEED-010#review-lexical-js-prs
  - Timestamp: unknown (2026-10-28 slice 2; interrupts after ~10580484ms and ~3240708ms)
  - Tool: Cursor
  - Open Dough release: 0.3.71
  - Evidence: conversation 9e3d53de-1aaa-43d4-a4c0-161f4d937215 compaction segment_000; `CallDynamicTool was interrupted by the user` on slice 2 / finish-slice-2 Tasks; coordinator notes "Interrupted agent left uncommitted product and evidence; no unexpected commits. Re-establishing focused proof…"; later #545 disposition left unmerged with evidence after attempt8.
  - Observed effect: no false done status; uncommitted adapted product and evidence were recovered; slice 2 completed as an evidenced non-merge without unexpected commits from the interrupted agents.
  - Inference: practice matches installed interrupted-delegation recovery; worth repeating on long measurement Tasks.

- Execution: SEED-019#template-interpolation-conditions
  - Timestamp: 2026-10-29T06:47:00+08:00
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.73
  - Evidence: conversation b6ecc26c-f7f1-4c5f-a3aa-2a2ccfa36047 L60 after interrupted slice-2 Task; user resume at 2026-10-29T06:51:00+08:00; attempt10 had 4/6 gates finished; coordinator resumed only cumulative-api-parameters and cumulative-cli without re-timing passed gates; plan left undone until delivery.
  - Observed effect: no false done; unfinished measurement and uncommitted trim were recovered; attempt10 completed and #545 merged after proof.
  - Inference: same interrupted-delegation practice held on a long six-gate Task.

- Execution: SEED-021#dart-support
  - Timestamp: 2026-10-29T11:20:00+08:00
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.74
  - Evidence: conversation 5ae6ce13-8910-4941-95bc-b0a708f808f0; slice-6 refactor Task `0767f523-8283-42ee-adab-0477adbf550b` started 2026-10-29T10:31+08:00 and ended mid-investigation without `## REFACTOR COMPLETE`; user `resume` at 11:20; coordinator inspected uncommitted slice-6 checkout/tests, re-ran refactor Task `93688ba4-6a67-47a7-8871-06f7a165395c` (no edits), then delivered `7a5b0d7` without marking done on the interrupt.
  - Observed effect: no false done; uncommitted slice-6 product kept; refactor re-checked clean; delivery continued from remaining work only.
  - Inference: same interrupted-delegation recovery held across a mid-refactor Task interrupt plus later coordinator resume.

## DD-016 — Post-change hot-path collapse reintroduced gated per-token dispatch

Measure before consolidating dispatch on a performance-gated path. In `SEED-019#template-interpolation-conditions` (2026-10-28T16:41:29+08:00, Cursor auto, 0.3.73), refactor `5dbb84bb-1fe7-4bb6-b359-cb3585c88194` extracted per-token dispatch in `TypeScriptReader.__call__`. Attempt9 failed incremental API (95% upper +5.02%) despite correct semantics. Restoring inline `after_interpolation` cost one eligible trim and remeasurement; attempt10 passed six gates (max upper +2.449%). Plan Learnings retain the guard.

## ODF-378 — First-landing expectation blocks story-branch increment publish

Former local code: DD-017.

When a launch plants a first-landing expectation whose landing context targets
trunk, story-branch increment delivery that omits `--landing-context` still
resolves that expectation and calls `retainLandingComparison` against the
execution-branch target. Context remote/target mismatch throws before push,
leaving a committed candidate unpublished until the expectation is temporarily
cleared. Landing context is for first trunk land only; intermediate
story-branch publishes should not bind it.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-378](https://github.com/terryyin/open-dough/blob/692bf5162b86fca9cbebc1c57c98efd227198e9d/docs/maintainer/findings/ODF-378.md); source recovery remains in this project’s pre-compaction revision.

## ODF-412 — Contributor publication carries spent qualification evidence

Former local code: DD-398.

Consider separating retained execution evidence from the final net contribution
diff while preserving contributor history and immutable proof recovery. Publishing
the full execution branch makes temporary reports dominate the hosted review,
even when ordinary wrap-up will remove them afterward.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-412](https://github.com/terryyin/open-dough/blob/a29f625255a3368eb721249c01df96e0f98edfd4/docs/maintainer/findings/ODF-412.md); source recovery remains in this project’s pre-compaction revision.

## ODF-367 — Source-only review restored formatter promises the plan proof narrowed

Former local code: DD-399.

The story listed seven formatters whose `--compact` output must stay unchanged
(XML, CSV, Checkstyle, HTML, clang, MSVS, extension printers). The executable
plan's proof named only "`--xml` (or `--csv`)", and the first implementation
proof followed the plan. The independent source-only review split the list into
separate ids, and the coordinator added the missing observations before
acceptance.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-367](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-367.md); source recovery remains in this project’s pre-compaction revision.

## DD-400 — Hosted CI-proof slices published with `progress: true` until observation

For a Behavior slice whose own proof needs a hosted CI receipt of its
publication, use fresh-increment `progress: true` so the slice stays unfinished
while the published revision is observed, then mark it done after the receipt.
Related to ODF-275's missing unfinished publish path; this records the
documented progress route working for story-branch hosted proof without a
manual commit fallback.

### Occurrences
- Execution: SEED-035#publish-lizard-ci-proof
  - Timestamp: 2026-11-05T18:06:32+07:00
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.86
  - Evidence: coordinator checked `progress: true` before slice 1 delivery; progress publish `c7643ce7e` (analyzer fragments); observation on run 42016250359 then done at `7a4aff612`; slice 2 progress publish `15dc13b8d`, web-covered run 42017933530 and web-skipped run 42018498291, done observations `78e05c9f9`/`cedd44fba`; wrap-up guidance at `.agents/skills/dough-execute-plan/references/wrap-up.md` / `fresh-increment.md`.
  - Observed effect: both hosted-proof slices published without premature `done`; real-run tab and artifact receipts were accepted afterward; no manual agent-commit workaround.
  - Inference: practice confirmation that `progress: true` is the guarded unfinished publish for hosted CI-proof slices in current guidance (addresses the ODF-275 gap for this path).

## ODF-441 — Fresh-increment delivery retries stumble on Darwin mktemp suffixes and git-rm staging

Former local code: DD-401.

On Darwin `/usr/bin/mktemp`, a template like `/tmp/foo.XXXXXX.json` does not
expand the `X`s (literal fixed path). The first call can create that file; an
immediate retry hits `File exists` and leaves `$PREP` empty for the preparation
JSON. Prefer trailing `XXXXXX` then rename (or `mktemp -t`). Separately,
`git rm` leaves deletions already staged — name them in `stagedPaths`, not
`ownedPaths`, per `fresh-increment.md`.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-441](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-441.md); source recovery remains in this project’s pre-compaction revision.

## ODF-413 — Hosted-CI commitment marked observed before receipt or merge head

Former local code: DD-402.

Do not record a hosted-CI source-review commitment as observed until the
applicable exact-head checks conclude successfully on the revision the
commitment names (the merged head when the promise is merge-gated). Local
`pytest` / `make` alone do not close that observation.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-413](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-413.md); source recovery remains in this project’s pre-compaction revision.

## DD-403 — Early cost-owner performance probe avoided unnecessary NS remediation

Probe cost ownership under quiet load before changing a product owner. For `SEED-035#ns-member-name-performance` at `43a07aafa2e1d7e28955ba554ced3263baca1874` (2026-11-06T21:25:18+07:00, Cursor auto, 0.3.86), attempt5 cumulative API parameters failed at ~+4.51% upper. On the refreshed target the prior set (~+0.98%) and full candidate (~+1.41%) passed, so the as-authored `_member_name` landed with six gates under 3% and no NS remediation. See `docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice1/` and `seed035-slice2/`.

## Retention

- Highest allocated local number: 403
- Shared-feedback adoption and compaction recovery (2026-11-07): `7129516bc5bd9ba57f370a6353ffe0e10eb1c861:DearDough.md`; its rows are retained at the linked Open Dough finding owners (`483d3a1e8f0ea24b929675b9a195faca48ac8f47:docs/maintainer/findings/`).
- Shared-feedback compaction recovery (2026-11-03): `4195708bad2061ebc1c829ebdff7868605b173bb:DearDough.md`; its rows are retained at the linked Open Dough finding owners.
