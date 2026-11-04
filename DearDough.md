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

Complete the source's relevant workload selection before the exact-head merge.
A later passing measurement repairs confidence but cannot repair the ordering.

### Occurrences
- Execution: SEED-010#review-fabio-malpezzi-prs
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; installed guidance at execution baseline 2e21e404
  - Evidence: PR527 merge e871ae448d16893301856b41124aba34496e9536; execution-branch revision af27c910, .planning/slice-plans/004-fabio-malpezzi-pr-reviews/527-review.md and PLAN.md obligation G1; docs/reviews/fabio-malpezzi-20261027/controls/.
  - Observed effect: the coordinator merged before resolving the independent refactor's omitted nested-function, async, match, continuation and class timing controls. The later expanded 22-control corpus passed with upper 95% slowdown bound -0.6803%; the temporal deviation remains.
  - Inference: a passing broad benchmark and original reported example did not establish that the affected corpus covered the accepted contract.

## ODF-397 — Native Git bypassed the installed backlog merge adapter

Former local code: DD-005.

Resolve the actual checkout's installed adapter before every merge touching the
backlog or done records, including clean fast-forwards and contributor branches.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-397](https://github.com/terryyin/open-dough/blob/692bf5162b86fca9cbebc1c57c98efd227198e9d/docs/maintainer/findings/ODF-397.md); source recovery remains in this project’s pre-compaction revision.

## DD-006 — `python -c` import probes contaminate via argv0

Do not use `python -c` to verify checkout-isolated imports for Lizard performance
or multi-checkout work. Prefer a script whose `sys.argv[0]` matches the real
harness worker (or another real file path under the intended checkout).

### Occurrences
- Execution: SEED-013#arrow-object-boundaries
  - Timestamp: unknown
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.71
  - Evidence: slice 3 performance agent
    `89783fe2-57ff-41aa-b641-0cef9d98622b`; diagnosis that
    `lizard.py` uses `abspath(sys.argv[0])` so `python -c` becomes
    `abspath('-c')` and contaminates toward cwd; corrected harness-shaped
    `/tmp/probe-paired-imports.py`;
    `docs/reviews/fabio-malpezzi-20261027/pr531/corrected-slice3-prelaunch.txt`
    (2026-10-27T11:47:50Z) and gaps/learnings in that agent's return.
  - Observed effect: false import-path failures during pre-measurement probes;
    real `paired.py` workers were fine. Extra diagnosis before timing.
  - Inference: probe argv0 must match the harness, not `-c`; this is a
    project-local import bootstrap trap rather than a failing harness.

## DD-007 — Expression-body flag gated on “newly created” missed paren-param arrows

When setting runtime-arrow expression-body context, derive it from the body
token for every path that reaches `_arrow_function`, including arrows already
pushed from `(...)` parameters. Do not gate the flag on “newly created.”

### Occurrences
- Execution: SEED-013#arrow-object-boundaries
  - Timestamp: unknown
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.71
  - Evidence: independent refactor agent
    `c1df6a2a-bff5-455b-a7a1-638d227d1f52` (“Confirmed an incomplete rule:
    `if created:` skips `_expression_body` when params already pushed”);
    plan slice 2 learning in
    `.planning/slice-plans/009-arrow-object-boundaries/PLAN.md`; fixture in
    `test/test_languages/testArrowObjectBoundaries.py` uses paren-param
    ternary objects.
  - Observed effect: first slice-2 implementation left paren-param ternary
    object arrows wrong until the refactor pass cohered ownership and reran
    proof (6 / 363 / reproduce 64).
  - Inference: multiple push paths for the same runtime arrow make
    creation-gated flags incomplete; body-token ownership is the coherent rule.

## DD-008 — Fork `push:false` alone is not a contributor-route refusal

When probing a fork PR for maintainer correction, treat API `push: false` as
incomplete. Combine `maintainer_can_modify` with a non-mutating divergent-SHA
dry-run push preview before refusing the route.

### Occurrences
- Execution: SEED-012#regex-literal-context
  - Timestamp: 2026-10-27T18:40:00+08:00
  - Tool: Cursor
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: slice 1 agent 7093d051-eac5-4592-b08e-cdb07e4ea249; plan accepted proof under `.planning/slice-plans/008-regex-literal-context/PLAN.md` slice 1; PR528 `maintainer_can_modify: true`, fork `push: false`, divergent dry-run accepted.
  - Observed effect: same-SHA dry-run looked up-to-date; only the divergent preview proved the MCM write path. Route stayed viable without a false human disposition stop.
  - Inference: collaborator-permission and no-op dry-run signals understate MCM delivery; divergent preview is the discriminating probe.

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

Before accepting a parser slice, the coordinator ran a small probe script over
ambiguous forms (split generics, argument lists, one-line object members,
curried arrows) against both a base export and the candidate. It exposed a
valid-continuation regression and a missing same-line member that the
implementer's passing tests and the reproducer did not cover.

### Occurrences
- Execution: SEED-014#multiline-arrow-boundaries
  - Timestamp: unknown (2026-10-27, slice 2 acceptance before 822bd05)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: plan `.planning/slice-plans/010-multiline-arrow-boundaries/PLAN.md` at `3cdc283`, slice 2 acceptance ("generic-split comma regression and a missing same-line object member; both corrected and tested"); tests `test_generic_arguments_split_after_comma_keep_their_line`, `test_same_line_object_members_stay_separate` in `test/test_languages/testJavaScriptArrowBoundaries.py`.
  - Observed effect: two return rounds before delivery instead of a later regression; both became discriminating tests.
  - Inference: a practice worth keeping for grammar heuristics where examples are not exhaustive.

- Execution: SEED-024#resolve-issue524-label-types
  - Timestamp: unknown (2026-11-02 UTC, before implementation publication 7fac94f)
  - Tool: Codex
  - Open Dough release: 0.3.80 during probes at a10a744;0.3.81 after 0e352b7 reconciliation before publication.
  - Evidence: 61e23de:.planning/slice-plans/016-issue524-label-types/PLAN.md G14–G17;7fac94f:test/test_languages/testTypeScriptAssertionRoles.py; retained root baseline/candidate and official-parser observations.
  - Observed effect: contextual identifier calls, runtime operator handoff and export-default callbacks lost under the first annotation heuristic became discriminating public regressions before merge. The final490 cases preserve the earlier200; qualified timing for superseded bytes was retained and not reused.
  - Inference: directed opposite-role controls should finish with semantic acceptance before expensive qualification; this records caught gaps and repetition, not measured review-time savings.

- Execution: SEED-029#fsharp-support
  - Timestamp: 2026-11-04 (time unknown; slice 1 and slice 3 acceptance before 65c552a and a20ab79)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.82 (installed by commit 210ae36)
  - Evidence: `test_attribute_on_the_binding_line_belongs_to_no_function` in `65c552a:test/test_languages/testFSharp.py`; `test_and_continuing_a_type_group_is_a_type` in `a20ab79:test/test_languages/testFSharpMembers.py`; story clauses on `type` items and type members in `b0c42aa:.planning/seeds/SEED-029-ocaml-fsharp-support.md` (`#fsharp-support`).
  - Observed effect: the slice 3 implementer reported `type A … and B` as "not a story example; left alone". The coordinator's probe showed a fake function `B` and a missing member `N`. Together with the same-line `[<Attr>] let` gap in slice 1, that made two return rounds, each ending in a discriminating test, before delivery.
  - Inference: an implementer's "unpromised edge" label needs a coordinator probe against the story's general clauses before it is accepted.

## DD-011 — Integrity checks must separate measured inputs from acceptance records

A post-measurement integrity checker asserted an entirely clean tracked tree,
although the coordinator still needed to write accepted PLAN evidence. It also
included its own previous generated manifest among hashed inputs. Keep measured
product/corpus/harness invariants separate from owned acceptance records and
exclude an output manifest from its own input map.

### Occurrences
- Execution: SEED-015#ternary-arrow-boundaries
  - Timestamp: unknown (2026-10-27T19:07–19:12 UTC observation window)
  - Tool: Codex
  - Open Dough release: unknown; installed guidance retained from established claim6f506711
  - Evidence: qualification9_refactor return; root-performance.md and root-qualification-integrity.py at e3956fa; cache seed015-root-737aeaf7/refactor-pre-integrity.json preserves the previous manifest.
  - Observed effect: independent refactor found the zero-tracked-diff/self-output mismatch before evidence delivery. It permitted only the owned PLAN path, kept product/tests unchanged, excluded its own output and regenerated integrity successfully. Four frozen drivers,120 timing pairs and public observations were unchanged; no tests or timing repeated.
  - Inference: distinguish evolving proof records from immutable measured inputs rather than weakening either check. This records the observed seam, not a measurement failure or new benchmark framework.

## DD-012 — Absolute CLI corpus paths bind historical output hashes

The broad CLI includes source paths in its output. A hash from an earlier
absolute corpus directory cannot directly validate the same immutable inputs
copied to a new directory. Freeze literal new arguments and compare all actual
new outputs; use exact prefix correspondence only to explain historical bytes.

### Occurrences
- Execution: SEED-015#ternary-arrow-boundaries
  - Timestamp: unknown (2026-10-27 UTC / 2026-10-28 Singapore execution)
  - Tool: Codex
  - Open Dough release: unknown; installed guidance retained from established claim6f506711
  - Evidence: final-setup-failure.json and final-performance.md at737aeaf; G8; root-freeze.json/root-performance.md retain the later prospective protocol and actual outputs.
  - Observed effect: the first ec2 freeze exited1 before any timing because a historical CLI hash embedded seed015-optimized-8dac7b80/corpus. Exact old/new prefix substitution explained the discrepancy; setup2 retained all three actual new byte-identical outputs. Failed setup/source hashes remained held; source/corpus/expected metrics were not normalized or weakened.
  - Inference: output fixtures that include paths need explicit argument correspondence before a timing launch. A path-only mismatch proves neither a product regression nor a performance result.

- Execution: SEED-029#ocaml-support
  - Timestamp: 2026-11-04 (time unknown; slice 1 and slice 3 acceptance before e8ecf8a and 59ddc71)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.82 (installed by commit 210ae36)
  - Evidence: `test_let_after_non_ending_tokens_is_local` in `e8ecf8a:test/test_languages/testOCaml.py` (rewritten so `then`/`else`/`->` lets sit at depth 0, not inside parentheses); `test_one_line_module_example` in `59ddc71:test/test_languages/testOCamlItemBoundaries.py`.
  - Observed effect: a returned test that passed only through the bracket rule, and the story's literal one-line module example, were caught by coordinator probes and pinned; the product needed no change.
  - Inference: the probes caught tests that did not discriminate, not product gaps; a test whose setup satisfies a second rule cannot observe the first.

## ODF-264 — Unbounded discovery and CI payloads waste coordinator context

Former local code: DD-013.

Narrow source searches and project public CI responses to needed fields before
emitting them. Check-run objects repeat app permissions and endpoints for each
job; retaining raw data need not print it into coordinator context.

### Occurrences
Distinct executions, original provenance, observations and qualified inferences are
[retained at ODF-264](https://github.com/terryyin/open-dough/blob/a29f625255a3368eb721249c01df96e0f98edfd4/docs/maintainer/findings/ODF-264.md); source recovery remains in this project’s pre-compaction revision.

## DD-014 — Empty `gh pr checks` is not a green merge gate

After pushing a corrected contributor head, do not treat `gh pr checks --watch`
reporting "no checks reported" as passage. Wait until the new head's required
workflow run is visible and successful before the exact-head merge; postpone
thanks until that run succeeds if a merge already happened.

### Occurrences
- Execution: SEED-010#review-lexical-js-prs
  - Timestamp: 2026-10-28T05:56:52Z
  - Tool: Cursor
  - Open Dough release: 0.3.71
  - Evidence: conversation 9e3d53de-1aaa-43d4-a4c0-161f4d937215 compaction segment_000 turns ~224–230; `gh pr checks 546 --watch` printed `no checks reported on the 'pr/js-keyword-as-a-member-name' branch` then `gh pr merge 546 --match-head-commit 395ae04…` at mergedAt 2026-10-28T05:56:52Z; head check-runs were still queued; coordinator then watched run 40640907400 to success before thanks. Distinct from DD-004 (missing performance corpus before merge).
  - Observed effect: PR #546 merged while the corrected-head fourteen-job matrix was not yet visible; merge commit `559b8c8` preceded green checks; thanks waited for run success.
  - Inference: empty check listing is an absent gate, not a pass; race after push can hide the new run from `gh pr checks`.

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

On a path under a paired performance gate, do not collapse duplicated hot-path
dispatch into a per-token method call without measuring first. Prefer keeping
the already-qualified inline shape (or an explicit Structure slice that owns a
new six-gate pass) when the contract cares about that loop.

### Occurrences
- Execution: SEED-019#template-interpolation-conditions
  - Timestamp: 2026-10-28T16:41:29+08:00
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.73
  - Evidence: slice-1 refactor agent 5dbb84bb-1fe7-4bb6-b359-cb3585c88194 extracted `_count_conditions_from_token` and bound `count_from = self._count_conditions_from_token` inside `TypeScriptReader.__call__`; attempt9 `measurement-summary.json` failed incremental-api (paired 95% upper +5.02%); slice-2 agent restored attempt8-style `after_interpolation` inline; attempt10 all six gates passed (max upper +2.449%); plan Learnings warn against reintroducing per-token method dispatch on `__call__` without a new six-gate pass.
  - Observed effect: one eligible ≤15-minute trim and a full remeasure were required before merge; semantics were already correct after slice 1.
  - Inference: duplication collapse on a measured hot path can recreate the cost the gate rejects; the contract's trim budget absorbed it here, but the refactor rule did not account for the gate.

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

## DD-399 — Source-only review restored formatter promises the plan proof narrowed

The story listed seven formatters whose `--compact` output must stay unchanged
(XML, CSV, Checkstyle, HTML, clang, MSVS, extension printers). The executable
plan's proof named only "`--xml` (or `--csv`)", and the first implementation
proof followed the plan. The independent source-only review split the list into
separate ids, and the coordinator added the missing observations before
acceptance.

### Occurrences
- Execution: SEED-030#compact-callback-report
  - Timestamp: 2026-11-04T11:00:00+07:00 (approximate; slice 1 acceptance before d912695)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.82 (installed by commit 210ae36)
  - Evidence: plan proof item 5 at `bd4069d:.planning/slice-plans/019-compact-callback-report/PLAN.md`; review ids S9.3–S9.7 and K3 in `d912695:.planning/slice-plans/019-compact-callback-report/source-review.md`; `test_compact_leaves_other_formatters_unchanged` and `test_compact_leaves_extension_printer_unchanged` in `test/testCompactReport.py`.
  - Observed effect: one extra test round before delivery. Five formatter commitments and the analysis-results example became observed proof instead of being accepted on the plan's narrower mapping.
  - Inference: slice planning can collapse a listed compound scope item into "e.g." proof. The separate-ids source review is the gate that catches this. Practice confirmation, not a defect in this execution's delivery.

- Execution: SEED-029#ocaml-support
  - Timestamp: 2026-11-04 (time unknown; slice 1 acceptance before e8ecf8a)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.82 (installed by commit 210ae36)
  - Evidence: slice 1 proof row in `ba8d367:.planning/slice-plans/020-ocaml-support/PLAN.md` named only key examples 1, 2, 6, 8; obligation G1 and review ids 2.3, 2.11.3–2.11.7, 2.12, 4.5, 5.1.2, 5.1.3, 5.7.3, 6.1.2, 7.1 in `e8ecf8a:.planning/slice-plans/020-ocaml-support/source-review.md`.
  - Observed effect: one return round on slice 1 (seven tests added, no product change) before delivery.
  - Inference: the source review ran at first acceptance, after the slice-1 delegation, so the implementer received only the plan's example-shaped proof; obtaining the review before first delegation would have put the ids in the brief.

- Execution: SEED-029#fsharp-support
  - Timestamp: 2026-11-04 (time unknown; during slice 1 implementation before 65c552a)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.82 (installed by commit 210ae36)
  - Evidence: `65c552a:.planning/slice-plans/021-fsharp-support/source-review.md`, with the slice 1 rows for 2.3–2.5, 3.2, 4.x, 5.x, 6.x, 8.x and 9.x and E6.2 moved to slice 2.
  - Observed effect: the coordinator again launched the source review after the slice 1 delegation. The added ids reached the implementer as a mid-flight message, not a return round, so no extra test round was needed. One allocation (E6.2, `sum` CCN 2) contradicted the plan's interim and came back as an implementer gap.
  - Inference: this is the third occurrence in a row. Running the source review and its allocation before first delegation remains the cheaper ordering; checking allocations against named interims would have prevented the E6.2 mismatch.

## Retention

- Highest allocated local number: 399
- Shared-feedback compaction recovery (2026-11-03): `4195708bad2061ebc1c829ebdff7868605b173bb:DearDough.md`; its rows are retained at the linked Open Dough finding owners.
