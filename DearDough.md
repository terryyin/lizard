# DearDough Process Findings

Source maintenance on 2026-10-28 retained collected evidence before shortening at Open Dough `32b5ae0b4414d755bcde4ef6cd822774d7450771:docs/maintainer/findings/`. The complete post-local source is `1fab8f40c7577f53b7736c79ea3f91b67a1fd0ca:DearDough.md`; linked source records share that revision. Unknown execution releases and local facets remain qualified.

The existing DearDough log currently holds both shared and project findings.
Project-owned entries DD-003, DD-006, DD-007, DD-008, DD-011 and DD-012 stay here
with their original evidence; DD-009 is a host-policy observation, DD-010 is a
successful practice, and DD-013 is qualified command-output practice. DD-004
concerns application of this project's performance acceptance gate; it remains
qualified and is not evidence of a shared performance mechanism defect.

The local maintenance selected [repeatable shell entry](https://github.com/terryyin/lizard/blob/2d845a1eaaccee05592a486234ad49fb7d1723de/.planning/seeds/SEED-018-repeatable-shell-entry.md#repeatable-shell-entry)
for DD-003. The adopted response is no longer only queued: `shell.nix`
prepares when `.venv` is missing or the `dev_requirements.txt` stamp
mismatches, otherwise reuses without pip install chatter, refreshes on
dependency-input change, and surfaces preparation failures on the failing
entry. Six distinct executions reported the prior repeated setup/noise,
including two overlapping-entry races. The import-probe trap has an
established usable form, the parser issue was corrected before acceptance,
and the remaining one-off proof seams were caught. They warrant no second
implementation story. Existing Taken work and direction retain their order.

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

<a id="dd-003"></a>

## DD-003 — Nix shellHook reinstalls Python dependencies on every command

**Adopted response:** [Enter the development shell repeatedly without rebuilding its environment](https://github.com/terryyin/lizard/blob/2d845a1eaaccee05592a486234ad49fb7d1723de/.planning/seeds/SEED-018-repeatable-shell-entry.md#repeatable-shell-entry) — SEED-018#repeatable-shell-entry.
**Adopted revision:** `be8d3c273fd475a0cedc4d1e9da58ff596c6db0c` (stamp-guarded `shell.nix` shellHook + relevant-use recording for SEED-018#repeatable-shell-entry).
**Relevant-use outcome:** Isolated checkout proof on 2026-10-28 (`/tmp/lizard-shell-slice2-*.out`): cold prep installs once (stdout retained the historical `virtualenv 20.13.2 requires platformdirs<3,>=2, but you have platformdirs 4.4.0` line when pip ran); warm and overlapping warm entries exit 0 with command output only and no pip install chatter; changing `dev_requirements.txt` refreshes (pip runs, stamp updates) then subsequent warm reuses cleanly; a failing `pip install -r` surfaces `ERROR: No matching distribution…` with non-zero exit and does not run the requested command or write the new stamp. No latency claim is asserted. Finding is adopted with relevant-use evidence; full retirement under a longer observation criterion remains a triage judgment, not blocked by queueing alone.

Every `nix develop -c` **formerly** ran `pip install -r dev_requirements.txt` in the
shellHook, printing pip output to stdout and upgrading `platformdirs` past
`virtualenv`'s pin each time. Agents had to filter that output out of
`gcloud meta list-files-for-upload` and other parsed command results.

### Occurrences
- Execution: SEED-012#modernize-website-react
  - Timestamp: unknown
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: `shell.nix` shellHook; slice 2 implementation report (upload list contained non-path pip lines); slice 2 refactor report (platformdirs 4.4.0 conflicts with virtualenv 20.13.2).
  - Observed effect: extra latency and stdout noise on each Nix-prefixed command; no proof result changed.
  - Inference: a project-environment issue rather than shared guidance; a stamp-guarded or quiet install would remove it.

- Execution: SEED-012#release-website-from-tags
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; executed installed guidance was unchanged from established start 96e5aa4
  - Evidence: shell.nix and repeated Nix-prefixed setup/read/test/cloud commands in this execution; the platformdirs 4.4.0 versus virtualenv 20.13.2 warning persisted, including final pytest/make checks (1,854 tests).
  - Observed effect: repeated pip installation/noise accompanied read-only commands as well as tests. PIP_QUIET reduced ordinary output; coordinators sequenced shell initialization because each invocation recreates the local venv. Proof verdicts remained valid.
  - Inference: this recurs in the existing project-environment issue; no shared guidance or environment fix was made by this story.

### Additional occurrence
- Execution: SEED-010#review-fabio-malpezzi-prs
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; installed guidance at execution baseline 2e21e404
  - Evidence: PR535 independent refactor return and sequential environment recovery session 90108; execution-branch revision f25aeca2, docs/reviews/fabio-malpezzi-20261027/pr535/.
  - Observed effect: two overlapping read-only Nix commands re-entered the mutating shellHook and produced an ensurepip diagnostic. A subsequent sequential Python/pytest import check passed. Accepted proof predated the race or ran in the separate validation checkout.
  - Inference: read-only command intent does not make shellHook entry safe to parallelize. Serialize Nix entries across agents in the same checkout.

### Additional occurrence
- Execution: SEED-012#regex-literal-context
  - Timestamp: 2026-10-27T20:25:00+08:00
  - Tool: Cursor
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: slice-3 post-change refactor agent d49cf4a4-9eaa-4044-96fb-f7e6a0bc649f; coordinator 946f4d3a-43e2-470f-8722-fc7bcd78fef5; `shell.nix` shellHook; filter stripping `platformdirs`/`Requirement already`/`Successfully` from `nix develop -c` stdout before a line-count check.
  - Observed effect: every Nix-prefixed command re-entered pip install; agents filtered shellHook/platformdirs noise from parsed results. Semantic and performance proof still passed.
  - Inference: same project-environment shellHook cost as prior DD-003 rows; serial Nix use avoided the parallel race, but stdout noise and platformdirs pin churn remained on each entry.

- Execution: SEED-016#arrow-parameter-bindings
  - Timestamp: unknown (2026-10-27 execution)
  - Tool: Codex
  - Open Dough release: unknown; installed guidance at established start 7f479101
  - Evidence: slice-3 independent refactor return launched three Nix-prefixed reads concurrently and reported ensurepip failure; coordinator recovery session ran the affected 21 tests serially; [slice4 proof](https://github.com/terryyin/lizard/blob/4bfd49bc7eb7cd136f6adb3bfad3ab93f9a1631b/docs/reviews/fabio-malpezzi-20261027/pr536/slice4-proof.md) records serial final qualification.
  - Observed effect: overlapping shellHook entry recreated the shared environment during read-only work. Sequential recovery passed; accepted final qualification used one serial outer runner.
  - Inference: reinforces the existing project issue: serialize Nix entry across agents, including read-only probes and observer acknowledgements.

- Execution: SEED-015#ternary-arrow-boundaries
  - Timestamp: unknown (2026-10-27 UTC / 2026-10-28 Singapore execution)
  - Tool: Codex
  - Open Dough release: unknown; installed guidance retained from established claim6f506711
  - Evidence: root-performance.md and delivery-local-proof.md at PR534 qualified head0bfc798; sequential Nix setup/read/test/publication tool results repeatedly print the platformdirs4.4.0/virtualenv20.13.2 warning despite PIP_QUIET=1.
  - Observed effect: shellHook initialization and dependency noise accompany read-only commands and proof. Every owned Nix entry was serialized; no SEED015 parallel-Nix race is claimed. Interpreter/import hashes and accepted proof held.
  - Inference: recurrence of the project environment issue; no total latency or causal timing allocation is asserted.

- Execution: SEED-010#review-lexical-js-prs
  - Timestamp: 2026-10-28T05:56:00Z
  - Tool: Cursor
  - Open Dough release: 0.3.71
  - Evidence: conversation 9e3d53de-1aaa-43d4-a4c0-161f4d937215 compaction segment_000 around PR #546 publish; temporary checkout `/tmp/pr546-publish-10000` Nix probe printed platformdirs 4.4.0 vs virtualenv 20.13.2 despite `PIP_QUIET=1`.
  - Observed effect: shellHook pip noise accompanied the contributor-head publish probe; semantic assert still passed and the push completed.
  - Inference: same project-environment shellHook cost; serial Nix use avoided races, but pin churn remained on entry.

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

## DD-005 — Native Git bypassed the installed backlog merge adapter

Resolve the actual checkout's installed adapter before every merge touching the
backlog or done records, including clean fast-forwards and contributor branches.

### Occurrences
- Execution: SEED-010#review-fabio-malpezzi-prs
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; installed guidance at execution baseline 2e21e404
  - Evidence: PR535 dependency merge into contributor head 1a44c0cad59ea3f3dcabe3c1ac601d905437a7c7; execution-branch revision f25aeca2, .planning/slice-plans/004-fabio-malpezzi-pr-reviews/535-review.md and CONTEXT.md. Later validation-checkout advance to 53e4850 used its installed product-backlog-git-merge.mjs adapter.
  - Observed effect: the coordinator used native Git for the dependency merge and an earlier private validation fast-forward before rereading the adapter rule. Complete candidate/landed trees matched the validated result, and the subsequent done-catalog audit found 18 records already current; no incorrect merged backlog was observed.
  - Inference: clean Git output does not validate domain reconciliation. The resulting product proof remains supported, while the process bypass should not recur.

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

## DD-013 — Unbounded discovery and CI payloads waste coordinator context

Narrow source searches and project public CI responses to needed fields before
emitting them. Check-run objects repeat app permissions and endpoints for each
job; retaining raw data need not print it into coordinator context.

### Occurrences
- Execution: SEED-015#ternary-arrow-boundaries
  - Timestamp: unknown (after Q0bfc publication and before PR534 merge48de7f2)
  - Tool: Codex
  - Open Dough release: unknown; installed guidance retained from established claim6f506711
  - Evidence: continuation_coordinator functions.exec q_gate_initial_1 output reports original token count23070 and truncation; later q_ci_final_groups/final merge guard project job ID/name/status/conclusion/head/URL and validate all32 checks.
  - Observed effect: one initial result printed repeated nested GitHub app metadata and was truncated. The full value was retained in a tool store; subsequent complete per_page100 collection and compact projection preserved both14-job matrices and final gate facts without that metadata. No CI verdict was inferred from truncation.
  - Additional evidence: root retained a20,788-token GitHub ALL_TOOLS discovery matching shared description boilerplate, and a125,371-token broad installation rg output; exact-name projection and exact API/receiver files replaced those searches before conclusions. No unrelated launch-record contents or truncated output established acceptance.
  - Inference: avoidable output/context cost, not a provider or CI failure. No aggregate token saving or elapsed-time reduction is claimed.

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

## Retention

- Highest allocated local number: 15
