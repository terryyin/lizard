# DearDough Process Findings

## DD-001 — Codex stream can yield before exposing observer identity

The prescribed Codex binding prints the observer directory only in its initial
tool result. If checkout setup or provider discovery outlasts that yield, a later
`CI_OBSERVER` line is parsed without exposing the identity to the coordinator.
Consider emitting identity when it becomes available, within the existing binding.

### Occurrences
- Execution: SEED-006#ci-verified-python-support
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; installed guidance came from commit 01afe9c
  - Evidence: launch d05e8e74-4462-4ca8-b3d9-99a957269f33, 2026-10-27; yielded cells 8 and 4; `.agents/skills/dough-execute-plan/references/ci-notify-codex.md` prints identity before its stream loop.
  - Observed effect: both launches yielded a watching session without a directory, with initial waits of 1 and 10 seconds. Two owner-based recoveries using `classifyOwnedObservation` and `codexStreamOwner` were needed before delivery.
  - Inference: the binding's initial-result assumption is sensitive to setup/discovery latency; coverage itself remained available.

- Execution: SEED-012#release-website-from-tags
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; executed installed guidance was unchanged from established start 96e5aa4
  - Evidence: dashboard launch 0a5582c3-35b9-49ce-a092-e6dae8e1f3bc, 2026-10-27; yielded cells 7 and 27 lacked directory/PID in their first result; owner-based recovery selected /tmp/dough-ci-501/watch-2aL4vZ. Replacement cell 108 used plain pipes and exposed directory/PID immediately.
  - Observed effect: coordinator recovered observer ownership manually before delivery; the later request-error notification required explicit acknowledgment because the binding lacked its parsed directory. Both delivered increments retained green receipts.
  - Inference: plain pipes avoided this occurrence; whether setup latency or PTY formatting prevented initial receipt capture was not established.

## DD-002 — Hosted-proof slices have no guarded increment that stays unfinished

A slice whose own proof needs a hosted CI receipt of its publication must be
published before it can be accepted. Guarded fresh delivery always marks the
selected slice `done` and correctly refuses while that slice's receiving
obligation is open, so the coordinator fell back to a manual agent commit plus
existing-candidate `deliver`, then a later plan-only fresh increment to mark it
done. Consider a documented fresh-increment path that publishes without the
status transition.

### Occurrences
- Execution: SEED-012#modernize-website-react
  - Timestamp: unknown
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: `execution-increment-delivery.mjs fresh` for slice 3 returned `preparation-failed`, operation `story obligations`, G3 `open-obligation`; refusal shortly before recovery commit `f6af787` (committed 2026-10-27T16:17:22+08:00; agent-commit + `deliver --validated-candidate`), `390e69d`, `ce549a1`.
  - Observed effect: one refused attempt, then three increments instead of two; the obligation guard itself prevented marking slice 3 done before its hosted run and skip receipts existed.
  - Inference: the workaround is sound but not described in the fresh-increment or delivery references, so another coordinator could instead mark done early or stall.

- Execution: SEED-012#release-website-from-tags
  - Timestamp: unknown
  - Tool: Codex
  - Open Dough release: unknown; executed installed guidance was unchanged from established start 96e5aa4
  - Evidence: source plan slices 1–3; diagnostic sources b34c4666, 94fc79a4, 5d320dd4, c034140a, f3d9ec34 prepared with an isolated Git index; regular accepted increments 0fb43372, 19f934a4, f0e6f2b4. Public tag web-0.0.4 points to the tested f3d9ec34 candidate.
  - Observed effect: hosted auth/deploy/public proof needed published source while its slice was still unfinished. The coordinator used commit-tree with temporary refs or immutable tags, then guarded fresh delivery after accepted proof; execution HEAD/index and slice status stayed intact during probing.
  - Inference: a documented guarded provisional-candidate path could preserve credit/hook conventions without either marking a slice done before proof or inventing Git plumbing.

- Execution: SEED-014#multiline-arrow-boundaries
  - Timestamp: unknown (2026-10-27 evening, between slice 3 delivery c5cc309 and slice 4 merge a2d2ab7)
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.71 (installed by commit 01afe9c)
  - Evidence: slice 4 progress commit `2764d8d` made by agent-commit plus `deliver --validated-candidate`, later plan-only fresh increment `25bff7f` marking slice 4 done; earlier `fresh` refusal "slice 1 is done … but not in its committed plan" forced a separate plan-only increment `4b8de7a` for the no-product-change slice 1.
  - Observed effect: an externally blocked hosted-delivery slice and an evidence-only slice each needed an extra increment shape not described by the fresh-increment reference.
  - Inference: same gap as above; also applies to slices whose proof is only an external observation.

## DD-003 — Nix shellHook reinstalls Python dependencies on every command

Every `nix develop -c` runs `pip install -r dev_requirements.txt` in the
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

## Retention

- Highest allocated local number: 10
