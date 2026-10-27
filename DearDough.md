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

## Retention

- Highest allocated local number: 8
