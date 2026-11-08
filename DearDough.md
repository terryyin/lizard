# DearDough Process Findings

Shared evidence lives in the linked Open Dough records. Project-owned findings and qualified practice observations keep their original identities here.

DD-003 retired after `be8d3c273fd475a0cedc4d1e9da58ff596c6db0c` adopted a dependency stamp and cold, warm, overlapping, changed-input and failure runs confirmed the behavior. The eight original executions and assessments are recoverable at `eebecc3e196a26f6f6b6a41df66ef97025a3361a:DearDough.md`.

Local compaction recovery: `6cd0c9531a3428fc922d5a58b9b3e3299a01b16a:DearDough.md`. Shared compaction recovery: `d1ffc74062ccf1e94dff1f45e96a4d1de07e1520:DearDough.md`; earlier full source at `1fab8f40c7577f53b7736c79ea3f91b67a1fd0ca:DearDough.md`. DD-010's OCaml occurrence was restored from DD-012 by `7129516bc5bd9ba57f370a6353ffe0e10eb1c861`.

<a id="dd-001--codex-stream-can-yield-before-exposing-observer-identity"></a>

## ODF-376 — Codex stream can yield before exposing observer identity

Former local code: DD-001.

A delayed Codex stream can yield before publishing the CI observer identity. The later parsed identity was invisible to the coordinator. [ODF-376 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-376.md#odf-376)

<a id="dd-002--hosted-proof-slices-have-no-guarded-increment-that-stays-unfinished"></a>

## ODF-275 — Hosted-proof slices have no guarded increment that stays unfinished

Former local code: DD-002.

Guarded publication used to require marking the slice done before the hosted proof could arrive. The coordinator used a manual commit and a later records-only transition. See DD-400 for successful later use of the guarded progress route. [ODF-275 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-275.md#odf-275)

## DD-004 — PR merge preceded a missing affected-workload performance gate

Apply the affected-workload gate before exact-head merge. For `SEED-010#review-fabio-malpezzi-prs` (Codex, execution time/release unknown, guidance baseline `2e21e404`), PR527 merged at `e871ae448` before the nested-function, async, match, continuation and class controls in `af27c910:.planning/slice-plans/004-fabio-malpezzi-pr-reviews/527-review.md` and plan obligation G1 were settled. The later 22 controls passed (upper 95% slowdown bound −0.6803%, `docs/reviews/fabio-malpezzi-20261027/controls/`). The project's workload-selection gap preceded merge and remains a distinct, qualified finding.

## ODF-397 — Native Git bypassed the installed backlog merge adapter

Former local code: DD-005.

Native Git bypassed the installed backlog merge adapter, including for apparently clean integrations. [ODF-397 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-397.md#odf-397)

## DD-006 — `python -c` import probes contaminate via argv0

Use a real script for checkout-isolated import probes. In `SEED-013#arrow-object-boundaries` (Cursor auto, 0.3.71, event time unknown), `lizard.py` resolved `abspath(sys.argv[0])` against cwd under `python -c`, causing false import-path failures. The harness's real `paired.py` workers were sound. See agent `89783fe2-57ff-41aa-b641-0cef9d98622b` and `docs/reviews/fabio-malpezzi-20261027/pr531/corrected-slice3-prelaunch.txt` (2026-10-27T11:47:50Z).

## DD-007 — Expression-body flag gated on “newly created” missed paren-param arrows

Derive arrow expression-body context from the body token on every `_arrow_function` path. In `SEED-013#arrow-object-boundaries` (Cursor auto, 0.3.71, event time unknown), `if created:` skipped arrows already pushed for `(...)` parameters. Refactor agent `c1df6a2a-bff5-455b-a7a1-638d227d1f52` fixed it before delivery (plan 009 slice 2, `testArrowObjectBoundaries.py` paren-param ternaries, proof 6/363 and reproducer 64).

## DD-008 — Fork `push:false` alone is not a contributor-route refusal

Combine `maintainer_can_modify` and a divergent-SHA dry-run push before rejecting a fork route. For `SEED-012#regex-literal-context` (2026-10-27T18:40:00+08:00, Cursor auto, 0.3.71 via `01afe9c`), PR528's fork said `push:false`, MCM was true and only the divergent dry-run proved access. No false delivery stop followed. See agent `7093d051-eac5-4592-b08e-cdb07e4ea249`, plan 008 slice 1.

## DD-009 — Plan-authorized contributor writes refused by host permission classifier

Plan-authorized PR533 fork publication and merge were blocked by the host classifier as “Create Public Surface” and “Merge Without Review.” `SEED-014#multiline-arrow-boundaries` (2026-10-27, time unknown, Claude Code Opus 5.5, 0.3.71 via `01afe9c`) required two conversation authorizations and stopped its CI observer. Plan 010 slice 4 at `3cdc283`, receipt `48eaccfb-ca45-4cd5-a7aa-dfecbbf15443`, push `1542385..b6074d8`, merge `a2d2ab7`. No correctness impact. Keep as a qualified host-policy observation.

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

Project discovery and check-run payloads repeated unnecessary detail and forced narrower retrieval. The existing ignore rule already covers the website reports facet. [ODF-264 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-264.md#odf-264)

## DD-014 — Empty `gh pr checks` is not a green merge gate

An empty `gh pr checks --watch` list after a push leaves the exact-head gate open. In `SEED-010#review-lexical-js-prs` (2026-10-28T05:56:52Z, Cursor, 0.3.71), PR546 merged at `559b8c8` for corrected head `395ae04` before the 14-job matrix appeared. Thanks waited for green run 40640907400 (conversation `9e3d53de-1aaa-43d4-a4c0-161f4d937215`, segment_000, turns ~224–230).

Project guidance `lizard-process-pr` §5 already required checks at `f0887ec`, so no repair was adopted. Later PR525 (`b97f795`) and PR548 (`2416c37`) waited for all 32 exact-head checks. Keep the application lapse open without new work.

## DD-015 — Interrupted Task recovery by inspecting uncommitted checkout

Interrupted Tasks left recoverable checkout evidence, which the coordinator inspected before resuming only unfinished work.

- `SEED-010#review-lexical-js-prs` (2026-10-28, time unknown, Cursor, 0.3.71): two interruptions left uncommitted product/evidence and no commits. Slice 2 ended in an evidenced non-merge (conversation `9e3d53de-1aaa-43d4-a4c0-161f4d937215`, segment_000).
- `SEED-019#template-interpolation-conditions` (2026-10-29T06:47:00+08:00, Cursor auto, 0.3.73): resumed the two outstanding attempt10 gates without retiming the other four, then merged PR545 (conversation `b6ecc26c-f7f1-4c5f-a3aa-2a2ccfa36047` L60).
- `SEED-021#dart-support` (2026-10-29T11:20:00+08:00, Cursor auto, 0.3.74): reviewed interrupted slice-6 refactor changes and tests, reran only the refactor, and delivered `7a5b0d7` (conversation `5ae6ce13-8910-4941-95bc-b0a708f808f0`).

## DD-016 — Post-change hot-path collapse reintroduced gated per-token dispatch

Measure before consolidating dispatch on a performance-gated path. In `SEED-019#template-interpolation-conditions` (2026-10-28T16:41:29+08:00, Cursor auto, 0.3.73), refactor `5dbb84bb-1fe7-4bb6-b359-cb3585c88194` extracted per-token dispatch in `TypeScriptReader.__call__`. Attempt9 failed incremental API (95% upper +5.02%) despite correct semantics. Restoring inline `after_interpolation` cost one eligible trim and remeasurement; attempt10 passed six gates (max upper +2.449%). Plan Learnings retain the guard.

## ODF-378 — First-landing expectation blocks story-branch increment publish

Former local code: DD-017.

A first-trunk landing expectation incorrectly bound an intermediate story-branch delivery, causing a target mismatch before push. [ODF-378 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-378.md#odf-378)

## ODF-412 — Contributor publication carries spent qualification evidence

Former local code: DD-398.

Publishing the entire execution branch put spent qualification evidence in the contributor review. PR548 selective staging was an execution-specific mitigation. [ODF-412 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-412.md#odf-412)

## ODF-367 — Source-only review restored formatter promises the plan proof narrowed

Former local code: DD-399.

The plan named XML or CSV proof for a seven-formatter promise. Source review recovered XML, CSV, Checkstyle, HTML, clang, MSVS and extension-printer commitments before acceptance. [ODF-367 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-367.md#odf-367)

## DD-400 — Hosted CI-proof slices published with `progress: true` until observation

Guarded `progress: true` kept two Lizard hosted-proof slices unfinished until CI was observed. `SEED-035#publish-lizard-ci-proof` (2026-11-05T18:06:32+07:00, Cursor auto, 0.3.86) published `c7643ce7e` and `15dc13b8d`, observed runs 42016250359, 42017933530 and 42018498291, then recorded done observations. The artifact receipts were accepted without a manual agent-commit fallback. [ODF-275 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-275.md#odf-275)

## ODF-441 — Fresh-increment delivery retries stumble on Darwin mktemp suffixes and git-rm staging

Former local code: DD-401.

Darwin mktemp requires trailing placeholders. Fresh-increment must put deletions already staged by git rm in stagedPaths. [ODF-441 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-441.md#odf-441)

## ODF-413 — Hosted-CI commitment marked observed before receipt or merge head

Former local code: DD-402.

A merge-gated hosted commitment was marked observed before exact-head checks on the promised merged revision finished. [ODF-413 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-413.md#odf-413)

## DD-403 — Early cost-owner performance probe avoided unnecessary NS remediation

Probe cost ownership under quiet load before changing a product owner. For `SEED-035#ns-member-name-performance` at `43a07aafa2e1d7e28955ba554ced3263baca1874` (2026-11-06T21:25:18+07:00, Cursor auto, 0.3.86), attempt5 cumulative API parameters failed at ~+4.51% upper. On the refreshed target the prior set (~+0.98%) and full candidate (~+1.41%) passed, so the as-authored `_member_name` landed with six gates under 3% and no NS remediation. See `docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice1/` and `seed035-slice2/`.

## Retention

- Highest allocated local number: 403
- Shared-feedback adoption and compaction recovery (2026-11-07): `7129516bc5bd9ba57f370a6353ffe0e10eb1c861:DearDough.md`; its rows are retained at the linked Open Dough finding owners (`483d3a1e8f0ea24b929675b9a195faca48ac8f47:docs/maintainer/findings/`).
- Shared-feedback compaction recovery (2026-11-03): `4195708bad2061ebc1c829ebdff7868605b173bb:DearDough.md`; its rows are retained at the linked Open Dough finding owners.
