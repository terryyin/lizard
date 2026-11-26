# DearDough Process Findings

Shared evidence lives in the linked Open Dough records. Project-owned findings and qualified practices keep their original identities here.

<a id="dd-404--absolute-sha-git-show-discriminators-fail-on-shallow-matrix-checkouts"></a>

- DD-404 retired after `dde1a337` replaced historical Git lookup with an in-repo fixture. [CI 42745089766](https://github.com/terryyin/lizard/actions/runs/42745089766) passed the shallow Ubuntu and Windows matrix on `a3ec770a`. Recover the SEED-052 occurrence at `7b7bb72d0d4edac878272201f9591bbb18e83a4a:DearDough.md#dd-404--absolute-sha-git-show-discriminators-fail-on-shallow-matrix-checkouts`.

Earlier local and shared compaction: `6cd0c9531a3428fc922d5a58b9b3e3299a01b16a`, `d1ffc74062ccf1e94dff1f45e96a4d1de07e1520` and `1fab8f40c7577f53b7736c79ea3f91b67a1fd0ca` (all `DearDough.md`). `7129516bc5bd9ba57f370a6353ffe0e10eb1c861` restored DD-010’s OCaml occurrence from DD-012.

<a id="dd-001--codex-stream-can-yield-before-exposing-observer-identity"></a>

## ODF-376 — Codex stream can yield before exposing observer identity

Former local code: DD-001. A delayed Codex stream can yield before publishing the CI observer identity. The later parsed identity was invisible to the coordinator. [ODF-376 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-376.md#odf-376)

<a id="dd-002--hosted-proof-slices-have-no-guarded-increment-that-stays-unfinished"></a>

## ODF-275 — Hosted-proof slices have no guarded increment that stays unfinished

Former local code: DD-002. Guarded publication used to require marking the slice done before the hosted proof could arrive. The coordinator used a manual commit and a later records-only transition. See DD-400 for successful later use of the guarded progress route. [ODF-275 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-275.md#odf-275)

## DD-004 — PR merge preceded a missing affected-workload performance gate

Apply the affected-workload gate before exact-head merge. For `SEED-010#review-fabio-malpezzi-prs` (Codex, execution time/release unknown, guidance baseline `2e21e404`), PR527 merged at `e871ae448` before the nested-function, async, match, continuation and class controls in `af27c910:.planning/slice-plans/004-fabio-malpezzi-pr-reviews/527-review.md` and plan obligation G1 were settled. The later 22 controls passed (upper 95% slowdown bound −0.6803%, `ad1f9aa1:docs/reviews/fabio-malpezzi-20261027/controls/`). The project's workload-selection gap preceded merge and remains a distinct, qualified finding.

## ODF-397 — Native Git bypassed the installed backlog merge adapter

Former local code: DD-005. Native Git bypassed the installed backlog merge adapter, including for apparently clean integrations. [ODF-397 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-397.md#odf-397)

## DD-006 — `python -c` import probes contaminate via argv0

Use a real script for checkout-isolated import probes. In `SEED-013#arrow-object-boundaries` (Cursor auto, 0.3.71, event time unknown), `lizard.py` resolved `abspath(sys.argv[0])` against cwd under `python -c`, causing false import-path failures. The harness's real `paired.py` workers were sound. See agent `89783fe2-57ff-41aa-b641-0cef9d98622b` and `ad1f9aa1:docs/reviews/fabio-malpezzi-20261027/pr531/corrected-slice3-prelaunch.txt` (2026-10-27T11:47:50Z).

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

## DD-012 — Absolute CLI corpus paths bind historical output hashes

Freeze literal CLI arguments and compare actual outputs before timing a relocated corpus. In `SEED-015#ternary-arrow-boundaries` (Codex, release unknown, guidance at `6f506711`, 2026-10-27 UTC/28 Singapore), the first EC2 freeze exited 1 before timing because the older hash embedded `seed015-optimized-8dac7b80/corpus`. Exact prefix correspondence explained the old bytes. Setup2 compared three actual identical outputs and retained failed setup/source hashes without weakening metrics. See `737aeaf` final-setup-failure/final-performance, G8, and later root-freeze/root-performance.

## ODF-264 — Unbounded discovery and CI payloads waste coordinator context

Former local code: DD-013. Project discovery and check-run payloads repeated unnecessary detail and forced narrower retrieval. The existing ignore rule already covers the website reports facet. [ODF-264 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-264.md#odf-264)

PR 569 also required narrower guidance and job reads. [Retained execution](https://github.com/terryyin/open-dough/blob/286c5e0dec58e72e9328b7dec4a1ab52e88710b1/docs/maintainer/findings/ODF-264.md#lizard--odf-264).

## DD-014 — Empty `gh pr checks` is not a green merge gate


In `SEED-010#review-lexical-js-prs` (2026-10-28T05:56:52Z, Cursor, 0.3.71), `gh pr checks --watch` returned no checks for `395ae04`. PR546 merged at `559b8c8` before the 14-job matrix appeared. Thanks followed green run 40640907400 (conversation `9e3d53de-1aaa-43d4-a4c0-161f4d937215`, segment_000, turns ~224–230). Lizard's own guidance already required the gate (`f0887ec`, §5). Later PR525/548 waited for all 32 exact-head checks on `b97f795` and `2416c37`. Keep the application lapse open.

## DD-015 — Interrupted Task recovery by inspecting uncommitted checkout

Interrupted Tasks left recoverable checkout evidence, which the coordinator inspected before resuming only unfinished work.

- `SEED-010#review-lexical-js-prs` (2026-10-28, time unknown, Cursor, 0.3.71): two interruptions left uncommitted product/evidence and no commits. Slice 2 ended in an evidenced non-merge (conversation `9e3d53de-1aaa-43d4-a4c0-161f4d937215`, segment_000).
- `SEED-019#template-interpolation-conditions` (2026-10-29T06:47:00+08:00, Cursor auto, 0.3.73): resumed the two outstanding attempt10 gates without retiming the other four, then merged PR545 (conversation `b6ecc26c-f7f1-4c5f-a3aa-2a2ccfa36047` L60).
- `SEED-021#dart-support` (2026-10-29T11:20:00+08:00, Cursor auto, 0.3.74): reviewed interrupted slice-6 refactor changes and tests, reran only the refactor, and delivered `7a5b0d7` (conversation `5ae6ce13-8910-4941-95bc-b0a708f808f0`).

## ODF-378 — First-landing expectation blocks story-branch increment publish

Former local code: DD-017. A first-trunk landing expectation incorrectly bound an intermediate story-branch delivery, causing a target mismatch before push. [ODF-378 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-378.md#odf-378)

## ODF-412 — Contributor publication carries spent qualification evidence

Former local code: DD-398. Publishing the entire execution branch put spent qualification evidence in the contributor review. PR548 selective staging was an execution-specific mitigation. [ODF-412 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-412.md#odf-412)

## ODF-367 — Source-only review restored formatter promises the plan proof narrowed

Former local code: DD-399. The plan named XML or CSV proof for a seven-formatter promise. Source review recovered XML, CSV, Checkstyle, HTML, clang, MSVS and extension-printer commitments before acceptance. [ODF-367 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-367.md#odf-367)

## DD-400 — Hosted CI-proof slices published with `progress: true` until observation

Guarded progress kept two Cursor executions unfinished until hosted proof was accepted, with ordinary done delivery and no manual agent-commit fallback. `SEED-035#publish-lizard-ci-proof` (0.3.86) retained three hosted receipts. `SEED-052#publish-explicit-ci-test-counts` (0.3.94) retained run `42745089766` and the Intent result after repairing the Windows and shallow-checkout failures. [ODF-275 evidence](https://github.com/terryyin/open-dough/blob/96e641815b9b7d957d97e03c612034d1c4093734/docs/maintainer/findings/ODF-275.md#odf-275). Full source rows: `09b475de260bd478059de2473c3898ed62a65eec:DearDough.md#dd-400--hosted-ci-proof-slices-published-with-progress-true-until-observation`.

## ODF-441 — Fresh-increment delivery retries stumble on Darwin mktemp suffixes and git-rm staging

Former local code: DD-401. Darwin mktemp requires trailing placeholders. Fresh-increment must put deletions already staged by git rm in stagedPaths. [ODF-441 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-441.md#odf-441)

## ODF-413 — Hosted-CI commitment marked observed before receipt or merge head

Former local code: DD-402. A merge-gated hosted commitment was marked observed before exact-head checks on the promised merged revision finished. [ODF-413 evidence](https://github.com/terryyin/open-dough/blob/483d3a1e8f0ea24b929675b9a195faca48ac8f47/docs/maintainer/findings/ODF-413.md#odf-413)

## DD-403 — Early cost-owner performance probe avoided unnecessary NS remediation

Probe cost ownership under quiet load before changing a product owner. For `SEED-035#ns-member-name-performance` at `43a07aafa2e1d7e28955ba554ced3263baca1874` (2026-11-06T21:25:18+07:00, Cursor auto, 0.3.86), attempt5 cumulative API parameters failed at ~+4.51% upper. On the refreshed target the prior set (~+0.98%) and full candidate (~+1.41%) passed, so the as-authored `_member_name` landed with six gates under 3% and no NS remediation. See `ad1f9aa1:docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice1/` and `seed035-slice2/`.

<a id="dd-405--windows-ci-proof-tsv-newlines-needed-successive-write-boundary-repairs"></a>

## ODF-488 — Windows CI-proof TSV newlines needed successive write-boundary repairs

Former local code: DD-405. Lizard's Windows pipeline tests passed after `a6fa1479`, `e22e93c4` and `a3ec770a` normalized captured stdout and forced LF at the converter and helper write boundaries. [CI run 42745089766](https://github.com/terryyin/lizard/actions/runs/42745089766) verified the local repair on `a3ec770a`. The shared TSV contract remains open. [ODF-488 evidence](https://github.com/terryyin/open-dough/blob/96e641815b9b7d957d97e03c612034d1c4093734/docs/maintainer/findings/ODF-488.md#odf-488).

## ODF-482 — Source freshness can select a contributor merge base without the claim

<a id="dd-489--source-freshness-can-select-a-contributor-merge-base-without-the-claim"></a>

Former local code: DD-489. After PR 569’s guarded merge, one of two best bases lacked the claim. Integrating the accepted target restored the normal check. [Retained evidence](https://github.com/terryyin/open-dough/blob/286c5e0dec58e72e9328b7dec4a1ab52e88710b1/docs/maintainer/findings/ODF-482.md#lizard--dd-489).

## DD-490 — Planned proof home was already oversized without a reorganization decision

When slice planning names an already-oversized suite as the only proof home and supplies no extract seam or size exception, post-change refactor correctly stops on the file-size gate and delivery waits on a human reorganization choice. Prefer naming the cohesive extract (or an authorized exception) in the plan before execution when the suite is known to exceed 250 lines.

### Occurrences
- Execution: SEED-067#cognitive-comma-ends-ternary
  - Timestamp: 2026-11-20T23:13:00+08:00
  - Tool: Cursor
  - Open Dough release: 0.3.109
  - Evidence: plan 034 outside-in proof named `testCognitiveComplexity.py` (~1377 lines); conversation `708d604e-26b6-4ae7-94ff-9212fda6318d` refactor stop then human chose option 1; delivered extract `test/test_extensions/testCognitivePeerCommaEndsTernary.py` at `579fd508`
  - Observed effect: Slice 1 product/proof were accepted before commit; delivery paused until extract authorization; extract completed and increment published
  - Inference: The Jidoka stop matched the file-size rule; avoidable cost was the mid-delivery human decision that planning could have pre-settled
- Execution: SEED-075#ruby-percent-literal-context
  - Timestamp: 2026-11-26T07:05:00+08:00
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.114
  - Evidence: the plan's proof names `test/test_languages/testRuby.py` (796 lines before the slice, 822 after) and supplies no seam or exception; the refactor pass returned `REFACTOR JIDOKA STOP`; the developer chose defer; the follow-up is queued as `SEED-076#ruby-reader-test-topics`; increment `8efebf80`
  - Observed effect: delivery paused for a developer question after proof was accepted; the slice published with the split deferred
  - Inference: same as before. Planning could have settled the decision for a 26-line addition to a suite already oversized
- Execution: SEED-075#perl-word-operator-context
  - Timestamp: unknown
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.114
  - Evidence: plan 038 Current decisions allowed extending shared `mark_member_names` or a Perl-local wrap and settled only "no reader-file split"; implementation edited `lizard_languages/code_reader.py` (269 lines); conversation `89a21532-f79f-4cc8-900e-cda83a0a689d` refactor subagent `4b02ce7d-0671-4a00-9681-6e7785f4398d` returned `## REFACTOR JIDOKA STOP`; 2026-11-26 07:21-07:58 +08:00
  - Observed effect: Production helper over 250 lines had no size decision; the refactor pass stopped, the coordinator settled keep-without-split from the plan's own helper-extension decision, and the pass resumed
  - Inference: Same gap as above on a shared production file rather than a proof home; naming each alternative's target file size and keep-or-extract decision in the plan would have avoided one stop and resume round trip

## DD-491 — Black assumed as selective formatter, then `git checkout` wiped the slice

Lizard’s delivery path is check-only `pycodestyle` via `make`/hooks, not an autoformatter. Invoking Black as “selective formatting” rewrote single-quote style (including import-only touchpoints); undoing with `git checkout HEAD --` on tracked paths erased the uncommitted Behavior change and forced a recovery re-apply plus focused re-proof before publish.

### Occurrences
- Execution: SEED-068#js-asi-parenthesized-assignment
  - Timestamp: 2026-11-21T00:07:10+08:00
  - Tool: Cursor
  - Model: auto
  - Open Dough release: 0.3.109
  - Evidence: coordinator transcript `51d6175b-e986-46af-bcfe-b3c4b397fcd5` (Black on owned paths → checkout wipe → recovery Task); `open-dough/evidence/recovery-note.txt`; `recovery-focused-proof` (1331 passed); plan Current decisions now record check-only pycodestyle / do not run Black; increment `3dd86338` used `formatting.notRequired`
  - Observed effect: Tracked slice edits wiped after Black; recovery restored seam and proof; delivery completed without autoformat
  - Inference: Resolve this project’s selective-format identity before naming a tool; do not use destructive checkout to reverse formatter noise on uncommitted owned work

## DD-492 — Coordinator implemented a planned slice instead of delegating it

Concrete defect. `dough-execute-plan` says implementation agents return uncommitted changes and the coordinator owns delivery, and it requires reading delegation guidance before the first delegation. For a one-slice reader fix, the coordinator edited the product and tests itself and never read the delegation, return-contract or proof-evidence boundary.

### Occurrences
- Execution: SEED-075#ruby-percent-literal-context
  - Timestamp: 2026-11-26T06:55:00+08:00
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.114
  - Evidence: coordinator session `e5ddabd7`: direct edits to `lizard_languages/ruby.py` and `testRuby.py`, then a delegated refactor pass only; increment `8efebf80`
  - Observed effect: proof was accepted from the coordinator's own runs. There was no implementation return or run record, and no independent implementer read the plan
  - Inference: delegation overhead was probably saved, at the cost of the independence the return contract provides. No defect was observed in the delivered behavior

## DD-493 — Absolute one-second Svelte timing assertions fail `make` under host load

Other. `testSvelte.py`'s linear-time tests assert under 1 s of wall-clock time. Under coverage at load average ~29, six to seven of them failed (1.4–2.8 s) in two `make` runs. They passed alone (92 in 2.4 s), and the full suite without coverage passed.

### Occurrences
- Execution: SEED-075#ruby-percent-literal-context
  - Timestamp: 2026-11-26T07:03:00+08:00
  - Tool: Claude Code
  - Model: claude-opus-5-5
  - Open Dough release: 0.3.114
  - Evidence: coordinator session `e5ddabd7`: `make` failed twice with Svelte `AssertionError: 1.42… not less than 1`; `uptime` load 29.22; `python -m pytest`: 3732 passed
  - Observed effect: about 3 minutes of reruns and diagnosis; the full `make` gate stayed red locally for reasons unrelated to the change
  - Inference: concurrent worktree sessions sharing the machine make absolute wall-clock bounds unreliable as a local gate

## Retention

- Highest allocated local number: 493
- Shared-feedback adoption and compaction recovery (2026-11-07): `7129516bc5bd9ba57f370a6353ffe0e10eb1c861:DearDough.md`; its rows are retained at the linked Open Dough finding owners (`483d3a1e8f0ea24b929675b9a195faca48ac8f47:docs/maintainer/findings/`).
- Shared-feedback compaction recovery (2026-11-03): `4195708bad2061ebc1c829ebdff7868605b173bb:DearDough.md`; its rows are retained at the linked Open Dough finding owners.
