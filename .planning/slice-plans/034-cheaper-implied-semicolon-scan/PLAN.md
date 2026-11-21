# Cheaper implied-semicolon scan for ND/NS

**Identity:** SEED-069#shared-implied-semicolons-scan

**Source:** [Cheaper implied-semicolon scan for ND/NS](../../seeds/SEED-069-shared-implied-semicolons-scan.md#shared-implied-semicolons-scan)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

Reduce the cost of `-END` and `-ENS` for JavaScript and TypeScript while preserving the statement boundaries and nesting results delivered by #578. On the frozen mixed workload, each extension must be at least **3%** faster than #578 (paired median ≤ −3%, or stock 3% harness clear as faster) and within 15% of the runtime immediately before #578. Plain analysis retains its 3% screen. The seed owns the examples and the scope of automatic semicolon insertion covered here. **2026-11-21:** developer selected option 1 — lower the 5% bar; finish remaining proof on the uncommitted call-seam.

## Existing solution and decisions

`lizard_languages/javascript_statements.py` owns the boundary rule. `TypeScriptReader.implied_semicolons` also supplies it to JavaScript, JSX, and TSX. The two extensions take different actions on the same boundary. ND closes its current statement when the condition depth permits it. NS presents a semicolon to its own state machine. Each currently drives a fresh scanner through the token stream after line counting. Preserve the token text, line numbers, and delivery order that feed both extensions and the language reader.

Start with the scanner's state transitions for common tokens. Use the retained profile to choose one cohesive change in `before()` and `_read()` that reduces state work shared by both extensions. Inspect group transitions and pending head, body, label, and loop state together because their ordering affects when a statement ends. Keep a single implementation of the boundary rule and verify the exceptional paths with the existing #578 fixtures. The earlier gate around `before()` gave no measurable gain. Sharing a scanner only when both extensions are enabled cannot meet the single-extension timings. The reader's other expression-boundary checks use their own parsing context and need no changes for this route.

The straight-through case edits `javascript_statements.py` (184 lines at `916d8193`). The existing extension callers and oversized fixture files can supply proof without edits. Apply the 250-line file check if the implementation seam changes. Use metric and language-reader names from [ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md) and follow decision ownership under [ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md).

## Outside-in proof

| Promise | Slice 1 observation |
| --- | --- |
| Preserve implied ends and true nesting | Run `nix develop -c python -m pytest -q test/testNestingDepth.py test/testJavaScriptNestedStructures.py test/testNestedStructures.py test/testNestedStructureScopes.py test/testPythonNestedStructures.py`. These include sibling and nested braceless controls, multiline expressions, blocks versus objects, template/JSX forms, and TypeScript assertions. Also run the sibling witness via `check578.py` through `FileAnalyzer` with both extensions. |
| Preserve function identities, positions, and nesting values on both frozen corpora | With `eq.py`, compare rows across `022f6200` and the candidate for each of `nd`, `ns`, `nd,ns`, and `ns,nd`, using the original `.js` filenames and replaying the same contents with `.ts` filenames to exercise the TypeScript reader. Compare filename, function name, start and end, CCN, ND, and NS. Exercise JSX and TSX with the existing fixtures. |
| Reduce ND and NS runtime independently | Run each on the mixed corpus against both pinned revisions. Require a paired median at most **−3%** against `022f6200` (or stock 3% harness clear as faster) and at most +15% against `c214bb46`. Retain paired rows and intervals for both extensions. |
| Preserve ordinary analysis and consumers | Compare plain API observation digests against `022f6200` on the frozen JS corpus and require the stock 3% paired screen. Run `nix develop -c python -m pytest` before completing under `AGENTS.md`. |

The planning trace searched `ImpliedSemicolons`, `implied_semicolons`, `max_nesting_depth`, and `max_nested_structures` at `916d8193`. Rerun the installed consumer trace on the candidate. The scanner and the two extension paths are exercised through the real `FileAnalyzer`. The option thresholds and output formatters read the same metric fields and are covered by corpus parity and the full suite, including `test_options.py`, `testExtension.py`, and `testOutput.py`. The full suite also covers direct metric readers in JavaScript binding, TypeScript annotation, optional operator, cognitive complexity, PHP, Python, and ST tests. Historical done records and prompt history do not consume live metric values.

## Proof inputs and observations

Use `~/.open-dough/evidence/lizard/SEED-069` for the two corpora, manifests, SEED-066's `check578.py` and `eq.py`, and the preparation replay. The corpora match the original SEED-066 job copies byte for byte. `corpus-js` contains 18 `.js` files and its manifest SHA-256 is `c8506ceb54f39f4b4cc81b53f95346c302f2002efb451135cde3e94708e2b640`. `corpus-mixed` contains 40 `.js` files and its manifest SHA-256 is `7f90da117dffb99441108c6716d0454839b4d8bc7dbef0f165fa9fec056fb773`. Both manifests hash paths and file contents. Use the unchanged names for timings to match SEED-066. The `.ts` replay is a correctness check.

For isolated timings, use detached source trees for `022f62007c48784900102f1ef196b818489f9823` (published #578, identical production bytes to the preparation base) and its first parent `c214bb46689540a8ab39388563d6c1f47e0b645e` (trunk just before #578). Historical SEED-066 q2 timings used the earlier `cb74bd63` baseline before #576/#577 landed. Use fresh pairs on the pinned comparisons for acceptance.

```sh
nix develop -c python .agents/skills/lizard-process-pr/scripts/paired_benchmark.py \
  --baseline "$BASE" --candidate "$PWD" --corpus "$EVIDENCE/corpus-mixed" \
  --out "$EVIDENCE/$RUN.json" --workload api --extension "$EXTENSION" \
  --pairs 40 --seconds 2
```

Run once per base and extension (`nd` and `ns`). For the plain control use `022f6200`, `corpus-js`, and omit `--extension`. Keep outputs outside the corpora and check that reports contain complete rows, valid batches, isolated workers, matching input manifests, and stable observations. For ND and NS, evaluate `100 * (median(candidate_seconds / baseline_seconds) - 1)` from `raw`. The harness's default verdict uses the upper confidence limit of a log-mean ratio. Its API digest omits ND and NS, so the explicit `eq.py` comparison carries nesting proof. Use the default harness verdict for the plain control.

Check host load and competing work immediately before timing. On Territory.local during preparation, load was about 36/23/17 on 16 logical CPUs, so preparation used worker smoke and count profiling only. Run the full screens sequentially on a quiet host and retain a same-revision no-op for a noisy run. If noise overlaps the decision, gather one fresh set on a quieter host before accepting a result. Five screens at 40 pairs and roughly 2 seconds per batch require at least 13 minutes of command time plus calibration. Capture baseline and candidate with the same interpreter and options per pair.

| Premise | Preparation observation at `916d8193` |
| --- | --- |
| The sibling witness distinguishes the #578 behavior | `check578.py` with the Nix interpreter and both extensions gave ND/NS 3/3 from the `c214bb46` source tree and 1/1 from the preparation checkout in both `.js` and `.ts`. CCN stayed 4. The focused suites returned 128 passed and 2 skipped. |
| The parity routes run through the selected readers and report the affected metrics | The `eq.py` replay gave 4,480 matching function rows per suffix across the selected extensions and both orders, with each extension's field compared where present. The preparation replay exercised `.js`, `.ts`, `.jsx`, and `.tsx`. |
| The paired API uses the pinned source trees and frozen workloads | `prepare-observe.py` invoked the actual isolated measurement worker with `--workload api`'s worker spec for ND, NS, and plain on `c214bb46` and `916d8193`. It processed 40, 40, and 18 files respectively, each with a positive sample and stable observation digest. The harness checked imported module locations. Full paired timings belong to Slice 1. |
| Scanner-local work reaches both single-extension runs | The same replay profiled the 40-file mixed workload through `FileAnalyzer`. Each single-extension run called `before()` 52,400 times, `_read()` 21,000 times, and `_statement_ends()` 7,000 times. Both extensions together doubled those counts. Plain analysis made zero calls to the scanner. |

Replay the retained observations with `nix develop -c python "$EVIDENCE/prepare-observe.py" "$PWD" "$PRE_578" "$EVIDENCE"` from the preparation checkout, with `PRE_578` pointing to an exported `c214bb46` tree. The retained observation log is `preparation-observations.txt`.

## Slices

### 1. Reduce statement-boundary scan cost for ND and NS

Type: Behavior
Status: done
Proof: Quiet C1 miss (+1.93% vs #578) shows scanner-local edits alone do not meet ≤ −5%; C3 parity held on attempt-2 before revert; attempt-2 product reverted. No product delivery; promises moved to slice 2 after overrun refinement.

Behavior: Given a JavaScript-family file analyzed with ND, NS, or both, Lizard reports the same nesting and ordinary metrics as #578. The same frozen mixed input analyzed with either extension alone meets both runtime targets. Plain analysis stays within its prior screen.

Learning: Cohesive `before()`/`_read` micro-opts (attempts 1–2) do not deliver C1/C2. Human authorized moving the change to the ND/NS call seam; slice 1 overran (15.5 min) before that work started. Outcome owned by slice 2.

### 2. Cut ND/NS implied-semicolon drive cost at the call seam

Type: Behavior
Status: planned
Proof: Same Outside-in proof table as the plan header (C1–C4 / commitments C1–C4); profile note retained under evidence.

Behavior: Same user-visible promises as slice 1. Implementation changes how ND and/or NS construct and drive `ImpliedSemicolons` (and only a shared scanner API if both still use one boundary-rule implementation). Do not rely on polishing `javascript_statements.py` alone. Do not share a scanner only when both extensions are enabled.

Start from a short profile of #578 vs pre-578 ND/NS time (fraction in `before` vs extension loop vs rest) on the frozen mixed corpus, then one cohesive call-seam change, then the mapped proof. Fresh clock via `--split-from 1`. Timed commands through the slice clock.

### 3. Finish proof under amended 3% bar

Type: Behavior
Status: planned
Proof: Parity/plain/full suite passed; C1 ND vs578 harness-clear. C2/C3 runtime failed — remaining outcome owned by slice 4 (do not mark done until C2/C3 close).

Behavior: Same amended goal (3% vs #578 / harness clear; ≤ +15% vs pre-#578; C3/C4/C5–C7 parity; plain 3%). Complete eq parity, NS and pre-578 timings, plain screen, full suite, and consumer-trace. Reuse focused C3 evidence where still valid. Fresh clock via `--split-from 2`.

### 4. Deepen NS drive and pre-578 recovery

Type: Behavior
Status: planned
Proof: C2 (NS ≤ −3% vs #578 or harness clear; NS ≤ +15% vs pre-578); C3 (ND ≤ +15% vs pre-578); retain C1/C4–C8.

Behavior: Under developer option-2 scope: deepen NS `_unterminated_statements` and shared ASI drive so NS clears the amended #578 bar, and deepen shared call-seam/ASI work enough that ND and NS clear ≤ +15% vs pre-#578. Keep single `ImpliedSemicolons` boundary rule. Fresh clock via `--split-from 3`.

## Current decisions and learnings

- **2026-11-20 attempt 1 (retained miss):** Inlining `(`/`[`/`)`/`]` handling into every `before()` call (before the `TRACKED` gate) plus early group/pending short-circuit for `_statement_ends` preserved C3 parity but **regressed** ND vs #578: paired median **+9.03%** (need ≤ −5%); CI [3.02, 15.36] under load ~21. Same-revision no-op median −2.65% (CI [−7.68, 4.13]) does not explain the miss. Evidence: `open-dough/evidence/timing-nd-vs578.json`, `timing-noop-nd.json`, `slice1-report.md`.
- **Design constraint after attempt 1:** Keep a **single** membership/`TRACKED` (or equivalent) gate on the common non-tracked path. Do **not** add per-token equality chains for group delimiters outside that gate. Specialize group and pending head/body/label/loop transitions only **after** entry into `_read` (or an equivalent already-gated path). Prefer cutting attribute traffic / local bindings inside `_read` and `_statement_ends`. `__slots__` is fine. Still no reader/extension plumbing expansion.
- **2026-11-20 attempt 2 (retained miss):** Applied that constraint (`_hold`/`_active`/`_block`, group equality only in `_read`). C3 still passes. ND vs #578 median **+1.08%**, CI [−3.33, 10.82] entirely above −5% under load ~45. Recovered attempt-1 regression but no win. Evidence: `a2-timing-nd-vs578.json`, `slice1-attempt2-report.md`.
- **Stop for human judgment (2026-11-20):** Two scanner-local cohesive attempts have not met C1. Slice clock ~2.7 min remaining (not enough for another full timing screen). Remaining scanner-local surface may be too small for the −5% target; plan forbids expanding into reader/extension plumbing without reassessment. Uncommitted attempt-2 scanner kept (parity-preserving). Need developer direction before another design path, target change, or quiet-host remeasure-only.
- **2026-11-21 resume:** Developer directed “Try again.” Host load now ~2.8/2.4/2.4 (16 CPUs). Continue with **quiet-host remeasure** of current attempt-2 only (no new design unless that remeasure clearly fails ≤ −5% again).
- **2026-11-21 quiet remeasure (retained miss):** Attempt-2 unchanged. ND vs #578 median **+1.93%**, CI [−2.36, 3.58] under load ~3.7 — still entirely above −5%. Same-rev noop median +2.53%. Loaded a2 (+1.08%) was not a load artifact. No further scanner-local hypothesis without expanding scope. Evidence: `quiet-c1-nd-vs578.json`, `quiet-noop-nd.json`, `quiet-remeasure-report.md`.
- **2026-11-21 human authorization:** Proceed as recommended — stop polishing `javascript_statements.py` alone; make one cohesive change at the **ND/NS call seam** (how each extension builds/drives the scanner after line counting), guided by a short profile of the #578 delta vs pre-578. Keep a single boundary-rule implementation and C3 parity. “Share only when both extensions are enabled” remains ruled out. Revert the non-winning attempt-2 scanner micro-opt before that work.
- **2026-11-21 slice 1 overrun:** Clock **15.5** active minutes (`overrun: true`, `storyOverruns: 1`). Failed sizing: one Behavior slice could not absorb two failed scanner-local attempts, quiet remeasure, human judgment, and a call-seam pivot with full C1–C4 proof. Product tree clean (attempt-2 scanner reverted). Call-seam work not started. Completed retained proof: scanner-local surface insufficient for ≤ −5% (quiet C1 **+1.93%**); C3 held on attempt-2 before revert; corpora/harness/trees remain valid. Replanning allowed → refined: slice 1 abandoned; slice 2 owns call-seam + proof with `--split-from 1`.
- **2026-11-21 slice 2 overrun (`storyOverruns: 2`):** Clock reported **197** active minutes (slice left running across a long session gap; real productive window was the morning call-seam work). Uncommitted call-seam kept: `implied_semicolon_drive.bind_implied_semicolon_before_at`, ND context-method binding, `ImpliedSemicolons.before_at(token, line)`, NS uses the same bind. Evidence: C3 focused pytest 128/2 + check578 ND1/NS1; C1 ND vs #578 raw median **−2.90%** (need ≤ −5%), CI [−6.94, 1.42], harness 3% screen “passes” — **story bar still missed**. C2/C4/eq/full suite not run. Profile (take ND): `before` ~0.41s of ~3.66s analyze (~11%).
- **Story-boundary reassessment (required at 2 overruns):** Delivered value = call-seam progress (~3% toward #578, C3 green on focused suites). Remaining scope = still ~2pp short of −5% plus NS/plain/parity/full proof. Sizing reason not to continue with another slice-only refinement: ASI is only ~11% of ND time; easy call-seam binding already spent; closing another 2pp inside 15 minutes without changing the story bar or authorizing a deeper pipeline change is not credible. Stop for human on goal/target vs further scope.
- **2026-11-21 developer chose option 1:** Lower the runtime bar from 5% to **3%** (≤ −3% median or stock 3% harness clear as faster vs #578); keep ≤ +15% vs pre-#578; finish C2/C4/eq/full suite on the current call-seam. Fresh source review required after this goal amendment.
- **2026-11-21 slice 3 proof return:** Parity/plain/full suite green. **C1** ND vs578 harness-clear (−2.90%). **C2 FAIL:** NS vs578 median +1.41% (remeasure); NS vs pre578 +25.40%. **C3 FAIL:** ND vs pre578 +24.99% (need ≤ +15%). Evidence: `slice3-proof-report.md`. Call-seam helps ND vs #578 modestly; does not bring ND/NS within +15% of pre-#578 (still ~#578-era overhead). Stop for human on pre-578 ceiling and/or NS vs578.
- **2026-11-21 developer chose option 2 — authorized design scope:** (a) deepen the **NS** implied-semicolon drive (`_unterminated_statements` / shared bind) so NS meets the amended ≤ −3% vs #578 (or harness clear); (b) deepen shared ASI/call-seam work as needed so **ND and NS** also meet ≤ +15% vs pre-#578. Keep one boundary-rule implementation; preserve C3/C4–C8 parity. No metric-definition or SEED-068 ASI-case expansion.
- **2026-11-21 slice 4 return:** (a) **met** — NS vs #578 median **−5.68%** harness pass; ND vs #578 harness clear (−0.22% remeasure). (b) **not met** — ND vs pre578 **+18.07%**, NS vs pre578 **+22.79%** (need ≤ +15%). Parity/suite green; C8 plain noisy under load (ASI unused). Profile: NS `before_at` ~4% of analyze — not enough headroom inside call-seam/ASI alone to reclaim another ~5–10pp vs pre-578. Evidence: `slice4-report.md`. Stop for human on pre-578 ceiling (or authorize scope beyond call-seam/ASI drive).
- **2026-11-21 developer chose option 3 — stop / revert:** Uncommitted call-seam product reverted (`lizardnd.py`, `lizardns.py`, `javascript_statements.py` restored to Take; `implied_semicolon_drive.py` removed). No product delivery. Planning/evidence retained for history. Execution stops unfinished; Taken entry remains until wrap-up/release.

## Plan review

After slice-1 overrun, the plan was refined in place: scanner-local Behavior closed as learning; remaining outcome is one Behavior slice at the ND/NS call seam. Commitments C1–C4 stay allocated to the slice that completes them (slice 2). Slice count: 2 (1 done as learning + 1 planned, now overrun). At `storyOverruns: 2`, story-boundary reassessment recorded above — no further slice-only refinement until the developer decides.
