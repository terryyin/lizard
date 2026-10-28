# PR545 template-interpolation conditions review

Disposition: **left unmerged** — real defect; adapted candidate fails the
strict &lt;3% performance upper bound on incremental names/counts after a
justified quiet longer retry. Follow-up required for performance-qualified
landing. Product changes were not published on the story branch.

Frozen contributor head: `baa95a4df8278421bb3d3a6487516030cc03a979` (OPEN;
historical 4-job checks SUCCESS). Current target during review:
`35b454b778239846de4b467715717f67ac4bc6c3` (includes merged #544). Starting
cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`. Discussion,
reviews and inline comments are empty. metadata.json retains the refreshed
GitHub observation.

## Value

On current target, `analyze_file.analyze_source_code` reports CCN 1 for
`function label` returning `` `${x ? 1 : 2} ${x && y}` `` because interpolation
bodies are opaque tokens to the condition counter. Correct metrics are CCN 3.
Accurate CCN benefits API users and warning thresholds. ADR-0001 governs CCN
meaning; ADR-0000 keeps human architectural ownership. PFE: extend
`TypeScriptReader` / `javascript_templates` — no parallel parser.

## Adaptation

Raw head is stale relative to master (including #544 `LINE_COMMENT`). Adapted
candidate extracts `interpolation_code`, counts conditions inside `${…}` on the
reader fast path, and limits atomic-template backtick scans to TSX/JSX via
`_scan_atomic_templates`. Discriminating tests cover nesting, literals, and the
four extensions. Correction+cleanup fit the ≤15-minute product budget before
performance qualification. literal-git-patches.json holds exact raw/adapted
patches; raw.diff and adapted.diff are DISPLAY ONLY.

Semantic proof on the adapted candidate: story `label` CCN 3 across
`.js`/`.ts`/`.jsx`/`.tsx`; focused suite 611 passed, 5 skipped. Consumer
dispositions: consumer-trace-dispositions.md. Deferred residual (keyword member
inside interpolation) remains out of scope per the story.

## Performance

Quiet microbench on the 1212-file corpus was ~flat (~−1%). Attempts 1–7 are
retained under attempt1/…/attempt7/ (inconclusive or failed gates under load).
Attempt8 prospectively chose 16s API / 24s CLI on the same frozen corpus and
hashes (not an invalid-code retry). Results:

| Comparison | Workload | Median change | Paired 95% CI | Gate |
| --- | --- | ---: | --- | --- |
| target→corrected | API | +0.892% | [+0.491%, +1.807%] | pass |
| target→corrected | names/counts | +1.831% | [+0.441%, +4.767%] | **fail** |
| target→corrected | broad CLI | −0.627% | [−1.748%, +1.010%] | pass |
| baseline→corrected | API | −8.216% | [−8.668%, −7.591%] | pass |
| baseline→corrected | names/counts | −6.608% | [−7.357%, −6.243%] | pass |
| baseline→corrected | broad CLI | +0.311% | [−0.585%, +2.810%] | pass |

Unresolved noise / residual cost on incremental names/counts does not pass.
Further timing of the same candidate to seek a pass is refused by contract.

## Follow-up

Beneficiary: JS/TS/JSX/TSX users of CCN inside template interpolations.
Reproducer: story `label` fixture plus nested/literal controls in inputs.json.
Expected: CCN 3 (and suite expectations) with all six performance gates’
paired 95% upper bounds strictly &lt;3% on a quiet machine, or a further
≤15-minute hot-path reduction that achieves that. Remaining work: qualify or
trim the adapted `_scan_atomic_templates` / `interpolation_code` path under the
contract harness (SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`), then
complete merge/thanks. Source: https://github.com/terryyin/lizard/pull/545.
Evidence root: this directory; external cache
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545`.
