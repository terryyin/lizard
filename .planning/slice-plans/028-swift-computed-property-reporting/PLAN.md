# Judge Swift computed-property reporting

**Identity:** SEED-041#swift-computed-property-reporting
**Source:** [canonical story](../../seeds/SEED-041-swift-computed-property-reporting.md#swift-computed-property-reporting)
**Source review:** [source-review.md](./source-review.md)

## Goal and scope

Swift users and maintainers get a recorded decision about computed properties
with an implicit getter and properties initialized by a closure. The existing
Swift reader changes only when a maintainer accepts that the reporting is
worth its cost and every required paired 95% upper slowdown bound is strictly
below 3%. Until both hold, trunk stays as it is and
[PR #556](https://github.com/terryyin/lizard/pull/556) stays unmerged.

**In scope:** confirm the story examples on current trunk; evaluate the open
PR head `363cfb19adde9cb10ed0a1d0a40085c9df7cd574` (review head
`0f323dbfb22ad4bc999563bfb1e84480148cb2ac`) in `lizard_languages/swift.py`
and its contributor tests in `test/test_languages/testSwiftAccessors.py`;
paired `api`, `api-parameters`, and `cli` against current trunk on a quieter
host than the 2026-11-06 runs; land with contributor history when both the
worth judgment and the bound hold, or record the judgment on the PR and leave
it unmerged.

**Excluded:** other languages and other pull requests; a second Swift reader
beside `SwiftStates`; merging #556 only to close review; treating the
abandoned SEED-038 candidate
`8c0f3b7202eb1e66c3bf06c5045df058d5af542e` as accepted code. No cohort
cumulative baseline for this story. Clearing the &lt;3% bound does not waive
the maintainer worth judgment, and a worth judgment does not waive a failed
bound.

**PFE:** Property and accessor discovery already belong to `SwiftReader` /
`SwiftStates` in `lizard_languages/swift.py`, with accessor coverage in
`test/test_languages/testSwiftAccessors.py` (and related Swift reader tests).
Reuse those owners. The PR is a candidate patch on them, not a new subsystem.

**Performance:** installed
`.agents/skills/lizard-process-pr/scripts/paired_benchmark.py`. Workloads
`api`, `api-parameters`, and `cli` against current trunk. Freeze a Swift
source corpus before timing. Each paired 95% upper slowdown must be strictly
below 3%. The 2026-11-06 retained runs under host load about 12–28 on 16
cores did not meet that bound (mixed-corpus Swift `api-parameters` upper
bounds 11.4% and 23.8%; contributor-patch-only 13.3%); broad Python `cli`
passed with upper bound 2.79%. Revisit on a quieter host before relying on
those figures.

## Outside-in proof

Public `analyze_file.analyze_source_code` on the story examples.
Discriminating contributor property-function tests fail without the behavior
and pass with it; existing accessor, stored-property, protocol, and local
controls stay green. Focused Swift reader tests the change reaches, then
`nix develop -c python -m pytest`, `nix develop -c make`, required hosted CI
on the exact landable head, and every required paired workload before merge.
When no candidate clears both gates, the recorded PR judgment and unchanged
trunk are the proof of the reject disposition.

## Settled decisive premises

Observed 2026-11-06 in this preparation workspace at
`b1052466a472010d753c0853966225514603a069` with `nix develop -c python` and
`lizard.analyze_file.analyze_source_code`, unless a row names another source.
The applied patch was `git apply` of `/tmp/pr556-obs/pr.diff` (from
`gh pr diff 556`), then `git checkout --` of touched product paths; the tree
held only the seed edit afterward. Intake packet also retained under
`/private/tmp/lizard-pr-556-evidence/terryyin--lizard--pr556/d75e4f0ceb6b2d428633e19016bc7a894096da42356382421946c985e80bd425`.
Prior paired JSON under `/private/tmp/lizard-pr556-bench/`.

| Premise | Observation | Result |
| --- | --- | --- |
| PR #556 still open at disposition head | `gh pr view 556`, head `363cfb19adde9cb10ed0a1d0a40085c9df7cd574`, state OPEN | True |
| Maintainer can modify the head branch | `gh pr view 556 --json maintainerCanModify` | true |
| Patch applies to this trunk | `git apply --check` then apply of retained `pr.diff` on `b1052466a` | Applies |
| `score` / `handler` missing on trunk | public analyzer, no patch; `explicit` accessors present | only `[('get', 2, 1), ('willSet', 1, 1)]` |
| Same properties after the patch | public analyzer, patch applied | `score`, `handler`, `get`, `willSet` (and `local` for a local function control) |
| Stored / local controls after the patch | same scratch | `stored` absent; function-local closure not a separate property function |
| Contributor accessor file green with patch | `pytest test/test_languages/testSwiftAccessors.py` after apply | 23 passed |
| Paired harness present | `.agents/skills/lizard-process-pr/scripts/paired_benchmark.py --help` | workloads `api`, `api-parameters`, `cli`; default limit 3% |
| Prior mixed Swift `api-parameters` failed the &lt;3% bound | `/private/tmp/lizard-pr556-bench/api-parameters-mixed-4s.json` and `-retry.json` | upper 11.4% and 23.8%; verdict `bound-not-met` |
| Prior contributor-patch-only mixed run failed | `api-pronly-mixed-4s.json` | upper 13.3%; verdict `bound-not-met` |
| Prior broad `cli` passed | `cli-4s.json` | upper 2.79%; verdict `passes` |
| Prior noisy/short batches inconclusive | `api-parameters.json` | verdict `inconclusive-short-batches` |

## Ordered slices

### 1. Decide computed-property reporting under the 3% and worth gates
Type: Behavior
Status: done
Proof: story examples through `analyze_file.analyze_source_code`; focused
Swift accessors 23 passed on candidate; full pytest 3043 passed and `make`
on head `363cfb19ad`; quieter-host paired `api` / `api-parameters` / `cli`
uppers 0.956% / 1.930% / 0.359% (`/private/tmp/lizard-pr556-bench-quiet/`);
maintainer worth accept 2026-11-06; hosted CI green on exact head; merged
`96b45e2ae` preserving contributor history; thanks posted.
Behavior: Given the confirmed trunk defects and PR #556 as candidate →
evaluate the current PR head in the existing Swift reader owners → when both
the maintainer worth judgment and the &lt;3% bound hold, land with contributor
history preserved and thank the contributor; otherwise record the unmet gate
on the PR, leave it unmerged, and leave the reader on trunk unchanged.

## Current decisions

- Keep the story's strict &lt;3% paired upper-bound contract on `api`,
  `api-parameters`, and `cli` versus current trunk. No cumulative cohort
  baseline.
- Keep the separate maintainer worth judgment: a cleared bound does not accept
  the change by itself.
- Change only the existing Swift reader owners; never add a second parser.
- Bound search: evaluate the current PR head. If it fails the bound or the
  worth judgment, the reject disposition completes the story. Do not invent a
  second Swift implementation path in this plan.
- Freeze baseline/candidate checkouts and a Swift corpus before timing;
  remeasure on a quieter host before treating the 2026-11-06 figures as
  decisive; retain every valid measurement, including failures.
- Prefer updating the contributor branch when `maintainerCanModify` remains
  true; otherwise an equivalent exact-head vehicle that keeps contributor
  history.
- Thank the contributor only after a confirmed merge.
- Quieter-host remeasure cleared all three workloads; maintainer accepted
  worth; PR #556 merged at `96b45e2ae` (head `363cfb19ad`).
- Delivery disposition for this increment: `records-only` (plan + source
  review). Product landed on trunk via merge of PR #556.

## Concern evidence

- Slice-plan refinement: not needed — one Behavior owns the complete
  disposition, one proof loop, and the worth gate is a current decision rather
  than a second outcome.
- Remaining slice-specific concerns: none identified.

## Execution complete

Product advice: no change. Both gates held (quieter-host paired uppers
0.956% / 1.930% / 0.359%; maintainer worth accept). PR #556 merged at
`96b45e2ae` with contributor history preserved; thanks posted. No product
correction or backlog recommendation from the retrospective.
