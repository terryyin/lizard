# Process PR #530 — Fix Python inline function discovery and suite boundaries

**Identity:** SEED-049#process-pr-530
**Source:** [canonical story](../../seeds/SEED-049-process-pr-530.md#process-pr-530)

## Goal and scope

Python users and maintainers get one complete disposition of
[PR #530](https://github.com/terryyin/lizard/pull/530) for this launch: merge
only when the story examples hold and every Fabio cohort paired 95% upper
slowdown is strictly below 3% on frozen Python `api`, `api-parameters`, and
`cli`; otherwise leave the PR unmerged with the unmet gate recorded.

**In scope:** refresh head/base/write-path; reproduce trunk vs head on the
story examples; reuse SEED-042 paired evidence when production Python reader
bytes, head SHA, corpus/harness contract, and gate still match; merge only
on a reachable contributor branch when both gates hold; otherwise reaffirm
the unmerged disposition without a second public judgment when one already
names this head.

**Excluded:** the multiline f-string `end_line` tokenizer limit; closed PRs
#526 and #529; other languages; dropping the Fabio cohort 3% bound; forcing
a push to `ahuimanu:fix/python-inline-suites`.

**PFE:** Inline-suite discovery belongs to `PythonReader` / `PythonIndents` /
`PythonStates` and Python reader tests. No second parser.

## Outside-in proof

Public `analyze_file.analyze_source_code` on the story examples. Retained
SEED-042 / SEED-045 judgment and paired measurements when still applicable.
Full suite and hosted CI only for a landable corrected head.

## Settled decisive premises

Observed 2026-11-07 in workspace
`/Users/terryyin/git/lizard/.worktrees/lizard-process-pr-https-github-com-terryyin-liza`
at `caee1cbf243eef4a50c207ceaf9f9634f86e0a18` with `nix develop -c python`,
unless a row names another source. PR head observed in detached worktree at
`2948b38ed0bf70c5a4c39163b45dfa50c3020314`.

| Premise | Consuming slice | Observation | Result |
| --- | --- | --- | --- |
| PR #530 still open at prior disposition head | 1 | `gh pr view 530` | OPEN; head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` |
| Trunk still misses `Stream.read` / `Stream.close` | 1 | `analyze_file` on current HEAD | `[]` |
| Block `kept` control unchanged on trunk | 1 | `analyze_file` | CCN 2, NLOC 4 |
| PR head discovers Stream methods and keeps nested `outer` | 1 | same fixtures on `2948b38ed` | `read`/`close`; `outer` + `outer.inner` |
| PR head still reports shared-line docstring NLOC 0 | 1 | `def value(): """A value."""; return 1` on head | NLOC 0 |
| `python.py` / `python_structure.py` unchanged since SEED-042 baseline | 1 | `shasum` of HEAD vs `b1052466a47` | identical |
| Contributor branch not maintainer-writable | 1 | `maintainer_can_modify` false; `gh api` fork `permissions.push` false | no write path |
| Prior paired evidence still names this head and gate | 1 | SEED-042 judgment comment `6687330144`; SEED-045 disposition | both candidates failed &lt;3% on api / api-parameters |

## Ordered slices

### 1. Confirm PR #530 disposition under the 3% gate
Type: Behavior
Status: done
Proof: story examples through `analyze_file` on trunk and head; write-path
and production-hash checks above; reuse SEED-042 paired results when
compatible, otherwise remeasure; merge only when behavior and every required
paired upper bound hold on a reachable branch; otherwise record unmerged
disposition for SEED-049 and leave trunk's Python reader unchanged.
Behavior: Given unchanged head `2948b38ed0bf70c5a4c39163b45dfa50c3020314`,
unchanged Python reader bytes on trunk, no contributor write path, and prior
paired failures still applicable → complete this launch with **unmerged —
real defect, performance gate unmet**, without posting a duplicate public
judgment when comment `6687330144` already covers this head.
Evidence: `.planning/evidence/pr-530/SEED-049-disposition.md`

## Current decisions

- Keep the Fabio cohort strict &lt;3% paired upper-bound contract.
- Do not force-push the fork; do not ask for further force-pushes on this head.
- Reuse SEED-042 measurements; do not retime solely to confirm a known fail.
- Prefer one local disposition note under `.planning/evidence/pr-530/` for
  this identity; keep the existing public judgment URL.

## Concern evidence

- Slice-plan refinement: not needed — one Behavior owns the complete
  disposition with settled premises.
- Remaining slice-specific concerns: none.

## Execution complete

Product advice: No product follow-up from this launch. PR #530 stays unmerged
at head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` because the Fabio cohort
paired &lt;3% gate remains unmet and the contributor branch is not writable.
Prior public judgment stands. A future approach that clears every required
paired upper bound can reopen the disposition.
