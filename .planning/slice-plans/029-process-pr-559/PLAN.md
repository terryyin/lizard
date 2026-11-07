# Process PR #559 — Count decisions inside Swift string interpolation

**Identity:** SEED-043#process-pr-559
**Source:** [canonical story](../../seeds/SEED-043-process-pr-559.md#process-pr-559)

## Goal and scope

Swift users get decisions inside `\(...)` string interpolations counted the
same way as equivalent code outside the literal. Maintainers get one
disposition of [PR #559](https://github.com/terryyin/lizard/pull/559): merge
the qualified head with acknowledgement, or leave it unmerged with an
evidence-backed reason and any follow-up linked.

**In scope:** the Swift literal/token change, its tests and changelog note,
review of that concept and its consumers (including condition-list scanning
after #557 and computed-property reporting after #556), resolving the current
merge conflict with trunk, correction on the contributor branch when a defect
remains and the branch is reachable without rewriting history, qualification,
and the merge or the non-merge record.

**Excluded:** other authors' PRs, unrelated language readers, and a new
project-wide correction-time or 3% performance policy. The Fabio cohort's
15-minute budget does not apply. The paired runner's default screening result
is evidence to record.

**Architecture / PFE:** Interpolation splitting belongs in
`lizard_languages/swift_literals.py` beside `literal_spans`, with recursive
tokenization through `SwiftReader.generate_tokens` / `_swift_tokens` in
`swift.py`. Reuse the existing delimiter scan; do not add a second Swift
reader, a parallel decision counter, or a PR-specific switch. Condition-list
and property reporting stay in their existing owners
(`swift_conditions.py`, `SwiftStates`).

**Assumptions:** Head, base and checks are refreshed before a dependent
action. A green contributor suite is evidence of its assertions only.
`maintainer_can_modify` plus a divergent dry-run already showed a write path
to `cpheinrich:codex/swift-string-interpolation`.

## Outside-in proof

Public `analyze_file.analyze_source_code` and the story's key examples.
Owning proof is `test/test_languages/testSwiftTokens.py`
(`TestSwiftStringInterpolation` and existing literal controls). Before merge:
full `nix develop -c python -m pytest`, `nix develop -c make`, a paired
`api` screening on a frozen Swift corpus against current trunk, and required
hosted checks on the exact final head.

## Settled decisive premises

Observed in this preparation workspace at
`1bbc6fce34235b810f44493a54435f6e273c952f` on 2026-11-07. Product bytes match
that revision for trunk observations. PR-head observations used detached
worktree `/tmp/pr-559-head-tree` at
`e613bda0142f016b5d8628b120e16655dc78ea79` with `nix develop -c python`.
Evidence packet:
`/private/tmp/pr-559-evidence/terryyin--lizard--pr559/aafbeec245667587877c050e075a09a6546e952f916a81af58ac293dbe84a14a`
(head `e613bda0142f016b5d8628b120e16655dc78ea79`, open, CONFLICTING with
trunk, 0 review comments). Write-path dry-run to
`https://github.com/cpheinrich/lizard.git` rejected non-fast-forward (not
permission denied).

| Premise | Consuming slice | Observation | Result |
| --- | --- | --- | --- |
| Decisions inside interpolations are not counted on trunk | 1 | `func f(x: Int) -> String { return "v \(x > 0 ? "pos" : "neg") \(x == 1 \|\| x == 2)" }` via `analyze_file` | `f` CCN 1, tokens 13 |
| Nested interpolation decisions are not counted on trunk | 1 | `return "\(flag ? "\(a && b)" : "")"` | `g` CCN 1 |
| Plain / escaped / raw-without-hash strings stay CCN 1 | 1 | text with `\|\|`/`?`, `"\\(a \|\| b)"`, `#"\(a \|\| b) \#(c ? 1 : 2)"#` | each `f` CCN 1; raw still misses the `\#` decision on trunk |
| Condition list with interpolated trailing closure undercounts on trunk | 1 | story `if`/`guard` example | `f` CCN 5 |
| PR head counts the claimed decisions | 1 | same fixtures on `e613bda01` | interp CCN 3 tokens 33; condlist CCN 7; nested CCN 3 |
| PR head focused suite passes | 1 | `pytest test/test_languages/testSwiftTokens.py` on `e613bda01` | 15 passed |
| Trunk still reports computed properties after #556 | 1 | `var score: Int { if flag { return 1 }; return 0 }` | `score` CCN 2 |
| Merge with current trunk conflicts | 1 | `git merge-tree` of `origin/master` and `e613bda01` | conflicts in `CHANGELOG.md` and `lizard_languages/swift.py` only; `swift_literals.py` and tests merge clean |
| Contributor branch is writable without force | 1 | `git push --dry-run` divergent tip to fork branch | non-fast-forward rejection (auth ok) |
| Benchmark command is the performance route | 1 | `paired_benchmark.py --help` in Nix | usage lists `api`, `api-parameters`, `cli` |

## Ordered slices

### 1. Land counted Swift string-interpolation decisions
Type: Behavior
Status: done
Proof: story examples through `analyze_file`; `TestSwiftStringInterpolation` red on trunk and green on the candidate; existing Swift token/literal and computed-property controls stay green; full pytest and `make`; paired `api` screening on a frozen Swift corpus against current trunk (record the interval; do not invent a new hard bound); merge only with hosted checks green on that exact head, else record the unmet gate.

Behavior: Given the story's interpolation, nested, comment, multiline, raw-string, and condition-list examples, public analysis reports the expected CCN, NLOC and function boundaries. Literal text, escapes and non-interpolating raw strings stay non-code. Interpolation parentheses remain code tokens so condition-list scanning and other bracket trackers stay balanced. The qualified candidate is merged onto trunk after the CHANGELOG and `swift.py` conflicts with current master are resolved on the contributor branch without rewriting history, or the PR stays open with the specific unmet gate recorded.

## Current decisions

- Execute in Trunk Mode (`--trunk`). The Swift change is published by merging
  the contributor PR. Lifecycle records land on `origin/master`.
- Preserve contributor history. Do not force-push. Push the merge resolution
  as a forward commit on `cpheinrich:codex/swift-string-interpolation`.
- Keep both Unreleased CHANGELOG bullets (condition lists / computed
  properties from trunk, and interpolation from the PR).
- Integrate `_swift_tokens` / `string_parts` into current `swift.py` without
  dropping `_PropertyHeader` or other trunk SwiftStates behavior.
- Freeze the benchmark corpus before timing. Keep raw JSON outside the
  repository.
- Thank the contributor after a confirmed merge, once, if no acknowledgement
  exists yet.

## Learnings

- Trunk still has the reported defect (CCN 1 vs expected 3).
- The PR head's focused suite passes; the only merge blockers observed are
  CHANGELOG and `swift.py` conflicts with post-#556 trunk.
- Slice-plan refinement was not needed: one behavior slice owns the single
  disposition and its one proof loop. No remaining slice-specific concern.

## Execution complete

Product advice: No product follow-up. Merged PR #559 after resolving the
CHANGELOG and `swift.py` conflicts with post-#556 trunk; story examples hold
(CCN 3 / 7 as claimed); paired API screening on Alamofire showed no
consequential regression (95% interval about −1.30% to +0.78%). Hosted checks
green on `0f6dfae9a`. Acknowledgement posted.
