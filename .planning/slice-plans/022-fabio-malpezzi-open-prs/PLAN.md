# Process FabioMalpezzi's open PRs

**Identity:** SEED-034#process-fabio-malpezzi-prs
**Source:** [canonical story](../../seeds/SEED-034-fabio-malpezzi-prs.md#process-fabio-malpezzi-prs)

## Goal and scope

JavaScript and TypeScript analyzer users get accurate function detection, CCN
and nesting metrics at acceptable speed, and maintainers get one complete
disposition for every PR in the execution-start frozen set: a worthwhile
verified merge with acknowledgement, or an evidence-backed unnecessary /
inadequate conclusion with any >15-minute follow-up linked and left unmerged.

**In scope at planning:** the expected starting queue #551–#554, still open at
preparation. At execution start, refresh
[FabioMalpezzi's open PRs](https://github.com/terryyin/lizard/pulls?q=is%3Apr+is%3Aopen+author%3AFabioMalpezzi),
freeze that set and the starting trunk revision, and replan if the set differs.
**Excluded:** later arrivals, other authors, unrelated language readers. No
phase of a frozen PR's cycle is deferred to another story.

Every frozen PR owns the full [Lizard PR workflow](../../../.agents/skills/lizard-process-pr/SKILL.md)
with the [previous-batch complete-cycle contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md):
reproduce and judge value on the current target; review the whole affected
concept and shared-language consumers; correct only within the 15-minute policy
(review/measurement excluded); prove discriminating public API/CLI regressions;
run fresh independent post-change refactor; pass affected and broad
incremental/cumulative performance with paired 95% upper slowdown strictly
below 3%; pass full suite, style checks and required CI; merge guarded by exact
head with authorized thanks — or record a non-merge disposition with linked
follow-up.

**Architecture / PFE:** Change existing owners rather than add parallel parsers
or PR-specific switches. Expected owners by claim: class-member field/modifier
handling in `javascript_objects` / `typescript_states` (#551); function
signature / generic params in `javascript_functions` / `typescript_declarations`
(#552); TypeScript optional-name tokenization in `typescript.py` (#553);
nested-structures extension in `lizard_ext/lizardns.py` (#554). Follow Accepted
[ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md) and
[ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md). No new
North Star topic is warranted.

**Assumptions:** Sibling order is impact priority (function reporting, then CCN,
then NS), not a supplier dependency. Cumulative accepted-set performance against
the frozen baseline is owned by each merging PR before its merge. Merge and
thanks require execution-time authorization already granted by the Take
instruction when execution starts.

## Outside-in proof

Public `analyze_file` / CLI on each claim fixture; contributor tests once
evaluated on the PR head; focused JS/TS suite from the review contract; then
full `nix develop -c python -m pytest`, `nix develop -c make`, required hosted
CI, and the contract performance protocol before any merge. Per-slice baselines
and expected results: [SLICE-PROOF.md](SLICE-PROOF.md). Retain durable per-PR
evidence under this plan directory and, when useful, a cohort folder under
`docs/reviews/`.

## Settled decisive premises

Observed in this preparation workspace at product revision
`5fbc4f64ec9d68790611833488eb30a8c76ec744` (2026-11-04) via
`analyze_file.analyze_source_code` / `FileAnalyzer` with `ns`, and `gh pr`:

| Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- |
| Open FabioMalpezzi set is still #551–#554 | freeze / all | `gh pr list --author FabioMalpezzi --state open` | True — refresh and freeze again at execution start |
| Heads match preparation OIDs and stay open | all | `gh pr view` | #551 `a8dabb23…`, #552 `c58534ac…`, #553 `692a456c…`, #554 `2368413b…` — OPEN |
| #551 claim still fails on current target | 1 | class field without `;` then `async`/`static`/`get` → no functions; multiline `handler = async () => {}` still one field | Defect remains; multiline control holds |
| #552 claim still fails | 2 | `export function first<T>(list: T[])` → no function | Defect remains |
| #553 claim still fails | 3 | `retryAfter` with `'retry-after'?: string` → CCN 3 (expected 2) | Defect remains |
| #554 claim still fails | 4 | `-ENS` / ns extension: `.catch` callback `max_nested_structures=1` | Defect remains |
| Focused JS/TS proof route exists | all | contract command under Nix | Available — re-run at execution; no product scratch edits remain |

## Ordered slices

### 1. Class methods after semicolon-less fields — PR #551
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/551 — preparation head `a8dabb2351014cbf5164fe31a3472036877130fc`
Proof: [slice 1](SLICE-PROOF.md); complete-cycle gates owned here.
Behavior: Given a class field without `;` followed by `async`/`static`/`get`/`set` on the current target → complete review cycle for #551 → public analysis reports those methods with correct boundaries in JS and TS while multiline async-arrow field values stay fields, or an evidence-backed non-merge disposition is recorded.

### 2. TypeScript generic function declarations — PR #552
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/552 — preparation head `c58534ac54c1f3edbe0fe009322170c4831fcf9f`
Proof: [slice 2](SLICE-PROOF.md); complete-cycle gates owned here.
Behavior: Given a TypeScript function declaration with type parameters → complete cycle for #552 → analysis reports the function with correct end line and parameters without breaking generic methods/arrows, or a non-merge disposition is recorded.

### 3. Quoted optional members vs ternaries — PR #553
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/553 — preparation head `692a456cfa5448a9435cf4e7c43b31eba8c47701`
Proof: [slice 3](SLICE-PROOF.md); complete-cycle gates owned here.
Behavior: Given a quoted TypeScript optional member in a type → complete cycle for #553 → CCN excludes that `?` while a genuine string ternary still adds one, or a non-merge disposition is recorded.

### 4. Member names that look like structures under NS — PR #554
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/554 — preparation head `2368413b2de05b05ff44ce863dcded8061342492`
Proof: [slice 4](SLICE-PROOF.md); complete-cycle gates owned here.
Behavior: Given `.catch(...)` / `?.catch(...)` under `-ENS` → complete cycle for #554 → nested-structure metric does not treat those member names as structures while real `if` / `try`/`catch` still raise NS, or a non-merge disposition is recorded.

## Current decisions

- One Behavior slice per frozen PR; each owns the entire complete-cycle contract.
- Freeze the open set and starting trunk revision at execution start; if it is
  not #551–#554, stop and replan before continuing cycles.
- No sibling supplier agreement; no project S/M/L or numeric slice timing policy;
  the contract's 15-minute correction budget (excluding review/measurement)
  remains the only correction time bound.
- Reuse existing JS/TS / NS owners; do not add parallel counters.

## Concern evidence

- Slice-plan refinement: not needed — four independent complete-cycle Behaviors,
  one proof loop each, cumulative performance owned inside each merging cycle,
  no fragmented outcome or hidden Structure preparation.
- Remaining slice-specific concerns: none identified.
