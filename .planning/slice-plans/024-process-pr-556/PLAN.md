# Process PR #556 — Swift computed properties and closure initializers

**Identity:** SEED-038#process-pr-556
**Source:** [canonical story](../../seeds/SEED-038-process-pr-556.md#process-pr-556)

## Goal and scope

Swift users get computed properties with an implicit getter, and properties
initialized by a closure, reported as functions named for the property, with
their branches, NLOC and tokens on that function. Maintainers get one
disposition of [PR #556](https://github.com/terryyin/lizard/pull/556): merge
the qualified head with acknowledgement, or leave it unmerged with an
evidence-backed reason and any follow-up linked.

**In scope:** the Swift reader change, its tests and changelog note, review of
that concept and its consumers, correction on the contributor branch when a
defect remains and the branch is reachable without rewriting history,
qualification, and the merge or the non-merge record.

**Excluded:** other authors' PRs, unrelated language readers, and a new
project-wide correction-time policy. The Fabio cohort's 15-minute budget does
not apply.

**Architecture / PFE:** The responsibility already lives in `SwiftStates`
inside `lizard_languages/swift.py`, which reports `func`, `init`, `subscript`
and explicit accessors. Change that state machine. Do not add a second Swift
reader, a token-wrapper counter, or a PR-specific switch. Follow Accepted
[ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md): the new
units are functions with CCN, NLOC and token count. No North Star topic is
warranted.

**Assumptions:** Head, base and checks are refreshed before a dependent
action. A green contributor suite is evidence of its assertions only. This
story's performance gate is a paired 95% upper slowdown strictly below 3% on
the affected API and the broader CLI, against the current target. It is not a
cumulative comparison to the Fabio cohort baseline.

## Outside-in proof

Public `analyze_file.analyze_source_code` and `FileAnalyzer` with `outside`,
plus one CLI run of the same fixtures. Owning proof is
`test/test_languages/testSwiftAccessors.py` (`TestSwiftPropertyFunctions` and
the existing accessor controls). Before merge: full
`nix develop -c python -m pytest`, `nix develop -c make`, the paired benchmark
below, a consumer-trace disposition, and required hosted checks on the exact
head.

## Settled decisive premises

Observed in this preparation workspace at `ea51a55f71c86ba29530d71ecf671b1526d3e110`
on 2026-11-06. Product bytes match that revision. The Swift reader observation
used `nix develop -c python` and `analyze_file.analyze_source_code("a.swift", …)`
/ `FileAnalyzer(get_extensions([Outside()]))`. The candidate observation applied
the packet diff in this workspace, ran
`nix develop -c python -m pytest test/test_languages/testSwiftAccessors.py -q`,
then reverted the patch. Evidence packet:
`/private/tmp/lizard-pr-556-evidence/terryyin--lizard--pr556/d75e4f0ceb6b2d428633e19016bc7a894096da42356382421946c985e80bd425`
(head `0f323dbfb22ad4bc999563bfb1e84480148cb2ac`, open, 0 review comments).

| Premise | Consuming slice | Observation | Result |
| --- | --- | --- | --- |
| Implicit getters are not functions; their `if` is global | 1 | `var score: Int { if flag … }`, `var body: some View { if flag … }`, annotation continued on the next line | `function_list` empty; `*global*` CCN 2 |
| Closure initializers are not functions | 1 | `let handler: (Int) -> Int = { x in if x > 0 … }` and `lazy var x = { … }()` | `function_list` empty; `*global*` CCN 2 |
| Explicit accessors stay functions and do not raise global CCN | 1 | `get { if origin … }` and `willSet` / `didSet` | `get` / `willSet` / `didSet` reported; `*global*` CCN 1 |
| Stored properties, protocol requirements and locals are not new functions | 1 | protocol `var name: Int { get }`, stored `let`/`var`, closure inside `func local` | only `local` reported; `*global*` CCN 1 |
| Contributor diff applies on this trunk and its Swift tests pass | 1 | `git apply --check` then apply, pytest, revert | check exit 0; 23 passed; patch reverted |
| Benchmark command is the slice's performance route | 1 | `nix develop -c python .agents/skills/lizard-process-pr/scripts/paired_benchmark.py --help` | usage lists `api`, `api-parameters`, `cli` |

## Ordered slices

### 1. Report Swift property bodies as functions
Type: Behavior
Status: planned
Proof: story examples through `analyze_file` and `-Eoutside`; `TestSwiftPropertyFunctions` red on trunk and green on the candidate; existing `TestSwiftAccessors` controls stay green; CLI lists the same function names; consumer-trace dispositions; full pytest and `make`; paired `api-parameters` on a frozen Swift corpus of the story examples and `cli` on the reconstructed broad corpus from `docs/reviews/fabio-malpezzi-20261027/README.md`, each with paired 95% upper slowdown strictly below 3% against the current target. Merge only with hosted checks green on that exact head; otherwise record the unmet gate.

Behavior: Given the implicit-getter and closure-initializer examples in the story, public analysis reports a function named for the property, starting at the property name, and those branches are not left only in `*global*`. Accessor blocks still report `get` / `set` / `willSet` / `didSet` (and the other accessor keywords the reader already treats as functions) without moving the enclosing scope's NLOC or token count onto a property function. Stored properties, protocol requirements and locals inside functions stay as they are on trunk. The qualified candidate is merged, or the PR stays open with the specific unmet gate recorded.

## Current decisions

- Execute in Trunk Mode (`--trunk`). The Swift change is published by merging
  the contributor PR. Lifecycle records land on `origin/master`.
- Preserve contributor history. Do not force-push. Push a correction only
  after `maintainer_can_modify` and a non-mutating dry-run show a write path.
- Review the applied reader before accepting it. A passing focused suite does
  not replace that reading. Correct in the existing `SwiftStates` owner.
- Freeze both benchmark corpora before timing. Do not time only the changed
  syntax for the CLI workload. Keep raw JSON outside the repository.
- Thank the contributor after a confirmed merge, once, if no acknowledgement
  exists yet.

## Learnings

- Trunk still has the reported defect. The PR diff applies here and the Swift
  accessor file, including the new property tests, passes on that applied tree.
- Slice-plan refinement was not needed: one behavior slice owns the single
  disposition and its one proof loop. No remaining slice-specific concern.
