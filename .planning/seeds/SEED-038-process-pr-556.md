---
id: SEED-038
status: active
planted: 2026-11-06
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/556
scope: Swift computed properties and closure-initialized properties
---

# SEED-038: Process PR #556

## Stories

<a id="process-pr-556"></a>

### Process PR #556 — Swift computed properties and closure initializers

**Identity:** SEED-038#process-pr-556
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/024-process-pr-556/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"0d3ea7b146f387baf6125f4250d6cbb5255e204683ae8bc26d1c180dd69d4ccd","plan":"7e539a1284eb2ed3dc861abf023ff4ffe7692d81d9e95e27bdfcca3f1f38a102"}}
```

**Goal.** Swift analyzer users and maintainers receive one evidenced disposition
for [PR #556](https://github.com/terryyin/lizard/pull/556): a worthwhile change
verified and landed with acknowledgement, or an evidence-backed unnecessary,
already-fixed, or inadequate conclusion with any required follow-up linked and
left unmerged.

**Source.** Maintainer instruction: `/lizard-process-pr
https://github.com/terryyin/lizard/pull/556`.

**Beneficiary.** Swift users whose types declare computed properties with an
implicit getter, or properties initialized by a closure, and maintainers
deciding whether to accept this contribution.

**Scope.** This PR only, from product-backlog intake through evidence-based
review of the Swift reader, any bounded correction that preserves contributor
history, independent refactor of the owned change, full local qualification,
hosted CI, and an authorized merge or a precise non-merge disposition.

Required behavior:

- Reproduce the claimed examples and nearby controls on the current trunk and
  on the proposal, through `analyze_file` or the CLI. Judge a real defect
  against an obsolete example, already-fixed behavior, or an invalid fixture.
- Review the whole affected concept and its consumers, including explicit
  accessors, stored properties, protocol requirements, locals, other Swift
  function boundaries, and shared metrics such as NLOC, token count, CCN, and
  `-Eoutside` global counts.
- Correct a worthwhile change on the contributor branch when that branch is
  reachable without rewriting history. Leave an inadequate candidate unmerged
  and link the remaining work when a complete correction cannot be finished
  inside this disposition.
- Qualify the final revision with discriminating regressions, the full suite,
  and style checks. Because the change is in the Swift token reader, also
  require the paired-benchmark gate: each required workload's paired 95% upper
  slowdown bound strictly below 3%, on the affected API and the broader CLI,
  against the current target. Refresh required hosted checks and guard any
  merge with the exact head SHA.

Deferred promises: other authors' PRs; unrelated language readers; a new
universal correction-time policy. Rejection constraints: do not merge an
inadequate candidate to finish the cycle; do not weaken an expectation to
disguise an implementation failure; do not add a parallel Swift counter or a
PR-specific switch beside the existing reader; do not force-push the
contributor branch.

Assumptions: `maintainer_can_modify` at intake does not by itself prove a
write path; confirm it before pushing corrections. The 3% paired bound is this
story's selected performance gate for the Swift reader change, recorded here
rather than treated as a new project-wide timeout. The Fabio cohort's
15-minute correction budget does not apply to this author.

**Key examples.** Claims to verify on the current target through the analyzer
or CLI; not automatic acceptance. Already-correct trunk or an unworthy change
receives a recorded reason instead of a merge.

- Given a computed property with an implicit getter, such as
  `var score: Int { if flag { return 1 }; return 0 }`, public analysis reports
  a function `score` whose branches are not counted only in the enclosing
  scope. An explicit `get { ... }` that is already reported stays reported.
- Given a property initialized by a closure, such as
  `let handler: (Int) -> Int = { x in if x > 0 { return 1 }; return 0 }`,
  including `lazy var x = { ... }()`, analysis reports a function named for
  that property. The function starts at the property name, so a type
  annotation that continues on the next line, and header lines before the
  brace, belong to that function's NLOC and tokens.
- Given braces that hold accessors (`get`, `set`, `willSet`, `didSet`, and
  the other accessor keywords the reader already knows), analysis still
  reports those accessors and leaves the enclosing scope's NLOC and token
  count unchanged. Stored properties, protocol requirements, and locals inside
  functions stay unreported as new functions. `-Eoutside` global counts stay
  the same around accessor blocks.

**Frozen at intake.** Head `0f323dbfb22ad4bc999563bfb1e84480148cb2ac` on
`cpheinrich:codex/swift-implicit-getters`; PR base
`b6b6b8c671bab79da1a6c28c61a2076e4735998d`; trunk at intake
`ab0138ecc187262cb57ea0395a1175c10a393595`. Refresh head, base, and checks
before any dependent action. `maintainerCanModify` was true at intake.

**Authority.** Full process-PR scope: review, reproduce, correct on the
contributor branch when reachable, qualify, and merge when the gates above
hold, or record a precise non-merge disposition. Publish ordinary lifecycle
records. Thank the contributor after a confirmed merge.
