---
id: SEED-053
status: active
planted: 2026-11-10
planted_during: lizard process pr
scope: JS/TS/JSX and Rust function-detection fixes from PR #564
---

# SEED-053: Process PR #564

<a id="process-pr-564"></a>

### Process PR #564 — Fix lost functions in JS/TS/JSX and Rust parsing

**Identity:** SEED-053#process-pr-564
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"6451c4a382e1c1267120e7fedc1042787d5705a2d53ba3f03e50194532303ccb"}}
```

**Goal.** Maintainers disposing
[PR #564](https://github.com/terryyin/lizard/pull/564), and users analyzing
JavaScript, TypeScript, JSX/TSX, and Rust, receive one complete disposition:
real functions those parsers currently miss are reported with correct
boundaries and CCN when the contribution remains valid on current trunk, or
the PR stays unmerged with an evidence-backed reason and any required
follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/564. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
JS/TS/JSX/Rust users whose functions are currently lost to nested Rust block
comments, misread generics/arrows, `static`/`async` object keys, JSX child
expression braces, lone `#` in TSX, parenthesized expression groups, bare
blocks, or a variable named `type`.

**Scope.** This pull request only. Compare the eight claimed parser cases on
current trunk through `lizard_languages` JavaScript/TypeScript/TSX/Rust
readers and the PR's regression tests. Correct remaining defects on a
reachable contributor branch, preserving contributor history, including
conflict resolution against current master when needed. Qualify the final
candidate with the full suite, style checks, and applicable performance
screening for per-token/parser changes. Merge only when those gates and
required hosted checks hold; otherwise record the precise unmet gate.

Deferred promises: unrelated language features, other PRs, and broad
reader refactors beyond the named miss cases. Rejection constraints: do not
merge an inadequate, obsolete, or conflicted candidate to finish the cycle;
do not weaken discriminating JS/TS/JSX/Rust expectations to disguise a
failure; do not accept a material runtime regression without an explicit
recorded decision.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given a Rust nested block comment `/*! ... /* ... */ ... */`, following
  functions remain reported rather than vanishing after an early comment end.
- Given `const r = f<S>(i, (b) => { ... })`, `f` is a generic call and the
  callback remains a function; it is not misread as a generic arrow.
- Given object keys named `static` or `async` followed by `:`, `,`, `}`, or
  `;`, later methods stay nested correctly.
- Given JSX/TSX child expression braces and a lone `#` in a fragment, child
  callbacks remain functions and `#` does not start a macro token.
- Given `(async () => ...)`, `(items.map((i) => {...}))`, a bare block at
  statement start, and `type` used as a variable before `as`, following
  functions remain reported.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `bec8cf92d2e4f43de847797b623030035d25d1d9` on
`AlexSimionGeorge:fix/js-ts-rust-function-detection`; base
`c99b67a136356fd3b8e8348e88bebfdd902324db`; `maintainer_can_modify` true;
merge state MERGEABLE/UNSTABLE; evidence under `.planning/evidence/pr-564/`.

**Outcome.** Merged. Final contributor head
`05a44d255bf419db1c549bb05f92d40349502695` (master merge plus nested-Rust and
generic-call regression tests) landed as merge commit
`f4c7f72883da56538298332647d4c342292ada42`. Acknowledgement:
https://github.com/terryyin/lizard/pull/564#issuecomment-6763552346.
