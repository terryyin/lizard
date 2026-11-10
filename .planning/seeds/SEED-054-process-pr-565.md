---
id: SEED-054
status: active
planted: 2026-11-10
planted_during: lizard process pr
scope: Java method detection fixes from PR #565
---

# SEED-054: Process PR #565

<a id="process-pr-565"></a>

### Process PR #565 — Fix Java methods lost, invented or misnamed

**Identity:** SEED-054#process-pr-565
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"0eaca3766e3cb0d195dbec24cbcf4abb6b10afcdb8a239e73ce67c83bbd2f02b"}}
```

**Goal.** Maintainers disposing
[PR #565](https://github.com/terryyin/lizard/pull/565), and users analyzing
Java (including Spring/JPA annotations, records, enums, annotation types,
nested types, and interfaces), receive one complete disposition: real methods
and constructors those patterns currently lose, invent, or misname are reported
with correct names and boundaries when the contribution remains valid on
current trunk, or the PR stays unmerged with an evidence-backed reason and any
required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/565. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
Java users whose methods vanish or are misnamed after multi-annotation classes,
in-method annotations with arguments, explicit record constructors, annotation
type defaults, enum constant bodies, local records, nested types, or interface
methods with bodies.

**Scope.** This pull request only. Compare the claimed Java reader cases on
current trunk through `lizard_languages` Java support and the PR's regression
tests. Correct remaining defects on a reachable contributor branch, preserving
contributor history. Qualify the final candidate with the full suite, style
checks, and applicable performance screening for per-token/parser changes.
Merge only when those gates and required hosted checks hold; otherwise record
the precise unmet gate.

Deferred promises: unrelated languages, other PRs, and broad reader refactors
beyond the named miss/misname cases. Rejection constraints: do not merge an
inadequate, obsolete, or conflicted candidate to finish the cycle; do not
weaken discriminating Java expectations to disguise a failure; do not accept a
material runtime regression without an explicit recorded decision.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given `@Entity @Table(name = "vets") class Vet { ... }`, methods inside the
  class are reported rather than lost after the second annotation's arguments.
- Given a method body with `@SuppressWarnings("unchecked") List<String> x = ...;`
  followed by later methods, those later methods remain reported.
- Given `record R(int x) { this(x, 0); ... }` with an explicit constructor,
  subsequent methods remain reported and the constructor is counted.
- Given `@interface M { String value() default "m"; }`, `value` is not a fake
  function that swallows the rest of the file.
- Given `enum Op { PLUS("+") { int apply() {...} }; }`, `PLUS` is not a
  function and `apply` is reported.
- Given a local `record` inside a method, and after nested types or interface
  methods with bodies, names and later outer methods stay correct.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `41c3ed6cd32f9d1006fbc103387dbcd3821ddf4f` on
`AlexSimionGeorge:fix/java-multi-annotation-class`; base
`8f0c650bd9dadafcb8e843e401401f765bc4e95c`; `maintainer_can_modify` true;
merge state MERGEABLE/CLEAN; evidence under `.planning/evidence/pr-565/`.

**Outcome.** Merged. Final contributor head
`41c3ed6cd32f9d1006fbc103387dbcd3821ddf4f` landed as merge commit
`b676d6f224491aa45578f8493e6aee38c340e6ff`. Acknowledgement:
https://github.com/terryyin/lizard/pull/565#issuecomment-6768581516.
