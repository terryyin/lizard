---
id: SEED-047
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: ObjC @interface/@protocol not reported as functions
---

# SEED-047: Process PR #490

<a id="process-pr-490"></a>

### Process PR #490 — Do not report an ObjC @interface class extension as a function

**Identity:** SEED-047#process-pr-490
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"e56137f902c4b9e2f99edd1883157eaec8a57468ab0c89dc9357b8f71e9b0e1c"}}
```

**Goal.** ObjC/ObjC++ users who analyze `.m` / `.mm` files with Lizard, and
maintainers disposing [PR #490](https://github.com/terryyin/lizard/pull/490),
receive one complete disposition: the change is landed when an `@interface`
class extension or category with an instance-variable block, and an
`@protocol` declaration section, are not reported as functions while
`@implementation` method bodies remain reported correctly, or the PR stays
unmerged with an evidence-backed reason and any required follow-up linked.

**Source.** Dashboard ad-hoc instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/490. Full process-PR scope: review,
reproduce, correct on the contributor branch when that route exists, qualify,
and merge when the skill's gates hold.

**Beneficiary.** ObjC/ObjC++ users whose sources use class extensions,
categories, or protocols, and maintainers deciding whether to accept this
contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby ObjC controls and shared consumers of the ObjC reader and C-like
function recognition. Correct a worthwhile change only on a reachable
contributor branch, preserving contributor history. Qualify the final
candidate with discriminating tests, the full suite, style checks, and a
performance screening of the ObjC reader path this change touches. Merge only
when those gates and required hosted checks hold; otherwise record the precise
unmet gate.

Deferred promises: other ObjC syntax beyond the reported `@interface` /
`@protocol` skip, other languages, and pull requests that are not #490.
Rejection constraints: do not merge an inadequate candidate to finish the
cycle; do not apply the Fabio cohort's 15-minute deferral or its strict 3%
bound as this story's policy. The paired runner's default screening result is
evidence to record, including a consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given an `@interface OCCFile () { CppFile* mCppFile; } @end` class extension
  before `@implementation` methods, analysis does not report `OCCFile` as a
  function; `init` / `dealloc` (and real methods) remain reported.
- Given `@interface Foo (Private) { int x; } @end`, the category is not
  reported as a function.
- Given `@protocol P - (void)m; @end`, the protocol is not reported as a
  function.
- Given ordinary `@implementation` method bodies and a classic
  `@interface Foo : NSObject { ... }` form that already escapes via `:`, real
  methods stay correct.

Intake metadata: head `241374a4bc75bcc49f5119472cb931918476d228` on
`Eljees:fix/305-objc-interface-is-not-a-function`; PR base
`0b97a9ae8e77b8d559e7ef187ad4b4e3ef02924f`; `maintainer_can_modify` true;
cross-repository; mergeable; evidence packet under `.planning/evidence/pr-490/`.

**Outcome.** Merged as `c054cd309a43d9e4dca4f4fecf513ac105199459` with
contributor head `241374a4bc75bcc49f5119472cb931918476d228` (no local
correction). On trunk before the change, the class-extension sample reported
`OCCFile` plus `init` / `dealloc`; after merge it reports only `init` /
`dealloc`. Category and class-extension ivar blocks report no functions;
protocol definitions stay non-functions; `@protocol Foo;` and
`@protocol(NSObject)` leave following methods/functions intact; classic
`@interface Foo : NSObject` still yields the implementation method. Merge
candidate `pytest` / `make` were green (3075 passed, 7 skipped). Hosted checks
were green 4/4 on the contributor head. Acknowledgement:
https://github.com/terryyin/lizard/pull/490#issuecomment-6699902010

Paired screening versus `origin/master` `66d1ea568` (candidate tip after
cherry-pick onto that base) on a frozen ObjC corpus (480 `.m` files, ~310 KB),
20 pairs, batches calibrated to 4 seconds: API paired change about -2.87%,
95% interval about -3.35% to -2.39%; CLI about -2.94%, interval about -3.61%
to -2.26%. Retained under `.planning/evidence/pr-490/perf/`. This story records
that screening and does not treat the runner's default 3% bound as a merge
gate.
