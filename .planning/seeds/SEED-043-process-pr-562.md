---
id: SEED-043
status: active
planted: 2026-11-07
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/562
scope: TypeScript template literal type interpolations in declarations
---

# SEED-043: Process PR #562

<a id="process-pr-562"></a>

### Process PR #562 — Keep TypeScript declarations open across template literal type interpolations

**Identity:** SEED-043#process-pr-562
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal.** TypeScript users who put template literal types with `${...}` inside
interface bodies or object type aliases, and maintainers disposing
[PR #562](https://github.com/terryyin/lizard/pull/562), receive one complete
disposition: the change is landed when those declarations stay open through
the interpolations so later functions keep their own metrics, or the PR stays
unmerged with an evidence-backed reason and any required follow-up linked.

**Source.** `/lizard-process-pr https://github.com/terryyin/lizard/pull/562`.
Full process-PR scope: review, reproduce, correct on the contributor branch
when that route exists, qualify, and merge when the skill's gates hold. This
invocation does not select planless execution or a separate landing of
lifecycle records.

**Beneficiary.** People who analyze TypeScript with template literal types in
declarations (`kind?: Extract<string, \`x-${string}\`>` and similar) and
maintainers deciding whether to accept this contribution.

**Scope.** This pull request only. Reproduce the claimed early-close of the
declaration on current trunk through the public analyzer or CLI, compare it
with the proposal, and check nearby TypeScript declaration controls and shared
consumers of function boundaries and CCN. Correct a worthwhile change only on
a reachable contributor branch, preserving contributor history. Qualify the
final candidate with discriminating tests, the full suite, style checks, and a
performance screening of the declaration-skip path this change touches. Merge
only when those gates and required hosted checks hold; otherwise record the
precise unmet gate.

Deferred promises: other TypeScript syntax, other languages, and pull requests
that are not #562. Rejection constraints: do not merge an inadequate candidate
to finish the cycle; do not apply the Fabio cohort's 15-minute deferral or its
strict 3% bound as this story's policy. The paired runner's default screening
result is evidence to record, including a consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given `interface Options { kind?: Extract<string, \`x-${string}\`>; }`
  followed by `function choose(x: boolean) { if (x) return 1; return 0; }`,
  analysis reports `choose` with CCN 2 and `*global*` with CCN 1, not only
  `*global*` with CCN 2.
- Given the same template literal type as a required field, a generic
  argument, nested generics, a nested interpolation, an `extends` clause, or
  an object type alias body, a following function still keeps its own metrics.
- Given one file that mixes those declaration forms with a runtime template
  interpolation and a class method, CCN and cognitive complexity (with
  `-Eoutside`) match the same code without the type-only declarations.
- Class fields, variable annotations, and standalone aliases
  (``type P = \`x-${string}\`;``) stay correct as they already are on trunk.

Intake metadata: head `8314ee5fe2985068c63e462682ca546ad9ce4c81` on
`cpheinrich:codex/ts-template-literal-type-braces`; PR base
`4f6979da10faabc72f1564f9341588afbb2924f0`; `maintainer_can_modify` true;
merge state at intake was conflicting with current trunk.

**Outcome.** Merged as `d9b431634ae273832560d6bf86451a735dfe421f` with
contributor head `684039ad072d1a87139500edd9377b42406fb59e` (original
`8314ee5fe2985068c63e462682ca546ad9ce4c81` plus master merges for CHANGELOG).
On current trunk, template literal types with `${...}` in interface bodies or
object type aliases ended the declaration early, so later functions were
missing; the merged skippers count `${` as an opener and restore those
functions. Extends clauses, class fields, variable annotations, and standalone
aliases stay correct. Hosted builds on `684039ad` passed; `web` was skipped.
Acknowledgement: https://github.com/terryyin/lizard/pull/562#issuecomment-6699493094

Paired screening on an equivalent TypeScript/JavaScript corpus without the
defective early-close pattern: API about -0.5% to +0.3%, API with parameters
about -0.3% to +0.8%, CLI about -1.0% to +1.0%. A 400-interface corpus that
exercises the bug has different observation hashes and more real work on the
fixed side; that is not same-work slowdown.
