---
id: SEED-048
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: TypeScript and TSX function boundaries
---

# SEED-048: Process PR #519

<a id="process-pr-519"></a>

### Process PR #519 — Fix TypeScript and TSX function boundaries

**Identity:** SEED-048#process-pr-519

**Goal.** TypeScript and TSX users who rely on Lizard function spans for
complexity limits, and maintainers disposing
[PR #519](https://github.com/terryyin/lizard/pull/519), receive one complete
disposition: the change is landed when return types, optional calls, JSX tags,
expression-bodied arrows, parenthesized expressions, and JSX comparisons no
longer end, merge, or lose functions incorrectly while nearby controls stay
correct, or the PR stays unmerged with an evidence-backed reason and any
required follow-up linked.

**Source.** Dashboard launch instruction
`lizard process pr https://github.com/terryyin/lizard/pull/519`. Full
process-PR scope: review, reproduce, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** TypeScript and TSX users whose functions currently end too
early, too late, or disappear, and maintainers deciding whether to accept this
contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby TypeScript/TSX/JavaScript controls and shared consumers of the
code-reader callback and JSX tag paths. Correct a worthwhile change only on a
reachable contributor branch, preserving contributor history. Qualify the final
candidate with discriminating tests, the full suite, style checks, and a
performance screening of the TypeScript/TSX path this change touches. Merge only
when those gates and required hosted checks hold; otherwise record the precise
unmet gate.

Deferred promises: multi-line expression bodies, IIFE-as-object-value endings,
other languages, and pull requests that are not #519. Rejection constraints: do
not merge an inadequate candidate to finish the cycle; do not apply the Fabio
cohort's 15-minute deferral or its strict 3% bound as this story's policy. The
paired runner's default screening result is evidence to record, including a
consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given a function with a simple return type such as `): void {`, the function
  ends at its closing brace rather than at the next statement.
- Given optional calls or indexes such as `f?.(x)` and `a?.[i]`, the opening
  bracket is not swallowed and the enclosing function keeps its correct end.
- Given JSX tags with hyphenated names, valueless attributes, spreads, member
  names with attributes, or type arguments, the component does not run to the
  end of the file or lose a brace.
- Given attribute expressions inside multi-line tags, function start lines are
  not shifted early and an assignment such as
  `c = (<D f={(v) => g(v)} />)` is not misread as an arrow named `c`.
- Given expression-bodied arrows that start on the line after `=>`, and arrows
  inside objects or argument lists, start and end lines match the arrow body
  and its terminating comma.
- Given parenthesized IIFEs or JSX such as `x = (function () {…})()` and
  `c = (<div>…</div>)`, they are not read as parameter lists of an arrow named
  `x` or `c`.
- Given a comparison in a JSX expression such as `{i < n}` after a tag, `<`
  does not open unclosed type parameters that swallow the rest of the file.

Intake metadata: head `3b039e3d1288e225cca3b08f538a3a19e9a6315d` on
`AdlerView:ts-function-boundaries`; PR base
`632869c84bbf6f0fa2aaf72ab64468baf86442aa`; `maintainer_can_modify` true;
merge state CONFLICTING/DIRTY; evidence packet under `.planning/evidence/pr-519/`.

**Outcome.** Merged as `f67008d965fc333a2ab85fc502373545494813bb` with
contributor head `a41f90716e99758b388954eb0c99e7b73b4d5b35` after merging
current master into the PR and porting the remaining boundary fixes onto
today's TypeScript/TSX architecture (member tags, type-argument elements,
JSX comparisons via non-tag `<` and generic-consume bail-out, multiline tag
newlines, flush-before-attribute-expression, arrow start lines, and
parenthesized JSX abandon). Codex P1/P2 regressions (expression-body commas,
attribute comparison spans, optional-index callbacks) stay correct on the
ported tree. Return-type and optional-call ends were already correct on trunk.
Local `pytest` / `make` were green (3089 passed, 7 skipped). Hosted
`Python application` on the final head was success. Acknowledgement:
https://github.com/terryyin/lizard/pull/519#issuecomment-6702301118

Paired screening versus trunk on a frozen 1200-file TS/TSX fixture corpus,
20 pairs, batches calibrated to 4 seconds: API paired change about +3.7% to
+4.7% (bound-not-met under the runner's default 3% gate); CLI median slightly
faster with a wide inconclusive interval. Retained under
`.planning/evidence/pr-519/perf/`. This story records that screening and does
not treat the runner's default 3% bound as a merge gate.
