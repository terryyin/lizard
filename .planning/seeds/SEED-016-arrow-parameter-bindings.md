---
id: SEED-016
status: active
planted: 2026-10-27
planted_during: FabioMalpezzi PR review, PR536 disposition
trigger_when: Correct anonymous arrow parameter capture without duplicating signature rules
scope: medium
---

# SEED-016: Arrow parameter bindings

<a id="arrow-parameter-bindings"></a>

### Report consistent JavaScript and TypeScript arrow parameters

**Identity:** SEED-016#arrow-parameter-bindings

**Beneficiary:** JS/TS, JSX/TSX and Vue users relying on parameter counts, names,
threshold warnings and duplicate-parameter analysis for callbacks and arrows.

**Goal:** Recognized anonymous and named arrow signatures expose consistent
formal parameter counts and binding names. Types and initializer expressions
retain their own meaning and CCN, and empty binding patterns still occupy slots.

**Origin:** [PR #536](https://github.com/terryyin/lizard/pull/536), frozen open
head `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`, parent `01afe9c`, reviewed
against `d7be8b90ac5872faf86e329e7d7249c98029f045`. Complete correction plus
cleanup was estimated **30–45 minutes before product edits**, above SEED-010's
fifteen-minute limit. No production correction was attempted. Review and
measurement are excluded from that limit.

**Reproducible expected and actual behavior:**

```typescript
const total = list.reduce((sum, item) => sum + item.price, 0);
const names = list.map(item => item.name);
const pick = flag ? (a, b) => a : null;
const sizes = list.map((x: Item, i: number): number => x.size * i);
```

Through `analyze_file.analyze_source_code("a.ts", source)`, current target reports
four anonymous functions with counts `0,0,0,0`. Expected `2,1,2,2`, names
`[sum,item]`, `[item]`, `[a,b]`, `[x,i]`. Raw PR536 and its clean current-target
adaptation provide this useful improvement and preserve accepted PR532/535 cases.

```javascript
const named = ({key: renamed}, tail) => { return tail; };
run(({key: renamed}, tail) => { return tail; });
```

Both signatures require count2 and the established public names
`['renamed','tail']`. Current named-arrow extraction follows that convention;
the proposal's independent collector returns `['key','tail']` for the anonymous
arrow. Compound patterns similarly return property/head names instead of the
named convention `['other','tail','last']`. This affects duplicate-list identities.

```javascript
run(({}, [], tail) => { return tail; });
const named = ({}, [], tail) => { return tail; };
```

Both have three formal parameter slots, despite two bindings introducing no
identifier. The proposal still reports only one. The named form's missing slots
are pre-existing and must be resolved for a coherent shared representation.

```typescript
run((number: number, $: Map<string, number>,
     cb: (value: number, index: number) => boolean,
     tail?: string): number => { return number; });
```

Expected one anonymous callback, count4, names `['number','$','cb','tail']`.
Callback type annotations currently create phantom functions; this pre-existing
problem is necessary remaining work for complete anonymous signature handling.

**Remaining work:**

- Reuse existing JavaScriptBindings, JavaScriptFunctions and FunctionInfo owners
  for named and anonymous signatures, instead of an independent pending-name
  collector. Preserve the public facade and accepted identifier/receiver behavior.
- Represent formal slots, including empty patterns, without counting bound
  identifiers as separate arguments or inventing a parallel parser-local count.
  Settle the public representation of nameless slots during refinement.
- Preserve aliases, nested/quoted/computed patterns, rest/defaults, generic and
  callback annotations, typed returns, initializer CCN and following functions.
  Add discriminating public analyzer/CLI and affected-consumer regressions;
  keep all accepted controls and the existing curried-arrow convention.
- Inspect the current website JSON/React parameter-count consumer alongside
  CLI, formatters, warnings and duplicate lists. Current trunk53e4850 changes
  website/runtime/CI setup but leaves all analyzer product sources unchanged.
- Eliminate unnecessary collector bookkeeping and measure the complete corrected
  candidate. Parent→raw API median slowdown4.92%, 95% interval[3.92%,5.43%];
  current-target→adapted API6.59%, interval[4.27%,8.58%]; names/counts7.64%,
  interval[6.76%,8.43%]. These fail the strict3% gate despite passing broad CLI.
  An unchanged-output five-input profile finds60000 extra `_read_name` calls;
  this supports one avoidable contributor, not an exact share of total cost.
- Independently refactor and verify the final candidate, focused/full tests,
  format checks and current required CI. Require paired95% upper bounds strictly
  below3% for corrected affected/broad incremental and cumulative workloads.
  Include all relevant corrected cases in the frozen timing corpus; do not
  repeatedly time the already-invalid proposal to seek a pass.

**Boundaries:** Member-assignment parameter loss (#537) and abandoned-expression
CCN (#539) are separately observed later review slices. No prerequisite or
blocking supplier agreement was established. This story does not authorize
unrelated boundary repairs or parser redesign.

**Evidence and replay:** [PR536 evidence](../../docs/reviews/fabio-malpezzi-20261027/pr536/README.md)
retains65 source/control files, expected/actual names/counts and non-parameter
metrics, exact escaped patches, source hashes, actual four-row CLI proof,
focused results, six paired raw datasets and bounded profile evidence.
Its bootstrap/reproducer commands assert snapshot paths and real CLI input/rows.

**Priority:** Queue after the four existing review corrections, preserving their
order and the maintenance/bug-fixing direction. No Take, executable plan or
implementation is created by this deferral.

**Evaluation:** All examples have the expected formal counts and established
binding names; annotations create no phantom functions; initializer/body metrics
and earlier accepted controls hold; final quality, full checks, CI and strict
performance gates pass. Keep PR536 unmerged pending that satisfactory revision.
