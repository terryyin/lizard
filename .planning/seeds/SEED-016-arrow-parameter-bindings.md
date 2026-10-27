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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/012-arrow-parameter-bindings/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"e3317d2e463627144194d030e0b65dac8cd5d42736d4284a55f204d1d45b4e48","plan":"83f259fed3a0883470982441211fa4b4297ed8b82d7cbdaf610c2fca6f022b23"}}
```

**Beneficiary:** JavaScript, TypeScript, JSX/TSX and Vue users relying on
Lizard's parameter counts, binding names, threshold warnings and
duplicate-parameter analysis for callbacks and arrows.

**Goal:** Recognized anonymous and named arrow signatures expose the same
formal parameter counts and established binding names. Type and initializer
expressions keep their own meaning and CCN; empty binding patterns still
occupy formal slots without inventing public names; callback-type annotations
create no phantom functions.

**Source:** [PR #536](https://github.com/terryyin/lizard/pull/536), frozen open
head `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`, parent `01afe9c`, reviewed
against `d7be8b90ac5872faf86e329e7d7249c98029f045`. The report is real and
useful. Raw and current-target-adapted proposals improve the reported
anonymous counts but keep a second name collector beside existing owners,
mismatch named binding aliases, under-count empty patterns, leave typed
callback phantoms, and fail the strict performance gates. Complete correction
plus cleanup was estimated 30–45 minutes before product edits, above
SEED-010's fifteen-minute limit. No production correction was attempted;
PR #536 remains unmerged. Naming and metric meanings follow
[ADR-0001](../../docs/adrs/0001-ubiquitous-language-accepted.md); the facade
and existing reader owners stay unchanged.

**Key examples:**

These situations define required public behavior through
`analyze_file.analyze_source_code` (and the equivalent CLI / FileAnalyzer),
not an exhaustive grammar. The frozen
[expected rows](../../docs/reviews/fabio-malpezzi-20261027/pr536/inputs.json)
and accepted PR532/535 controls in
[all-inputs.json](../../docs/reviews/fabio-malpezzi-20261027/pr536/all-inputs.json)
give explicit names and counts; the
[reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py)
checks them. Function names follow the existing binding convention.

```typescript
const total = list.reduce((sum, item) => sum + item.price, 0);
const names = list.map(item => item.name);
const pick = flag ? (a, b) => a : null;
const sizes = list.map((x: Item, i: number): number => x.size * i);
```

Pre-condition: four anonymous arrows (parenthesized, bare, ternary branch,
typed). Trigger: analyze as TypeScript. Result: four anonymous functions with
counts `2,1,2,2` and names `[sum,item]`, `[item]`, `[a,b]`, `[x,i]`. Current
target reports `0,0,0,0`. The same counts hold for the authored JS/TS/TSX
controls, JSX attribute arrows, Vue script blocks and `.cjs`/`.mjs` readers.

```javascript
const named = ({key: renamed}, tail) => { return tail; };
run(({key: renamed}, tail) => { return tail; });
```

Pre-condition: a destructuring alias in both a named and an anonymous arrow.
Trigger: analyze the file. Result: both have count 2 and public names
`['renamed','tail']` (the established named-arrow convention; not property
key `key`). Compound, nested, quoted and computed patterns follow the same
rule (`['other','tail','last']`, nested/quoted/computed aliases). The
proposal's independent collector returns property/head names for anonymous
forms and breaks duplicate-list identities.

```javascript
run(({}, [], tail) => { return tail; });
const named = ({}, [], tail) => { return tail; };
```

Pre-condition: empty object and array binding patterns among formal
parameters. Trigger: analyze the file. Result: both signatures have
`parameter_count` 3. Public `parameters` lists only established binding names
(`['tail']` here); nameless empty patterns occupy slots without inventing
names, so `parameter_count` may exceed `len(parameters)`. Trailing-comma empty
tokens remain excluded as today. The proposal still reports one; the named
form's missing slots are pre-existing and must be resolved for a coherent
shared representation.

```typescript
run((number: number, $: Map<string, number>,
     cb: (value: number, index: number) => boolean,
     tail?: string): number => { return number; });
```

Pre-condition: an anonymous typed arrow whose annotation includes a callback
function type. Trigger: analyze as TypeScript (and TSX). Result: one anonymous
callback, count 4, names `['number','$','cb','tail']`; no phantom
zero-parameter function from the callback annotation. Generic parameters,
typed returns, rest/defaults, nested default CCN, ternary-default CCN
ownership and following functions stay correct.

**Settled public representation:** `parameter_count` is the number of formal
parameter slots (ADR-0001; ECMAScript formal-parameter grammar). `parameters`
is the list of established binding names for slots that introduce identifiers
(aliases, rest heads, plain ids — matching named-arrow / PR532 convention).
Empty patterns contribute a slot to the count and no public name; they invent
neither duplicate-list keys nor placeholder identifiers. Prefer extending the
shared `FunctionInfo` / binding-owner representation over a parser-local
parallel count; do not weaken trailing-comma exclusion.

**Scope:**

- Required behavior: reuse existing `JavaScriptBindings`,
  `JavaScriptFunctions` / `JavaScriptFunctionsMixin`, `TypeScriptStates` and
  `FunctionInfo` owners for named and anonymous signatures; no independent
  pending-name collector. Preserve the public facade and accepted
  identifier/receiver behavior from PR532/535. Capture anonymous arrow
  formals with the same counts and binding names as named arrows across
  JavaScript, TypeScript, JSX/TSX and Vue script (`.cjs`/`.mjs` reuse the
  reader). Represent empty-pattern slots as settled above. Skip callback-type
  annotation arrows so they create no phantoms. Preserve aliases,
  nested/quoted/computed patterns, rest/defaults, generic and callback
  annotations, typed returns, initializer/default CCN and following
  functions. Keep the existing outer-only curried-arrow convention.
- Proof: discriminating public-analysis tests for the promised frozen rows
  (`arrow-reported`, `arrow-authored*`, `arrow-named`, `arrow-defaults`,
  `arrow-binding-*`, `arrow-alias-*`, nested/quoted/computed bindings,
  `arrow-empty-binding*`, `arrow-nested-default`, `arrow-typed`,
  `arrow-generic*`, `arrow-return-*`, `arrow-sigils*`, `arrow-calls`,
  `arrow-ternary-default`, `arrow-jsx`/`arrow-tsx`, `arrow-control`, and the
  wrapper-call half of `arrow-member`) plus the 34 accepted PR532/535
  controls, across CLI, formatters, warnings, duplicate-parameter lists and
  the current website JSON/`parameter_count` consumer path. Keep new tests in
  a small cohesive module below 250 lines; leave unrelated large test files
  byte-identical. Remeasure the corrected candidate on the frozen affected
  corpus (include all relevant corrected cases) plus the broader fixed-source
  corpus against current target and the review baseline, with paired 95%
  upper slowdown bounds strictly below 3% for incremental and cumulative
  affected and broader workloads. Do not repeatedly time the already-invalid
  proposal. Run the full pytest suite, `make` and final hosted CI before any
  exact-head guarded merge.
- Rejection constraints: the strict below-3% performance gate, consistent
  named/anonymous binding names, formal empty-pattern slots, no callback
  phantoms, and preserved accepted 532/535 controls are acceptance
  constraints from the retained review, not optional polish. Examples do not
  forbid other valid forms the shared readers already support.
- Deferred promises: member-assignment parameter loss
  (`module.exports.add = (a, b) => …` remaining count 0) stays with later
  review slice #537; this story promises the wrapper-call callback improvement
  (`api.wrap((x, y) => …)` → count 2) only. Abandoned-expression enclosing
  CCN (`arrow-abandoned.js` expecting outer CCN 3) stays with #539. No
  prerequisite or blocking supplier agreement is asserted with SEED-014 or
  SEED-015. No unrelated boundary repairs, parser redesign, ENS-only
  performance claim, full JS/TS grammar, merge, contributor-head write or
  acknowledgement during preparation.

**Retained evidence:** Parent→raw API median +4.92%, 95% interval
[+3.92%, +5.43%]; current-target→adapted API +6.59%, interval
[+4.27%, +8.58%]; names/counts +7.64%, interval [+6.76%, +8.43%]. Broader CLI
passes do not cancel failed affected gates. An unchanged-output five-input
profile finds 60,000 extra `_read_name` calls, supporting redundant collector
bookkeeping as one avoidable contributor, not an exact share of total cost.
Durable metadata, patches, corpus, logs and commands live in the
[PR536 evidence](../../docs/reviews/fabio-malpezzi-20261027/pr536/README.md).

**Completion:** Promised rows and controls have correct formal counts and
established binding names across the related readers; annotations create no
phantoms; initializer/body metrics and earlier accepted controls hold; final
quality, full checks, CI and strict performance gates pass. Keep PR #536
unmerged until a final corrected revision satisfies all gates.
