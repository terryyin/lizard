---
id: SEED-010
status: active
planted: 2026-10-27
planted_during: Maintainer request to review FabioMalpezzi contributions
trigger_when: Complete a bounded set of contribution dispositions
scope: large
---

# SEED-010: FabioMalpezzi contribution review batches

## Parent problem and human decisions

For analyzer users and maintainers, 19 unreviewed contributions should become
worthwhile verified fixes or evidence-backed conclusions. The owner requested
on 2026-10-27 that the completed focus be wrapped/landed and the remaining work
be split into a small number of later stories. Each story owns complete cycles;
correction, performance, refactor, CI and disposition are never horizontal phases
passed to another story. The later work is not urgent and must not start now.

Three stories preserve all original scope: eight completed dispositions, seven
structural reviews and four lexical/condition reviews. Structural errors can lose whole functions
or their metrics, so that batch precedes lexical policy review. This ordering is
not a technical prerequisite. The first completed batch retains its stable identity.
The strongest smaller alternative (one later 11-PR story) leaves a broad mixed
review; collapsing independent PR loops into fewer slices would hide gates.
No PR is dropped, and later batches remain useful if siblings are deferred.

Full policy: [complete-cycle contract](../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md).
The [frozen set and mapping](../../docs/reviews/fabio-malpezzi-20261027/story-map.md)
preserve original heads/slices. The completed first batch is recoverable at 625971ab in Git.
Scope removal and QA handoff were rejected. No S/M/L band definitions are recorded
in this project; completed work uses actual outcomes, while later effort hypotheses
remain unestimated until refinement. No numeric sizing policy is invented.

<a id="review-lexical-js-prs"></a>

### Conclude the JavaScript and TypeScript lexical and condition PR reviews

**Identity:** SEED-010#review-lexical-js-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/007-lexical-js-pr-reviews/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"666f1903382f7f56e736fb6e226e60af04eb0428f73a3f443b8a664b1476e616","plan":"da03a879282b5dc28eedc87d0d8213b0429d33cd36f523d87c81b7490694c831"}}
```

**Beneficiaries:** JavaScript/TypeScript/JSX/TSX users who rely on accurate
comment termination and condition counts; maintainers disposing the remaining
FabioMalpezzi lexical/condition contributions.

**Goal:** Give each of frozen PRs #544–#547 a complete, independently useful
disposition — merge a worthwhile, fully verified correction with acknowledgement,
or leave the PR unmerged with an evidence-backed unnecessary/inadequate reason
and any >15-minute follow-up linked — so comment and condition metrics match
the language rules those PRs claim.

**Current target evidence (preparation, this checkout):** On current `master`,
public analysis still matches the contributor baselines for the four claims:
`b` CCN 3 / NLOC 7 after a `// \` line; `label` CCN 1 for conditions inside a
template interpolation; `load` CCN 2 for `p.catch(...)`; `pick` CCN 1 for
`x?y:0`; and TSX method `m` CCN 3 when optional parameters/fields are present.
Frozen heads still match the open PR heads. Claims remain to verify against
each refreshed head and the complete-cycle contract; they are not automatic
acceptance.

**Scope:** Exactly PRs #544, #545, #546, and #547 from
[frozen-review-set.json](../../docs/reviews/fabio-malpezzi-20261027/frozen-review-set.json)
and [story-map.md](../../docs/reviews/fabio-malpezzi-20261027/story-map.md).
Refresh each selected PR's state/head and the current target before acting.
Later arrivals and other authors are out of scope. Each PR owns its entire
[complete-cycle contract](../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md):
reproduction and value; whole-concept and shared-language consumer review;
eligible ≤15-minute correction (review/measurement time excluded); fresh
independent refactor; affected and broad incremental/cumulative performance
strictly below 3%; full suite, format/hooks, and required CI; exact-head merge
and thanks — or an evidenced unnecessary/inadequate conclusion with a linked
canonical follow-up when correction exceeds 15 minutes. No correction,
performance, refactor, CI, merge, or acknowledgement phase is deferred to a
sibling story.

Deferred promises: structural PRs #537–#543; nesting-extension (-ENS) counting
of keyword member names; a residual where a keyword member name inside a
template interpolation may still count after both #545 and #546 land (the
contributor named a separate follow-up); C/C++ line-comment backslash
continuation; optional chaining and nullish coalescing token shapes beyond the
#547 discrimination; new languages or analyzer options. No supplier agreement
with the structural sibling is justified: lexical dispositions remain useful if
structural work waits. Whichever remaining batch finishes last also reconciles
the full 19-PR cohort and combined evidence; each merge still owns its own
pre-merge combined performance validation.

**Key examples:**

- Given `function b` whose `return` chains `||` comparisons with a `// \` line
  comment between them, analyzing `.js` / `.ts` / `.jsx` / `.tsx` reports
  CCN 4 and NLOC 8 for `b` (the next line is not swallowed). Equivalent C/C++
  line-comment continuation after `\` remains unchanged when shared comment
  machinery is touched.
- Given `function label` returning a template whose interpolations contain
  `x ? 1 : 2` and `x && y`, analyzing the four extensions reports CCN 3 while
  function boundaries, token counts to the states, nested templates, literal
  text, strings, comments, and regex inside the template stay correct.
- Given `function load` returning `p.then(parse).catch(() => null)`, analyzing
  the four extensions reports CCN 1 for `load`; genuine `catch` / `if` / `for` /
  `while` / `case` control-flow constructs still increment CCN.
- Given `function pick` returning the unspaced ternary `x?y:0`, analyzing `.js`
  and `.ts` reports CCN 2. Given a `.tsx`/`.jsx` class with optional field
  `name?: string` and method parameters `a?: number, b?: string`, analyzing
  reports CCN 1 for `m` (optional markers are not ternaries). Optional chaining
  `?.` and nullish coalescing `??` keep their existing non-ternary behavior.

**Effort hypothesis:** Four complete review loops; no project S/M/L band is
invented — size during planning from the contract gates.
**Depends on:** none (structural sibling is not a prerequisite).
**Safe stop:** Any finished subset of these four dispositions retains value if
later PRs in this story or the structural sibling wait.
**Mapped plan:** [007 lexical review](../slice-plans/007-lexical-js-pr-reviews/PLAN.md).
