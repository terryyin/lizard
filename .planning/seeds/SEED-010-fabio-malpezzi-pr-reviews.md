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

<a id="review-structural-js-prs"></a>

### Conclude the JavaScript and TypeScript structural PR reviews

**Identity:** SEED-010#review-structural-js-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/006-structural-js-pr-reviews/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"d1447236708b4abec06acbed6f9ca002adef5e36a51faee372857b694e54a7b1","plan":"e11499a78823db7e64a6bfacc5bf2a8a9d3ddcc56cf981709a962906a0e08fe1"}}
```

**Goal:** For JS-family analyzer users who depend on function detection, parameter
counts, boundaries and CCN, and for maintainers disposing contributor structural
fixes, each of frozen PRs #537–#543 receives one complete, independently useful
disposition: a worthwhile change verified and landed with acknowledgement, or an
evidence-backed unnecessary / inadequate conclusion with any required >15‑minute
follow-up linked and left unmerged.

**Scope:** Exactly PRs #537, #538, #539, #540, #541, #542, #543 from the
[frozen set](../../docs/reviews/fabio-malpezzi-20261027/frozen-review-set.json)
and [story map](../../docs/reviews/fabio-malpezzi-20261027/story-map.md). At
refinement (2026-10-28) those heads remain open and match the frozen OIDs; refresh
each selected PR's state/head and the current target again before acting.
Later arrivals and other authors are out of scope.

Every selected PR owns the full
[complete-cycle contract](../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md):
reproduce and judge value on the current target; review the whole affected concept
and shared-language consumers; correct only when eligible within the 15‑minute
policy (review/measurement excluded from the limit); prove discriminating public
API/CLI regressions; run fresh independent post-change refactor; pass affected and
broad incremental/cumulative performance strictly below 3%; pass full/format/
required CI; merge guarded by exact head with post-merge thanks — or record an
unnecessary / inadequate conclusion with linked follow-up. No correction,
performance, refactor, CI, merge or acknowledgement phase is deferred to another
story.

Deferred promises: PRs #544–#547 (lexical/condition batch), new PR arrivals, and
unrelated language readers. Rejection constraints: do not invent an S/M/L sizing
policy; do not merge an inadequate candidate to “finish” a cycle; do not treat
sibling queue order as a supplier dependency without a justified agreement.
Assumptions: structural-before-lexical ordering is impact priority, not a
technical prerequisite; completed first-batch merges (#527/#532/#535 and later
current-target fixes including #534/#536 corrections) are controls, not this
story's remaining work.

**Key examples:**

- Given a member assignment whose right-hand side is an arrow with two parameters
  (PR #537 claim), public analysis reports those two parameters on the arrow
  function while ordinary member calls remain calls, not parameter lists.
- Given a function whose body starts with an IIFE then continues with statements
  and methods (PR #538 claim), analysis keeps the outer function's end line and
  CCN together with the following method, instead of ending the outer function at
  the IIFE.
- Given an enclosing expression that abandons a provisional arrow or method
  signature (PR #539 claim), tokens and CCN stay on the enclosing expression;
  real arrows still own their defaults and body conditions.
- Given `switch` `case`/`default` labels that contain callbacks (PR #540 claim),
  those callbacks are reported as functions; label colons are not consumed as
  TypeScript type annotations, while genuine annotations and ternaries stay
  distinct.
- Given object members literally named `get`/`set` alongside real accessors
  (PR #541 claim), the named members and a following function are all present
  with correct boundaries.
- Given object properties whose values are nested arrays, and bracketed computed
  keys (PR #542 claim), each function keeps its own end boundary and following
  functions remain visible.
- Given object spreads of calls or grouped expressions that carry callbacks
  (PR #543 claim), nested callbacks are retained and no fictitious object method
  is invented from the spread.

Examples are claims to verify, not automatic acceptance. Already-correct trunk,
unworthy changes, or corrections estimated above 15 minutes receive a recorded
reason or linked follow-up instead of a merge.

**Effort hypothesis:** Seven complete review loops; unestimated — this project
records no S/M/L band policy; the correction time limit excludes review and
measurement. **Depends on:** none assumed; verify any genuine technical
dependency during planning/execution and record only a justified supplier
agreement. **Safe stop:** Its seven dispositions retain value if the lexical
batch waits. **Mapped plan:**
[006 structural review](../slice-plans/006-structural-js-pr-reviews/PLAN.md).


<a id="review-lexical-js-prs"></a>

### Conclude the JavaScript and TypeScript lexical and condition PR reviews

**Identity:** SEED-010#review-lexical-js-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Beneficiaries:** JS-family users relying on comment/template interpretation and
condition counts; maintainers evaluating these remaining contributions.
**Goal:** Give each of PR544–547 a complete disposition with worthwhile verified
changes landed and acknowledged, or a recorded reason for leaving it unmerged.
**Scope:** Exactly PR544,545,546,547 from the frozen set. Refresh head/target;
later arrivals and other authors are excluded. Every PR owns its entire cycle:
reproduction/value, whole-concept/shared-language review, eligible correction,
fresh independent refactor, affected/broad incremental/cumulative <3% performance,
full/format/required CI, exact-head merge and thanks, or an evidenced unnecessary/
inadequate conclusion and linked >15min follow-up. Do not defer QA to a sibling.

**Key examples to re-establish:**

| PR | Reported observable result to verify |
| --- | --- |
|544|A JS line-comment backslash does not hide the next line; preserve C/C++ continuation rules.|
|545|Template interpolation conditions count while literals/nesting/regex and boundaries remain correct.|
|546|Keyword member names add no conditions; genuine control flow still does.|
|547|Unspaced ternaries count; TypeScript optional markers do not, with chaining/nullish controls preserved.|

Claims may prove invalid/already fixed or need >15min corrections; retain a final
reason/follow-up instead of merging an inadequate candidate. No pending correction,
performance, refactor, CI or acknowledgement is passed to another review story.
**Effort hypothesis:** Four complete review loops, unestimated pending refinement.
**Depends on:** No assumed structural-story prerequisite; verify genuine dependency.
**Safe stop:** These conclusions/fixes retain value even if structural work waits.
Whichever remaining batch actually finishes last also reconciles the full 19-PR
cohort and final combined evidence; each individual merge validates combinations
beforehand, never postponing its own performance gate to that final summary.
**Mapped plan:** [007 lexical review](../slice-plans/007-lexical-js-pr-reviews/PLAN.md).
**Resume:** Run dough-story-refinement, reusing/realigning the mapped plan; it is
awaiting story refinement, not ready for slice-plan refinement or execution.
