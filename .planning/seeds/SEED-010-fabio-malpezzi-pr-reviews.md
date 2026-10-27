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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Beneficiaries:** JS-family users relying on detected functions, parameter counts,
boundaries and CCN; maintainers evaluating the remaining structural contributions.
**Goal:** Give each of PR537–543 a complete, independently useful disposition,
landing worthwhile fully verified changes and acknowledging the contributor.
**Scope:** Exactly PR537,538,539,540,541,542,543 from the frozen set; later arrivals
and other authors are excluded. Refresh current heads/target and verify the claims.
Use the full complete-cycle contract: reproduction/value; whole-concept correctness
and consumers; eligible correction; fresh independent refactor; affected/broad
incremental/cumulative <3% performance; full/format/required CI; guarded merge and
thanks, or an evidence-backed unnecessary/inadequate conclusion with follow-up
under the existing >15min policy. No acceptance phase is left to another story.

**Key examples to re-establish:**

| PR | Reported observable result to verify |
| --- | --- |
|537|Member-assignment arrows expose two actual parameters, while real calls remain calls.|
|538|An IIFE keeps setup's following statements and method boundaries/CCN.|
|539|Abandoned provisional signatures retain enclosing expression tokens and CCN.|
|540|Switch labels allow callbacks without treating label colons as types.|
|541|Members named get/set coexist with genuine accessor prefixes and following functions.|
|542|Array property values and computed names preserve function end boundaries.|
|543|Spreads of calls/grouped expressions retain callbacks without fictitious methods.|

Examples are claims, not automatic acceptance. Already-correct or unworthy changes
receive reasons; coherent corrections above 15 minutes receive linked canonical stories
and stay unmerged. Accepted PRs 527/532/535 and later current-target changes are controls.
**Effort hypothesis:** Seven complete review loops, unestimated pending refinement;
the correction limit excludes review/measurement. **Depends on:** No assumed sibling.
**Safe stop:** Its seven conclusions/fixes stand independently if lexical work waits.
**Mapped plan:** [006 structural review](../slice-plans/006-structural-js-pr-reviews/PLAN.md).
**Resume:** Run dough-story-refinement, reusing and realigning that mapped plan;
it is awaiting story refinement, not ready for slice-plan refinement or execution.


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
