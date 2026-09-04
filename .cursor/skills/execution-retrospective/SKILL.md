---
name: execution-retrospective
description: >-
  Audit a completed plan execution by reconstructing its original plan and
  related commits, then reviewing the aggregate change for bugs, story drift,
  missed refactoring smells, and worthwhile improvements. Use even when cleanup
  deleted the PLAN or the user gives only a partial reference. Create a
  follow-up slice PLAN when meaningful repository work remains, but never
  execute it.
---

<objective>
Produce an evidence-backed retrospective of one completed plan execution.
Recover the original story and exact execution commit set, review their combined
effect, and create a follow-up PLAN only when filtered repository findings need
work.

When the current conversation contains the just-finished execution, also review
the thread for process improvements and overlooked developer decisions. Do not
implement findings, edit rules or skills, commit, push, or start plan execution.
</objective>

<context>
Read `.cursor/agent-map.md`, `.cursor/rules/problem-decomposition.mdc`, and
`.cursor/rules/planning.mdc`. Read **post-change-refactor** in full and reuse its
smell definitions against the aggregate execution diff; do not run its editing
workflow. Read **slice-planning** before creating any follow-up PLAN.

The PLAN may have been removed by normal completed-plan cleanup. A partial
capability name, surviving conversation context, old path, commit message, or
distinct story phrase is sufficient input for discovery. Do not require the
user to supply a deleted file's exact name.

The review is read-only except for a follow-up PLAN created through
**slice-planning**. Preserve all existing working-tree changes. Do not create a
separate retrospective artifact unless the user asks.
</context>

<process>

<step name="resolve_the_original_plan">
Resolve one plan using evidence in this order:

1. Current conversation and execution transcript.
2. Current `.planning/phases/` and `.planning/quick/` contents.
3. Git history for renamed or deleted planning paths.
4. Commit messages and diffs containing distinctive story language.

Useful history operations include:

```bash
git log --all --name-status -- .planning/phases .planning/quick
git log --all --diff-filter=D --summary -- .planning/phases .planning/quick
git log --all --grep='<capability phrase>'
git log --all -S'<distinct story phrase>' -- .planning
git show <revision>:<historical-plan-path>
```

Recover the earliest execution-ready plan revision, its source seed/story when
present, beneficiary, intended outcome, boundaries, and outside-in proof. Also
record later plan changes that were explicitly approved or supported by new
evidence; do not misclassify them as drift.

If two plans remain equally plausible after history inspection, stop and ask the
developer to choose. Do not combine them.
</step>

<step name="build_the_execution_commit_set">
Identify every related execution commit, including final cleanup when it removed
the PLAN. Include a commit only with evidence such as:

- it updates the PLAN's slice status or cleanup;
- its message names the story, capability, or slice;
- its diff implements, proves, refactors, or documents the recovered story; or
- the execution thread explicitly associates it with the plan.

Record each included SHA and reason. Inspect intervening commits and exclude
unrelated work; do not assume one contiguous range belongs to the plan.

Review all selected commit diffs together. When they are contiguous and contain
no unrelated work, use the net diff from the parent of the first implementation
commit through the last related commit. Otherwise review the selected patches
together and inspect their changed files at the last related implementation
commit. Do not let later commits or current uncommitted work contaminate the
historical result, and never mutate the current worktree to reconstruct it. Keep
planning-only changes as provenance, not as product-quality findings.
</step>

<step name="review_the_combined_outcome">
Compare the original story contract, approved changes, aggregate diff, and final
code/tests/docs. Report only concrete findings with evidence and impact:

1. **Bugs** — incorrect behavior, regression, unsafe edge case, broken contract,
   or missing proof that makes a defect plausible.
2. **Story drift or dispute** — a promised outcome is missing, an unapproved
   outcome was added or removed, or the implementation contradicts the original
   boundary. Do not flag an explicit developer decision as drift.
3. **Missed refactoring smells** — apply the post-change-refactor checks to the
   whole plan result, not one incremental commit.
4. **General improvement** — another specific, consequential improvement tied
   to this execution that is not already covered above.

Pay special attention to cumulative-execution residue:

- superseded interim implementations, callers, flags, branches, fixtures, and
  compatibility paths;
- tests duplicated at the same observable boundary or tests that only pin an
  obsolete implementation;
- docs or comments that preserve implementation history rather than the current
  product truth; and
- additions followed by workarounds instead of removal.

When a later slice replaced temporary behavior, the replaced code and its
test/documentation residue should be gone. Do not keep a negative assertion or
documentation solely to record that the old implementation no longer exists;
retain it only when absence is an enduring product requirement.

Verify a suspected finding with focused read-only checks or tests when useful.
Do not run broad suites. Filter out style preferences, speculative redesigns,
duplicate symptoms, and findings without a plausible impact.
</step>

<step name="create_follow_up_plan_when_needed">
Recheck each historical finding against the current revision and working tree;
report a later fix, but do not plan work that is already resolved. Deduplicate
the remaining meaningful findings by root cause and frame them as a bounded
correction of the original story. If none remain, create no plan.

If findings remain, invoke **slice-planning** and write a new PLAN under its
normal `.planning/phases/` or `.planning/quick/` location. The PLAN must cite
the original story and reviewed commit set as its source. If the findings cannot
form one bounded outcome, let slice-planning's input gate stop rather than
inventing an invalid plan; report the required developer choice.

After writing the PLAN, stop. Do not invoke **slice-plan-refinement** unless the
developer separately requests it. Never invoke **execute-plan**, implement a
slice, commit, or push. Tell the user explicitly that a new plan was generated
and has not been executed.
</step>

<step name="review_the_execution_process_when_available">
Run this step only when the current conversation contains the just-finished plan
execution or a sufficiently complete execution transcript. Use the actual
thread record, including tool failures, corrections, waits, reversals, and user
responses. If context is incomplete, state the limitation and do not infer
missing events.

Propose only evidence-backed process improvements in these areas:

- redundant or wasteful steps, agents, checks, or repeated context loading;
- unclear, conflicting, or duplicated rules/skills that caused churn;
- a missing instruction or stop condition that caused a bug or avoidable
  back-and-forth;
- a decomposition or sizing assumption disproved during execution; and
- a specific practice or concept the developer could usefully learn.

Distinguish necessary investigation from avoidable waste. Propose rule or skill
changes; do not edit them in this skill. Keep repository findings and process
proposals separate, and do not create a slice PLAN for process proposals unless
the developer later selects one as work.
</step>

<step name="surface_overlooked_developer_attention">
Inspect the plan and available thread for a concrete unresolved request,
decision, warning, failed verification, or Jidoka stop that required developer
attention but appears to have been overlooked or left unanswered.

Do not infer neglect merely because the transcript is incomplete or the
developer gave no ceremonial acknowledgement. When evidence is clear, place an
attention banner at the absolute end of the response, after every other section
and completion marker:

```text
!!!!!!!!!! DEVELOPER ATTENTION REQUIRED !!!!!!!!!!
<the overlooked item, its impact, and the response needed>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
```

Nothing may follow the banner. Omit it when no overlooked action is supported by
evidence.
</step>

</process>

<success_criteria>
- One original plan/story was recovered or an explicit ambiguity was surfaced.
- Every included execution commit has a reason; unrelated commits are excluded.
- The aggregate result was reviewed for bugs, story drift, post-change-refactor
  smells, and consequential improvements.
- Superseded code and redundant or historical-only tests/docs received explicit
  scrutiny.
- A follow-up PLAN was created through slice-planning iff meaningful repository
  work remained and passed its input gate.
- Any new PLAN was reported as generated but not executed.
- Process proposals are based on the execution thread when that record exists.
- Any overlooked developer action appears in the required final banner.
- Final response includes `## EXECUTION RETROSPECTIVE COMPLETE`.
</success_criteria>

<output>
Report:

1. Resolved plan/story and provenance.
2. Included commit manifest and aggregate-diff boundary.
3. Filtered findings, ordered by impact, or `none`.
4. Follow-up PLAN path and `generated, not executed`, or `no follow-up plan`.
5. Process improvement proposals when thread evidence is available.
6. Evidence limitations.

```text
## EXECUTION RETROSPECTIVE COMPLETE
```

Append the developer-attention banner after this marker only when its evidence
gate passes.
</output>

<out_of_scope>
- Implementing or fixing findings.
- Starting, executing, committing, or pushing a follow-up PLAN; do not refine it
  unless the developer separately requests that pass.
- Editing rules or skills from process proposals.
- Reviewing unrelated repository quality.
- Treating a normal plan cleanup as lost evidence before searching Git history.
</out_of_scope>
