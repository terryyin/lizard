---
name: dough-post-change-refactor
description: >-
  Refactors concepts implicated by the current uncommitted change before commit.
  Includes untouched representations and orchestration needed for coherence,
  stops before unapproved cross-subsystem refactoring or disputed plan behavior,
  and tests only after edits. Use after a slice, for post-change refactoring,
  or to clean up the current change.
---

# Refactor the current change

Preserve observable behavior while making implicated concepts cohesive, clearly
named, and justified. Return to the caller without starting another slice,
committing, pushing, amending, or triggering CI.

## Discover scope

Run `git status`, `git diff`, and `git diff --cached`. If there is no uncommitted
change, report empty scope and use the completion handoff below without edits
or tests.

For a nonempty change, resolve this project's navigation, domain vocabulary,
production subsystem boundaries and applicable architectural decisions, tooling wrapper,
whitespace check with generated-artifact exclusions, and focused test commands.
Resolve generation triggers and commands only when affected. If necessary
context is missing, report it and stop without claiming completion.

Use the caller's active plan and current slice when supplied. Code may be
justified by the current change or the immediate next slice in that plan;
without a plan, only the current change supplies justification. Later slices
cannot justify speculative code. The next slice can justify retaining code but
cannot independently trigger refactoring.

## Decide before editing

Read [refactor checks](references/refactor-checks.md) and make one read-only pass
through them. For each candidate, identify:

1. An issue introduced, exposed, or materially aggravated by the current change.
2. The smallest complete set of representations, callers, tests, fixtures, and
   configuration needed to make the concept coherent.
3. The production subsystems that the refactoring would change.

Trace the concept through its representations and orchestration, including
untouched code that repeats its knowledge or must change for coherence. Apply
[delivery scope versus implementation reach](../dough-story-refinement/references/planning.md#examples-and-constraints):
story membership cannot exclude a necessary edit. Dependency adjacency alone
does not establish scope. Do not initiate unrelated cleanup discovered during
tracing.

If a candidate exposes a disputed plan restriction, use the shared
[plan-conflict handoff](../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction)
before editing that behavior. Preserving it is not a clean review result when its
justification is disputed; return a decision stop without disguising a behavior
change as refactoring.

Identify real production boundaries from domain responsibilities and this
project's architectural decisions; delivery groupings and story-named handlers
do not establish separate subsystems. If product organization coincides with
story boundaries, require independent domain justification. Whole-concept
examination does not authorize overriding an architectural decision; surface
any conflict for human resolution before the conflicting edit.

If a candidate needs coordinated production refactoring in more than one
production subsystem, stop before editing unless the human has authorized that
named concept and those subsystems, or the active plan specifically identifies
the cross-subsystem structure as necessary for the current authorized
responsibility. The plan must connect the concept, affected boundaries, and
current outcome; an inferred benefit, generic cleanup authorization, or a later
story is insufficient. Tests, fixtures, generated artifacts, and configuration
following one production seam do not alone constitute a crossing; neither does
the original behavior change spanning subsystems.

If this crossing becomes apparent during editing, stop before the first
cross-subsystem edit and reverse only this agent's edits for that candidate.
Preserve others' changes and return the decision handoff below. Do not substitute
a partial refactor or proceed with other candidates while the gate is unresolved.

With no candidates, report `none — already clean` and complete without tests.
Otherwise edit in refactor-check order, without repeating broad discovery.
Use this project's whitespace check. Regenerate affected artifacts through their
source generator and validate consumers; never manually repair generated output.

## Verify edits

When the caller supplied accepted proof, compare each refactor edit with its
reported boundary, implementation, setup, and observation locations. Identify
which accepted proof remains unchanged and which the refactor invalidated;
unchanged proof requires no rerun. Rerun only commands whose covered behavior or
paths the refactor invalidated. If the covered boundary moved, explain why the
original command no longer applies and run a focused replacement. Without
supplied proof, run focused tests related to the refactor edits. Use this
project's literal commands and the shared
[behavioral test guidance](references/refactor-checks.md#tests-as-behavioral-documentation).
Do not run the full suite. Own any verification command that yields a running
command identity through its terminal result under the shared
[verification ownership](../dough-execute-plan/references/delegation.md#own-verification-to-its-terminal-result)
rule. Fix failures caused by the refactor and require
passing relevant proof before completion. Report other unresolved failures to
the caller; do not claim success.

## Return control

On completion, return a targeted report with the refactor outcome, every path
and conceptual or product boundary changed by refactoring, and checks that
changed code. For each relevant accepted proof, name whether its boundary and
inspected locations stayed unchanged or which edit invalidated them. For
invalidated or replacement proof, report the literal command, result, and
concrete setup and assertion or signal locations. Also report files renamed,
extracted, split, or deleted; unresolved gaps or contradictions; consequential
learnings; passing test commands or `skipped — no refactor edits`; and
approximate active elapsed time. Do not attach routine raw traces, full command
logs, or a duplicate full diff; the caller inspects the reported locations. End
with `## REFACTOR COMPLETE`.

For a disputed plan restriction, return the shared plan-conflict handoff and end
with `## REFACTOR JIDOKA STOP`; the caller must resolve it before treating the
review as complete or committing.

When the subsystem gate stops work, report the triggering issue, concept,
affected subsystems and representative files, why a narrower change would be
incomplete, expected risk and focused validation, and elapsed time. Ask the human
to authorize the refactoring, defer it, or approve a described narrow exception.
End with `## REFACTOR JIDOKA STOP`. The caller must resolve this decision before
considering refactoring complete or committing.
