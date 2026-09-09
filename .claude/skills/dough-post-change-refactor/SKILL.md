---
name: dough-post-change-refactor
description: >-
  Refactors concepts implicated by the current uncommitted change before commit.
  Includes untouched code needed for coherence, stops before unapproved
  cross-subsystem refactoring, and tests only after edits. Use after a slice,
  for post-change refactoring, or to clean up the current change.
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
production subsystem boundaries, file-size limits and exemptions, tooling wrapper,
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

Include untouched code when it represents the same concept, duplicates the same
knowledge, or must change for coherence. Dependency adjacency alone does not
establish scope. Do not initiate unrelated cleanup discovered during tracing.

If a candidate needs coordinated production refactoring in more than one
production subsystem, stop before editing unless the human has authorized that named concept
and those subsystems. Generic cleanup authorization is insufficient. Tests,
fixtures, generated artifacts, and configuration following one production seam
do not alone constitute a crossing; neither does the original behavior change
spanning subsystems.

If this crossing becomes apparent during editing, stop before the first
cross-subsystem edit and reverse only this agent's edits for that candidate.
Preserve others' changes and return the decision handoff below. Do not substitute
a partial refactor or proceed with other candidates while the gate is unresolved.

With no candidates, report `none — already clean` and complete without tests.
Otherwise edit in refactor-check order, without repeating broad discovery.
Use this project's whitespace check. Regenerate affected artifacts through their
source generator and validate consumers; never manually repair generated output.

## Verify edits

When the caller supplied `proof:` commands, rerun only those whose covered
behavior or paths the refactor invalidated. If the covered boundary moved,
explain why the original command no longer applies and run a focused replacement.
Without supplied proof, run focused tests related to the refactor edits. Use
this project's literal commands and observable stable boundaries, real lower layers,
and crafted data; mock external services rather than internal collaborators.
Do not run the full suite. Fix failures caused by the refactor and require
passing relevant proof before completion. Report other unresolved failures to
the caller; do not claim success.

## Return control

On completion, report checks that changed code, files renamed, extracted, split,
or deleted, passing test commands or `skipped — no refactor edits`, and approximate
active elapsed time. End with `## REFACTOR COMPLETE`.

When the subsystem gate stops work, report the triggering issue, concept,
affected subsystems and representative files, why a narrower change would be
incomplete, expected risk and focused validation, and elapsed time. Ask the human
to authorize the refactoring, defer it, or approve a described narrow exception.
End with `## REFACTOR JIDOKA STOP`. The caller must resolve this decision before
considering refactoring complete or committing.
