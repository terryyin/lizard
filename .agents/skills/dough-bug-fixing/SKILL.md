---
name: dough-bug-fixing
description: >-
  Resolves a reported discrepancy, defect, or regression by gathering expected
  versus actual behavior and passing the report into bounded shared execution,
  or by placing known larger or inconclusive work first in this project's
  product backlog. Use for a bug report, defect, regression, broken or
  unexpected behavior, a mismatch between intended and actual results, or a
  larger or inconclusive bug that needs a first-priority story. Do not use for
  the word "fix" alone, a reporting-only request, or refinement-only work.
---

# Resolve a reported discrepancy

Gather the supplied report and investigate it without repairing the product.
Close any standalone exploration workspace before invoking shared execution for
an authorized bounded repair. Return an evidence-backed disposition to the
coordinator. Route known larger work and incomplete or inconclusive attempts as
first-priority backlog stories. Do not invent a defect, dismiss a report for
lack of confirmation, or treat branch delivery as integration.

## Stay in request authority

A reporting-only or refinement-only request does not authorize execution or code
change. Contribute the report's expectations, evidence, gaps, and acceptance
examples to the existing artifact that request already owns, such as a seed
under [story refinement](../dough-story-refinement/SKILL.md). Stop after that
contribution.

Invoke this skill for an authorized discrepancy, defect, or regression, not
because the instruction contains the word "fix".

## Admit the accepted mission

Asking questions to understand the report is not a mission. Once the
instruction accepts diagnosing a report that is not known larger work (see
[Route remaining work](#route-remaining-work)),
[admit it](../dough-execute-plan/references/admit-accepted-work.md) before
investigation, unless it explicitly selects [one-shot work](../dough-execute-plan/references/one-shot.md);
a discrepancy found inside an active story returns its evidence to that story's
owner instead. Its story's Goal is resolving the reported discrepancy, its
expectations are the gathered report below, and its approach is `planless` when
the instruction also authorizes the bounded repair, otherwise `unselected`. That
reference owns its checkout, refusals, continuation, and closure.

## Gather the report

Collect what the report already supplies and name the gaps:

- intended or expected behavior
- actual or observed behavior
- evidence (repro steps, tests, logs, revision, environment)
- remaining uncertainty (validity, cause, or scope)

Resolve this project's reported and expected behavior, relevant code and tests,
tooling wrapper, focused test commands, fixture conventions, and testing rules
from the established task and repository. If required behavior or tooling
context is missing, report the gap and stop the affected work rather than
inventing it.

## Investigate in an exploration workspace

If the report is already known to be larger than the bounded repair, skip
investigation and use [Route remaining work](#route-remaining-work). Do not
start an investigation solely to justify that route.

Before checkout-bound investigation, read and follow the shared
[exploration workspace lifecycle](../dough-manual-testing/references/exploration-workspace.md).
Enter that lifecycle before investigation begins.

Use a session-created workspace only to establish the report's validity,
evidence, reproduction, and likely size. Do not repair the product there. A
temporary reproduction test or harness is investigation evidence, not a product
change or durable planning artifact. Keep the investigation bounded to the
evidence needed to choose a supported disposition.

If evidence shows that actual behavior already matches intended behavior, close
any session-created workspace safely and return an explained-no-change
disposition. Create no plan or further workspace for that result. If the report
is known larger, or investigation is incomplete or inconclusive, use [Route
remaining work](#route-remaining-work). If a repair is supported within the
ten-minute attempt, first remove owned temporary investigation artifacts and
safely close any session-created workspace, then invoke shared execution. Report
a repair the instruction did not authorize with its evidence instead. If the
lifecycle cannot close safely, return its retained-workspace handoff and do not
begin repair.

## Invoke shared execution for repair

Only after exploration is complete and any session-created workspace has closed
safely, invoke [dough-execute-plan](../dough-execute-plan/SKILL.md) as one
planless contextual instruction from the applicable local `main` or other
already integrated revision. Let execute-plan create its own execution branch
and worktree; never reuse or nest a session-created exploration workspace. For
an admitted story, execution continues under its claim in the story's owned
checkout. If the established checkout already belongs to an active execute-plan
repair, return the evidence to that owning execution instead of invoking a
nested one. Pass `--no-replan` and a ten-minute hard limit. Carry the gathered
expectation, actual behavior, evidence, and gaps. Do not plan, invent another
story, or start a local implement-and-refactor loop. That execution publishes
the validated repair through [increment and repair
publication](../dough-execute-plan/references/trunk-publication.md#publish-an-execution-increment-or-repair).
Do not push the repair through a separate procedure. Debug with available
knowledge; do not require a separate debugging skill.

The contextual instruction must require:

1. **Reproduce before repair.** Treat the likely defect as a hypothesis until a
   test confirms it. Write the minimum test or smallest addition that reproduces
   the discrepancy. Prefer updating an overlapping or contradictory test over
   adding duplicate coverage.
2. **Stable observable boundary.** If the report matches an existing E2E
   scenario's feature and user interaction, extend or add that E2E test.
   Otherwise write a small test through a stable boundary such as a controller,
   mounted component, CLI entry point, or deliberate domain API. Exercise real
   lower layers with crafted data and this project's fixture helpers; mock only
   external dependencies, subject to this project's testing exceptions. Do not
   test internal helpers when a stable boundary is available or widen exports
   solely for tests.
3. **Useful expected-versus-actual assertion.** Assert observable output such as
   the response, rendered text, terminal output, or exit code. Prefer
   expected-versus-actual assertions to boolean checks so the diff identifies
   the mismatch. Tighten assertions that could pass while the discrepancy
   remains. Run the test and confirm it fails because of the reported behavior,
   not a typo or environment issue. If that failure cannot be confirmed, do not
   claim reproduction or proceed to repair.
4. **Absence needs a product contract.** An absence assertion requires a product
   reason: the current intended behavior is that the result is missing. Removal
   history alone cannot justify it. An explicit promise such as "cancellation
   creates no order" still justifies asserting that no order exists.
5. **Smallest repair and related verification.** After a confirmed failing
   reproduction, make the smallest change that passes the test. Remove debugging
   and dead code. Run the regression test and related tests in the same file or
   feature. Select only relevant E2E specs using this project's test runner. Do
   not run the full E2E suite unless explicitly requested.
6. **Shared refactoring.** Keep the existing pre-commit refactoring obligation.
   Execute-plan delivery already runs
   [dough-post-change-refactor](../dough-post-change-refactor/SKILL.md) on the
   full uncommitted change. Do not replace that step with a local refactor loop.

A supported conclusion that the reported behavior is already correct is a valid
resolution. Use execute-plan's explained-empty-change path. Do not invent a
defect to have something to repair.

## Route remaining work

Use this one rule for known larger work, an incomplete `--no-replan` return, or
an inconclusive report. Do not invent severity categories.

For checkout-bound routing or artifact changes, continue the shared
[exploration workspace lifecycle](../dough-manual-testing/references/exploration-workspace.md)
already entered for the session, or enter it now if investigation did not need
a checkout.

Do not repeat execute-plan preservation, rollback, or retry. Link the evidence
execution preserved: under this project's executable-plan root (see [refine an oversized slice](../dough-execute-plan/references/oversized-slice.md)
and [resolve execution context](../dough-slice-planning/SKILL.md#resolve-execution-context)),
or for an escalated one-shot attempt, as edits restored in its claimed workspace.
Carry the gathered expectation, actual behavior, remaining uncertainty, and
acceptance examples into the canonical story. For an inconclusive report, the story first
asks whether intended behavior is violated, then repairs a confirmed violation.

An admitted story, including one a grown one-shot attempt escalated into, is
already that canonical home: carry the evidence into it and keep it Taken,
without queueing it again or creating another story, unless the developer
explicitly returns it to **Backlog list** with the product backlog's
`place --return`. Otherwise choose the canonical home:

- Reuse an existing owning story when moving it first in **Backlog list**
  preserves that story's scope.
- Otherwise create the canonical home with
  [story refinement](../dough-story-refinement/SKILL.md) and
  [seed format](../dough-story-decomposition/references/seed-format.md).
- If promoting an existing owning story would distort broader scope, or
  ownership is ambiguous, stop. Return a Jidoka handoff that names that
  decision. Do not guess the move.

Place the story first under **Backlog list** using
[product backlog](../dough-product-backlog/SKILL.md) canonical references,
deduplication, and Taken placement. Do not list the same work twice. Do not
move or interrupt **Taken** work; leave it running and in place, and report any
contradiction with it to the coordinator.

When the owned workspace contains an authorized canonical story, executable
plan, backlog change, or other durable planning evidence, those records follow
[preparation disposition](../dough-story-refinement/references/preparation-disposition.md).
Remove the disposable reproduction with
[retained-artifacts.mjs](scripts/retained-artifacts.mjs). Name the disposable
reproduction paths and the durable record. It removes only those disposable
paths, leaves unrelated exploration content, and never commits or pushes.
Until an explicit keep, the local draft remains in the owned workspace, is not
published, and the module's result states that pending disposition. An
explicit keep lands this owned workspace through that disposition's
[keep and publish](../dough-story-refinement/references/preparation-disposition.md#keep-and-publish-the-retained-result),
which uses [Dough Land](../dough-land/SKILL.md); that keep stops while
unrelated exploration content remains in the workspace. A different checkout
that holds a pending human edit stays untouched.

Workspace retirement after a confirmed disposition follows Dough Land's
[Retire the worktree](../dough-land/SKILL.md#retire-the-worktree). An
unrelated exploration workspace is not this session's workspace; leave it in
place. Do not start repair.

An established checkout remains under its owning workflow's delivery and
cleanup rules; do not impose this retention sequence or create a nested
workspace there.

Do not start execution of the queued work. Later refinement or planning reads
the same linked expectations, evidence, gaps, and examples; do not invent
another tracker.

If the backlog or a required canonical home cannot be identified, name the
missing context and stop rather than inventing a queue entry.

## Report the disposition

Return control to the coordinator with an evidence-backed disposition. Name the
gathered expectation, actual, evidence, and gaps, then what execution returned
or which remaining-work route was taken.

- **Repaired:** the reproduction test failed for the reported mismatch, the
  smallest change made that proof green, related verification passed, and
  shared delivery completed refactoring and delivery. One-shot delivery reached
  trunk and ends with its [retirement](../dough-execute-plan/references/one-shot.md#retire-the-workspace).
  Otherwise do not report branch delivery as integrated: the coordinator invokes
  [story wrap-up](../dough-story-wrap-up/SKILL.md#integrate-committed-story-branch-mode-closure)
  for integration to the selected target (default `main`). Reporter
  confirmation on main is pending when a repair needs it; never fabricate it.
- **Explained no-change:** exploration evidence or execute-plan's
  explained-empty-change return shows the actual behavior matches the intended
  behavior. Resolve it without a repair. One-shot work follows its
  [no-change finish](../dough-execute-plan/references/one-shot.md#finish-with-no-change);
  otherwise story wrap-up closes the admitted story and its workspace.
- **Unresolved:** validity, cause, or scope remains unconfirmed, and the report
  is not known larger and was not an incomplete or inconclusive execution
  return. Do not claim resolution or invent a queue entry.
- **Recovery:** failed delivery or shutdown remains ordinary recovery. Do not
  claim resolution.
- **Queued:** known larger work, an incomplete `--no-replan` attempt, or an
  inconclusive report now has an actionable canonical story first in the queue,
  or its admitted story stays Taken with that evidence. Do not claim resolution
  or start further work.
- **Jidoka:** routing could not move an owning story safely. Name the
  scope-distortion or ownership decision and return it to the coordinator
  without a guessed queue entry. Do not claim resolution.

Use `## BUG REPORT RESOLVED` only for a repaired disposition returned by
execute-plan or an explained-no-change disposition supported by exploration or
execute-plan evidence. That marker is not main integration and not reporter
confirmation. Otherwise report the gap without the marker.
