# Admit accepted work that no backlog list holds

Use the shared [startup command](../SKILL.md#take-or-admit-work) with `--admit`
for accepted independent work before diagnosis, review, observation, or edits.
Start queued work through Take and continue work you already hold under its
claim. Report another agent's claim. In an [established session](established-session.md),
use its workspace and branch rules. [One-shot work](one-shot.md) is admitted
only if it [grows](one-shot.md#escalate-when-the-work-grows), using `--carry`
to include its edits and move a queued entry when applicable.

## Decide whether a mission was accepted

A mission is an independently requested outcome the developer or parent
instruction has accepted for work, such as a bug diagnosis, exploratory test,
standalone review, maintenance task, or retrospective correction. Clarification,
reporting, refinement, and recommendations await their own applicable workflow.
Tests, refactoring, CI repair, and retrospective for an active story use its
claim. Queued preparation keeps its Preparing assignment. Caller-selected
current-branch work has no remote claim.

For test optimization, [discovery](../../dough-test-optimization/SKILL.md) leaves
no claim. Reading evidence, timing unchanged tests, and explicitly requested
profiling can happen in discovery. Admit each authorized bounded experiment as
`planless` before its first edit. If the evidence calls for a plan, admit it
`unselected` before planning.

## Prepare the story

Supply `--link <seed path>#<anchor> --title <entry title>` with the link relative
to the backlog directory. Choose the source of the story:

- **Draft in a checkout.** Reuse the canonical story, including the story of an
  accepted retrospective correction. For a new story,
  [allocate its identity](../../dough-product-backlog/references/identity.md#allocate-a-new-identity)
  and write its Identity, Goal, scope, and known expectations. Use
  [record-state](../../dough-product-backlog/references/record-preparation.md)
  for the actual approach: `unselected`, explicitly authorized `planless`,
  or `planned` with its plan. Pass the checkout holding the draft as
  `--integration`. Without it, startup reads the published story.
- **Draft owned by a launch.** Run `execution-start.mjs draft` with
  `--repository`, `--remote`, `--target`, and a one-line `--title` and
  `--goal`. Retain the returned identity, link, title, and `ownedDraft` with
  the attempt. Pass them to the same start command, using `--owned-draft`
  for the JSON object and the launch's stable publisher ID. The claim writes
  the new seed with `unselected` preparation. A default-checkout launch
  passes that checkout as `--repository` and claims in a separate clean
  Trunk Mode worktree, preserving the session checkout.

## Act on the result

Admission publishes the story section (or whole new seed), its declared plan,
the Taken entry, and agent assignment together. It reports carried paths as
`admitted`. After confirmation, startup withdraws the published draft from
its originating checkout where unchanged, preserving sibling edits, subsequent
edits and staged content. Refresh is reported separately.

Follow the shared [start result and recovery](execution-location.md#read-and-recover-the-start-result)
with the original admission flags. An existing claim you own continues.
A source refusal identifies the missing or conflicting story input. A done
identity needs a fresh allocation. A source conflict reports the path and
preserves both versions for a human decision. Resume an uncertain launch with
the retained draft, starting revision, and candidate SHA. Allocate a new
identity only when the previous attempt is confirmed unpublished. For a losing
retained launch candidate in Trunk Mode, `--retry-admission true` reports
`retry-admission` once the candidate is preserved and the checkout can retry.

## Continue into implementation

Admission grants no implementation authority or readiness. Proceed under the
explicit planless instruction when applicable. Otherwise, once authorized,
record the approach and [assess readiness](../../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion)
on the same story. Publish its preparation through ordinary keep, including
the plan and its Taken link for planned work. Run the usual start without
`--admit` to resume the existing claim. Use ordinary checkout-bound setup
before implementation.

## Finish the mission

An evidenced no-change conclusion completes the mission through ordinary
[story wrap-up](../../dough-story-wrap-up/SKILL.md). Inconclusive work stays
Taken until it completes or receives an explicit disposition.
