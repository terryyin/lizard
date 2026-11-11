# Settle an existing follow-up plan

An existing retrospective follow-up must be queued by default, or dropped on
explicit human instruction, before wrap-up can complete. Do not silently retain
an unlisted follow-up or infer permission to drop it from absent, empty, or
skipped retrospective advice. Handle it by its presence even when advice is
absent. Record which disposition was applied; use
[completion attention](../../dough-land/SKILL.md#completion-attention) after
settling it or when a disposition remains unresolved.

Without an explicit drop instruction, validate the follow-up against the
correction-input contract in
[planning scope and lifecycle](../../dough-story-refinement/references/planning.md#choose-the-planning-level),
then put its one canonical active home first in the queue. No further approval is needed. Do not
execute it during wrap-up. Do not refine or replan it except for the Intent Coverage
amendment authorized below. Preserve the plan contents needed for later execution.

Resolve one canonical active home under [dough-product-backlog](../../dough-product-backlog/SKILL.md#canonical-active-homes):

- Queue a follow-up's story, which every new correction has, under its
  identity with the plan linked in its seed; do not also queue the plan.
- Queue a plan-homed follow-up through its plan, its canonical active home; do
  not create or recover a seed for it.
- If required correction input is missing, name the missing field and do not
  guess or queue the addition. Keep the follow-up plan and any other needed
  active-work context, report the gap, and stop before deleting the completed
  predecessor's history.

Preserve unrelated queue order after that first item, near-future direction,
and human text. Repeating wrap-up must recognize either canonical home and must
not duplicate the follow-up or queue entry.

When explicitly instructed to drop the follow-up, identify its owned plan,
canonical story section when applicable, associated records, and any queue
entry. Preserve them in the
[before-cleanup commit](../SKILL.md#commit-closure-inputs-and-preserve-git-recovery),
then remove that entry through the backlog `complete --dropped` operation and delete only those owned follow-up
records during cleanup. Dropping does not claim that the follow-up was
implemented. Preserve sibling stories and unrelated work. Unresolved ownership
stops the affected deletion and blocks closure; do not leave an unlisted plan
and report wrap-up complete.

## Dispose Intent Coverage findings

When wrap-up has a returned Intent Coverage report for the checked revision,
dispose every actionable finding before Done publication. Name the checked
revision in the disposition section; one probe does not protect the entire
story. After a repair changes the checked promise, establish current
applicability before claiming completed-revision coverage. On resume, reuse a
returned report for that revision and recognize already-applied dispositions.
Record evidence and rationale for each disposition. A deliberate scratch fault
alone is not evidence of an actual delivered defect.

Compatible gaps:

- When a gap's outcome fits the existing retrospective follow-up, extend that
  follow-up's plan and proof within the same outcome, record preparation
  reassessment through the established workflow, keep one queue entry, and
  link the coverage evidence. This authorized amendment is the sole wrap-up
  exception to "do not refine or replan" above.
- Otherwise reuse matching queued work when present, or queue a separate
  understood correction with evidence, rationale, and one canonical home under
  [dough-product-backlog](../../dough-product-backlog/SKILL.md#canonical-active-homes).
  Repeating wrap-up must not duplicate that identity.

Priority of new coverage-sourced work:

- Place it after the retrospective follow-up by default.
- Place it ahead only for a dependency or material risk, recording why.
- Queue routine authorized test strengthening quietly within wrap-up's
  authority.
- Surface findings that need developer judgment, materially change priorities,
  or call the completed story's proof into question. Settle actual delivered
  defects and obtain human-owned decisions before dependent closure.

Retain justified priority exceptions and coverage evidence links through
[reconciliation](#preserve-follow-up-priority-during-reconciliation).

## Keep an active follow-up's provenance

Keep an active follow-up's queue link, correction input, and provenance. If its
source locator points into deleted predecessor history, replace the locator
with the before-cleanup commit and repository-relative path.

## Preserve follow-up priority during reconciliation

When integrating or reconciling the queued follow-up with main or this project's
configured trunk, keep the retrospective follow-up first by default even if the
target gained another top item, unless a recorded Intent Coverage priority
exception or later explicit human instruction already placed other work ahead.
This rule resolves that competing-top-item priority: retain both entries with
the follow-up first when no justified exception applies, preserving unrelated
order and compatible membership changes. Follow the installed backlog Git
adapters and their conflict/validation procedure; when competing priority stops
reconciliation, apply this rule to the resolution before continuing. Do not
select an entire side or treat Git's ours/theirs labels as intent. A later
explicit human priority instruction wins. After reconciliation, verify each
follow-up's one canonical identity is still present in the intended order and
no sibling work was lost or restored against a compatible removal.
