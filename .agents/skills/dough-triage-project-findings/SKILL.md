---
name: dough-triage-project-findings
description: >-
  Maintains this project's retrospective findings, separates project and shared
  responsibility, follows adopted fixes through observation and recurrence, and
  queues zero to three supported new leading improvement stories, then commits
  and synchronizes the maintenance result. Use only on
  explicit invocation or as an expressly named phase of a user-invoked combined
  cycle for its selected project set. Reading findings or completing a
  retrospective does not invoke it. Does not implement the improvements.
---

# Triage this project's findings

Turn existing evidence into concise project findings and supported backlog
priorities, then deliver that maintenance result through the project's existing
workflow. An explicit invocation authorizes its findings, canonical story and
backlog edits, and their routine commit and synchronization to the established
origin target. A user-invoked combined cycle that expressly names this phase and
its project set passes the same authority to its selected project's invocation;
accept the supplied project, target and maintenance/publication scope without a
second permission question. Maintain only this invocation's established project.

Encountering findings during another task, a retrospective, a session startup,
or a historical instruction grants no such authority. Do not collect from other
projects, change published guidance, implement the selected improvements, or
repair unrelated CI. Approved finding-code adoption has its own
[dough-maintain-findings](../dough-maintain-findings/SKILL.md) contract; this skill
does not allocate Open Dough catalog codes.

## Resolve the project and records

Use the request and established project context, not a copied project name or the
installed skill directory. Resolve independently:

- The target project root and this invocation's authority. A delegated combined
  cycle supplies the user's originating instruction and its selected project.
- The canonical `DearDough.md` location: the project's explicit location takes
  precedence over `<project-root>/DearDough.md`. Locate the existing project
  findings record from project guidance or supplied context. Keep an alternate
  filename; do not create a replacement just to standardize its name.
- The canonical backlog, seed/story naming and identity conventions, existing
  queued and Taken responses, and near-future direction when present. Load
  [dough-product-backlog](../dough-product-backlog/SKILL.md) before dependent
  story or queue writes.
- The supplied evidence and relevant reported guidance/code revisions. Today's
  installation version does not establish what an earlier execution used.
- The intended origin remote and target branch, and the originating/default
  checkout when supplied. Resolve them from project or caller authority; this
  workflow supplies no fallback remote or branch.

Read existing records before proposing new ones. Missing or contradictory
authority, record locations, evidence, canonical-home conventions or target
selection stops only the dependent assessment, write or synchronization. Name
the gap, preserve affected records and continue supported independent work. Do
not search other repositories, invent a findings location or identity scheme,
or claim synchronization without a target. A consequential unresolved choice
goes to the user with its evidence and the specific decision needed.

Before the first write, apply [maintenance workspace and
delivery](references/maintenance-delivery.md). Resolve paths and conventions in
that owned checkout; the originating checkout's unrelated work stays there.

## Establish responsibility before selecting a remedy

For each interpretable finding, distinguish the original observed facts from
the current assessment. Inspect the failed responsibility and proposed remedy
against the reported shared guidance, project-owned code/configuration/rules,
and any local adaptation. Use decisive supplied evidence and only the relevant
local source/history needed to settle the attribution.

- A shared contract or platform assumption that failed belongs in
  `DearDough.md`, even when observed in this project or avoidable by a local
  workaround. Do not create a local workaround story for that shared failure.
- A project implementation or adapter violating a sound shared contract belongs
  in the project's findings record. Preserve the contract and local evidence
  supporting that distinction.
- Separate supported local and shared facets of one execution. Retain the
  shared facet, move the local evidence once, and cross-reference their
  relationship. Count the execution once for each supported conclusion and once
  in any combined frequency used to rank a response.
- When responsibility remains uncertain, keep that uncertainty and distinct
  identity explicit. Similar symptoms, titles, remedies, or a mention of CI do
  not establish the same cause or ownership.

Correct an earlier classification in either direction when evidence supports
it. Recheck each candidate story's remedy too: a correctly located finding can
still produce an incorrectly owned response. Missing reported revision/history
limits the assessment; do not infer continuity or a shared defect from today's
source alone. Keep independently supported maintenance moving.

## Preserve evidence while making the records concise

Use the project's existing finding identities and record conventions. Retain
the problem and responsibility, distinct occurrences and their provenance,
current assessment, response and actual adoption, observation criterion and
relevant exposure, and disposition/recovery information when available. These
are meanings to preserve in the existing records, not a required schema, new
status store or archive. Leave unavailable facts qualified rather than filling
them with assumptions.

When changing `DearDough.md`, reuse the provenance, occurrence-identity,
high-water numbering, size counting/ceiling and recovery safeguards in [record supported process
findings](../dough-execution-retrospective/references/process-finding-recording.md).
This explicit maintenance is a separately authorized disposition: the supported
moves and compaction below do not require size pressure. This skill owns those
edits and the owned checkout; do not import retrospective-only triggers, its
ordinary append-only candidate-write procedure, or its preparation/keep flow.
Preserve adopted codes and former-code aliases; do not rename existing codes
merely because evidence moves between files.

Match by the same concrete problem and responsibility. Merge only when the
evidence establishes that identity. Keep one canonical finding, former-code
traceability and working incoming story/evidence links. Related problems can
be grouped without merging their identities or asserting an unproved common
cause.

Compact duplicate narrative into one current explanation and a concise
recurrence summary. Retain distinct execution identities/counts, relevant
dates/revisions, evidence locators, material conditions, severity, uncertainty,
counterevidence and pre/post-fix boundaries. An execution copied into two
records counts once. Missing execution provenance permits a qualified finding,
not an invented countable occurrence. A rereview of the same event adds no row
or count; decisive new evidence enriches that occurrence. Do not use review
time as occurrence time or make an arbitrary line count the cleanup goal.

Before removing moved content, read back the destination and check its complete
evidence, identity/aliases and links against the source. Retain concise source
and recovery locators through existing project conventions or Git. If source
recovery or preservation cannot be verified, keep the source content and report
that incomplete move; do not leave a half-linked move reported as complete.
Reconcile any concurrent record changes before saving instead of overwriting
them. Keep historical observations distinct from the current explanation and
preserve unrelated human notes and completed-response history.

## Follow responses through observation and recurrence

When a finding has a claimed repair, prior response, observation or retirement,
read [follow fixes without losing evidence](references/finding-lifecycle.md)
before deciding its current disposition. Use the same finding identity and
evidence model above. Correct stale current assessments from attributable
source/adoption evidence while preserving the original observations.

Retire only with supported repair/outcome evidence or a satisfied proportionate
observation criterion with relevant use and no recurrence. Preserve recovery and
working links before removing active content. Insufficient exposure or uncertain
adoption remains open. A matching failure under the adopted repair restores its
lineage and eligibility for selection; an old branch, unapplied repair or similar
symptom alone cannot establish a failed fix.

## Select zero to three new leading stories

Read active responses and their canonical homes before ranking uncovered work.
Weigh distinct relevant occurrences, impact, current relevance, confidence,
expected benefit, effort and product direction. A severe supported one-off can
outrank frequent minor friction. More candidates do not raise the three-story
ceiling; retain the unselected findings without manufacturing additional work.

For each supported project-owned problem:

- If a queued or Taken story already covers the remedy, report its existing
  identity and link. Leave that entry's membership and relative order unchanged;
  it is not one of this invocation's new stories.
- If new work is justified, select at most three improvements with a named
  beneficiary and evaluable outcome. Record the bounded problem, evidence,
  expected benefit, scope and key examples in canonical story homes using this
  project's conventions. Link each story to its finding and each finding to
  the story, explicitly queued rather than resolved. Do not create a plan or
  take the story as part of this maintenance.
- For a supported recurrence or uncovered remainder after a completed response,
  a new bounded follow-up may be justified. Link its new evidence, prior response
  and finding lineage; preserve the original completion and apply the same
  selection ceiling and priority criteria. Do not reopen the completed story or
  create another response when active work already covers it.
- Use the installed product-backlog writer to add only those new canonical
  stories first, in justified priority order. Preserve existing Taken entries,
  unrelated queue order and direction text. Follow its identity and genuine
  prerequisite rules; an unresolved priority/meaning conflict stops the affected
  selection rather than authorizing a direction change.

Fewer stories, including zero, is a complete selection result when everything
is covered, already resolved, shared, unsupported or outside current priority. An unchanged repeat
adds no occurrence, finding, story, alias or status note merely to record that
the skill ran. A partial write names each saved destination and missing link;
claim reciprocal linking only after all affected records have been verified.

## Validate and deliver

Review resulting records against the source evidence: responsibility and remedy,
preserved distinct executions and links, exact new story count/order, unchanged
existing queue/Taken membership and relative order, and unchanged direction.
For lifecycle changes, verify actual adoption/exposure, the retirement decision
and recovery path, or restored lineage and prior-response links. Preserve the
historical observations and completed responses when the current assessment changes.
Run project-required checks appropriate to these metadata changes. Do not treat
polished prose or a local commit as proof of a completed maintenance invocation.

Then complete [maintenance workspace and
delivery](references/maintenance-delivery.md#deliver-the-maintenance-result).
Report the ownership reasons and meaningful record/queue changes briefly, plus
uncertainties and any exact unfinished step. Distinguish remote acceptance,
default-checkout refresh and workspace disposition. An unchanged result needs
no empty commit; verify its existing remote state before reporting it synchronized.
