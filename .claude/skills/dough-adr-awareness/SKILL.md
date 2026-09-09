---
name: dough-adr-awareness
description: >-
  Use and cite this project's current Accepted Architectural Decision Records
  for architecture-shaped work, follow supersession, surface conflicts instead
  of silently drifting, and preserve human ownership of decisions and
  exceptions. Use when a task mentions ADRs or architectural constraints, may
  reverse an existing decision, or crosses architecture areas defined by this project.
---

# ADR awareness

Keep architecture-shaped work consistent with this project's current
Accepted ADRs without taking decision authority from humans.

## Required project context

Resolve the following context from the repository's own guidance as needed for
the current request; ask the human only for a missing value that affects it:

- the repository-relative ADR store and index or catalog path;
- the fields that authoritatively classify an ADR as Proposed, Accepted,
  Rejected, or Superseded, and the human-owned rule for resolving disagreement
  between those fields;
- the filename conventions and supersession-link convention;
- the project's definition of architecture-shaped work and the areas governed
  by each ADR;
- the durable place to record an explicitly approved exception;
- the human advice, announcement, approval, rejection, and supersession
  process; and
- the precedence between ADR guidance and any planning or delivery workflow.

The proposal template is required only when the human asks for proposal-drafting
help. Do not assume a template, ADR path, status syntax, or exception trail from
another project.

If context needed for the current request cannot be resolved, name the missing
context and stop the affected work. Do not claim that an ADR check completed.

Do not require policies for situations absent from the current request. When
the index and record statuses agree, no disagreement-resolution rule is needed.
When no relevant record indicates supersession and no lifecycle change is
requested, a supersession-maintenance convention is not needed. Do not invent
either policy. An actual status disagreement or unclear successor still blocks
dependent work as described below.

## When to apply

Apply this skill when the human explicitly asks to check, cite, contradict,
supersede, or otherwise work with ADRs. Also apply it to architecture-shaped
work according to this project's definition, typically cross-cutting choices
about technology stacks, persistence, API contracts, authentication, packaging,
repository layout, or shared conventions.

Do not apply it to a local bug fix, wording change, or isolated refactor that
makes no architectural choice. Do not use it to propose or approve an ADR.
Proposal drafting is allowed only when the human explicitly asks for draft help.

## Workflow

### 1. Load and classify the current records

Read this project's ADR index or catalog explicitly, even if it is outside the
host tool's default file index. Enumerate records classified as Accepted by
this project's authoritative status fields.

Use this project's authority rule when metadata disagrees. A filename is
not sufficient authority on its own: when the authoritative index and in-file
status agree, a conflicting filename convention is a hygiene mismatch to
surface, not a reason to reclassify the record. When authoritative signals
conflict and this project's rule does not resolve them, report the ambiguity and
stop work that depends on the record's status until a human resolves it.

Treat Proposed records as non-binding drafts. Treat Rejected and Superseded
records as history unless the task is historical. If a current record links to
a successor, follow the chain to the newest valid current record. Report and
stop dependent work on a missing target, a cycle, or an unresolved successor's
status; do not silently fall back to an older decision.

If the ADR store is missing, report that fact. If it exists but has no current
records, say so and continue only if the requested work does not depend on an
unavailable decision.

### 2. Select only relevant decisions

Use the task, expected touched areas, and this project's architecture scope to
select records that might constrain the work. Inspect titles, status, scope, and
supersession metadata first. Read the full Decision and Consequences only for
plausibly relevant records. Ignore Proposed records unless the human is actively
working on that draft.

Do not recursively load unrelated guidance or turn this check into a survey of
the whole repository.

### 3. Consume and cite constraints

Follow each relevant current Accepted decision in design, executable planning,
and implementation. When a decision materially affects the approach, cite its
identifier, title, and repository-relative path. Cite only relevant decisions;
do not dump the full catalog or embed project-specific decisions in this skill.

Surface metadata hygiene mismatches separately from the decision's authority so
that a naming issue does not silently alter a current decision.

### 4. Stop on conflicts

If the requested or natural approach conflicts with a relevant current Accepted
ADR:

1. cite the conflicting record and briefly identify the incompatible choice;
2. stop the conflicting implementation and any dependent executable planning;
3. ask the human to choose whether to follow the ADR, begin their project's
   human-owned update or supersession process, or explicitly own an exception
   for this context; and
4. continue with a deviation only after explicit human approval, then record
   the ADR and exception in this project's durable trail.

Disagreement with a decision is not permission to ignore it. If the responsible
human is unavailable, leave the conflict unresolved and do not proceed along the
conflicting path.

### 5. Help with mechanical hygiene only after direction

After a human has made a status or supersession decision, help align the index,
in-file status, filename, and supersession links with this project's conventions.
Do not change an ADR to Accepted, Rejected, or Superseded without clear human
direction, and do not rewrite superseded history.

If the human explicitly requests proposal draft text, use their supplied
template, fill only evidence-supported content, and leave the record Proposed.
Remind them that consultation, announcement, decision, and approval remain
human-owned. If no required template is available, report that drafting is
unavailable rather than inventing a process or format.

## Boundaries

- Humans own proposing, consulting, deciding, accepting, rejecting,
  superseding, and approving exceptions.
- This skill consumes, cites, conflict-checks, and performs directed mechanical
  maintenance; it creates no new approval gate or architecture authority.
- Project-specific paths, status rules, architecture scope, and workflow
  precedence remain project context, not defaults supplied by this skill.
- Never copy another project's ADR decisions, domain terminology, credentials,
  secrets, personal data, or machine-specific paths into output.

## Completion

For an explicit invocation, summarize the current relevant ADRs consulted,
citations that constrained the work, conflicts or approved exceptions, metadata
hygiene findings, and unresolved gaps. End a successfully completed explicit
invocation with:

`## ADR CHECK COMPLETE`

Do not emit that marker when context, status authority, supersession, or a
human-owned conflict decision needed for the current request remains unresolved.
