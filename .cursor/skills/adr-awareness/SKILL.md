---
name: adr-awareness
description: >-
  Use, cite, and maintain consistency with Accepted Architectural Decision
  Records under docs/adrs/. Load relevant ADRs for architecture-shaped work,
  flag conflicts (refuse silent drift; allow human-owned exceptions), and help
  update supersession/index hygiene. Does not propose, discuss, or approve
  ADRs — humans own the advice process. Triggers on: ADR, architectural
  decision, architecture constraint, contradict ADR, supersede ADR, check
  ADRs, docs/adrs.
---

<objective>
Keep agent work consistent with **Accepted** ADRs without taking human
decision authority.

Purpose: Humans own propose → discuss → approve (`docs/adrs/README.md`).
Agents **consume**, **cite**, **conflict-check**, and help **maintain** records.

Output: Cited ADRs in reasoning and answers; conflict surfaced or
human-owned exception noted; optional hygiene edits (index links, Superseded
status) ending with `## ADR CHECK COMPLETE` when invoked explicitly.
</objective>

<context>
**Locations (docs/ is not in default index — read explicitly):**

| Path | Role |
|------|------|
| `docs/adrs/README.md` | Human playbook + index table |
| `docs/adrs/_template.md` | Template for humans (do not invent process) |
| `docs/adrs/*-accepted.md` | **Current** recommendations — primary input |
| `docs/adrs/*-rejected.md` | History only |
| `docs/adrs/NNNN-*.md` without suffix | Proposed — not binding yet |

**Always-current rule:** Prefer **Accepted** ADRs. Treat **Superseded** and
**Rejected** as archaeology unless the task is historical. If an Accepted ADR
links to a superseding ADR, follow the newer one.

**When this skill applies (architecture-shaped work):**
- Cross-cutting stack, parsing model, CLI/API contracts, packaging, shared
  conventions across `lizard.py` / `lizard_languages/` / `lizard_ext/`
- Explicit user mention of ADR / architecture decision
- Change that would reverse or bypass an existing Accepted ADR

**When it does not apply:**
- Local bugfix, copy, or single-module refactor with no architectural choice
- Writing or approving a new ADR unless the human asked only for **draft text**
  for *their* proposal (they still announce, discuss, and approve)

**Conflict policy (hybrid):**
- **Default (agent alone):** Do **not** implement an approach that conflicts
  with an Accepted ADR. Stop, cite the ADR, and ask how to proceed
  (follow ADR / human opens superseding ADR / human owns exception).
- **Human-owned exception:** If the human **explicitly** accepts a deviation
  for this context, proceed and leave a trail (PR/commit message or note
  pointing at the ADR and the exception). Do not invent silent exceptions.
- Not agreeing with an ADR is normal; that is not grounds for the agent to
  ignore it.
</context>

<process>

<preflight_gate name="load_index">
Read `docs/adrs/README.md` and list every **Accepted** ADR path from the index
(and/or glob `docs/adrs/*-accepted.md`). If the directory is missing or empty
except the meta ADR, continue with whatever Accepted files exist.
</preflight_gate>

<step name="select_relevant">
From the task and touched areas, open only ADRs that might constrain the work.
Skim titles/Status first; read full Decision + Consequences for candidates.
Ignore Proposed unless the user is actively working that draft.
</step>

<step name="consume_and_cite">
When an Accepted ADR constrains the approach:
- Follow it in design and implementation.
- Cite it briefly (ADR number + title + path) in the reply when the choice
  matters — do not dump every ADR.
</step>

<step name="conflict_check">
If the requested or natural implementation conflicts with an Accepted ADR:
1. Cite the conflicting ADR (quote the Decision line if short).
2. **Stop** implementing the conflicting path.
3. Ask the human to choose: follow ADR, supersede/update via human process,
   or explicitly own an exception for this context.
4. Only after an explicit exception, continue and record the trail as above.
</step>

<step name="maintain_hygiene">
When the human has already Accepted/Rejected/Superseded an ADR (or asks to
clean up), help with mechanical consistency only:
- Update the index table in `docs/adrs/README.md`
- On supersession: set old Status to Superseded with link; ensure new file is
  `*-accepted.md`
- Do **not** change Status to Accepted/Rejected/Superseded unless the human
  clearly directed that status change
- Do **not** invent new ADRs or approve drafts as part of coding work
</step>

<step name="draft_help_only_when_asked">
If the human asks to draft proposal text: copy `_template.md`, fill Context /
options from the codebase, leave Status `Proposed`, and remind them they own
announce → discuss → approve. Do not announce or treat the draft as Accepted.
</step>

</process>

<success_criteria>
- Relevant Accepted ADRs were read explicitly from `docs/adrs/`
- Conflicts were not silently implemented
- Human exception (if any) was explicit and trailed
- No agent-owned approval of ADRs
- Explicit invocations end with `## ADR CHECK COMPLETE`
</success_criteria>
