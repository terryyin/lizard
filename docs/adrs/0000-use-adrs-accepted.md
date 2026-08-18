# 0000 — Use Architectural Decision Records (ADRs)

**Status:** Accepted  
**Date:** 2026-08-18  
**Decision makers:** Team  

## Context

Architectural choices for Lizard must not depend on a standing architect role,
nor on a community that becomes an accidental decision forum. Contributors need
autonomy to decide, a duty to seek advice from people who care, and a durable
record so humans and AI agents can recall *why* we chose something.

Phase plans under `.planning/` track near-term delivery and are cleaned up when
done. They are the wrong place for long-lived architectural constraints.

## Decision

1. **Human advice process** — Anyone may decide. Before deciding, ask people
   affected or interested for advice. No vote or committee approval. The
   decision maker owns the call after consultation. Process details:
   `docs/adrs/README.md`.
2. **Document with ADRs** — Record cross-cutting / hard-to-reverse / often
   re-asked choices under `docs/adrs/`, using `_template.md`, numbered files,
   and statuses Proposed → Accepted | Rejected | Superseded.
3. **Agents consume, do not approve** — Coding agents must find and follow
   **Accepted** ADRs, cite them when relevant, and surface conflicts. They must
   not treat Propose/Discuss/Approve as their mandate. Skill:
   `adr-awareness`.
4. **Recommendations** — Accepted ADRs guide daily work. Deviations need an
   explicit human-owned exception or a superseding ADR. Peers (and agents)
   challenge silent drift.
5. **Change over time** — Adjust an ADR or add a new one that supersedes the
   old. Implementation work to align the codebase still goes through normal
   product prioritization — an ADR does not auto-prioritize backlog items.

## Consequences

- New Accepted ADRs become part of agent-facing architectural memory.
- Humans keep authority; agents keep continuity.
- Rejected and Superseded ADRs remain for archaeology; only Accepted ones are
  current recommendations.
- Planning (GSD / slice-planning) stays for delivery; ADRs stay for durable
  decisions.

## Related

- Playbook: [README.md](./README.md)
- Agent skill: `.cursor/skills/adr-awareness/SKILL.md`
- Background: [advice process](https://www.reinventingorganizationswiki.com/Theory/Advice_Process/); ADR templates inspired by [Michael Nygard / joelparkerhenderson ADR](https://github.com/joelparkerhenderson/architecture-decision-record)
