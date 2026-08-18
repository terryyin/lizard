# Architectural Decision Records (ADRs)

Human playbook for **proposing, discussing, and approving** architectural
decisions. Agents **use, cite, and help maintain** Accepted ADRs — they do not
own approval. See `.cursor/skills/adr-awareness/SKILL.md`.

## Advice process (authority)

Anyone may make an architectural decision. Before deciding, seek advice from
people affected by it or who want to be involved. There is no committee vote
and no separate architect approval. Consensus is not required — consultation is.

After you decide, communicate it so others can follow it (or challenge a bad
deviation later).

## When to write an ADR

Write one when the choice is **cross-cutting**, **hard to reverse**, or
**likely to be re-asked** ("why do we do this?"). Skip local, easily reversible,
or obviously one-team choices.

ADRs are not phase plans. Near-term delivery stays under `.planning/`. An ADR
records a durable constraint or choice that should outlive a phase.

## Lifecycle (humans)

| Status | Meaning |
|--------|---------|
| **Proposed** | Draft open for advice; not yet binding for agents |
| **Accepted** | Current recommendation teams and agents should follow |
| **Rejected** | Considered and declined; keep the record and reasoning |
| **Superseded** | Replaced by a newer ADR; keep history, do not treat as current |

Only humans move an ADR to Accepted, Rejected, or Superseded.

### Steps

1. **Draft** — Copy `_template.md` to `NNNN-short-title.md` (next free number).
   Status: `Proposed`. Fill Context, Decision, Consequences (optional Pros /
   Cons / Prerequisites / Assumptions).
2. **Announce start** — Tell the team you are deciding *X* and link the draft
   (PR, chat, or meeting). People who want to contribute opt in; people who only
   need the outcome wait for approval.
3. **Discuss** — Arrange whatever form fits (PR comments, thread, workshop).
   You own how discussion runs. Seek advice until significant open issues are
   addressed or explicitly accepted as trade-offs.
4. **Decide** — When you have done that homework, you approve (or reject)
   yourself. Rename to `…-accepted.md` or `…-rejected.md`, set Status, and open
   the status-change PR as the last discussion surface if needed.
5. **Announce end** — Share the Accepted/Rejected ADR and, for non-trivial
   decisions, hold a short info session (record and link from the ADR when
   useful). Approval announcements are for learning, not reopening debate
   unless something major is wrong.
6. **Supersede** — To change course, add a new ADR and mark the old one
   `Superseded by ADR-NNNN` with a link. Do not silently rewrite history.

## Recommendations, not law

Accepted ADRs are **recommendations**. A team may deviate when their context
demands it. Expect peers to ask why; either explain the exception or update /
supersede the ADR. Escalation for bad-faith ignore-everyone decisions is a
**people/behavior** issue for humans — not an architecture committee redo.

## Layout

| Path | Role |
|------|------|
| `docs/adrs/_template.md` | Copy this for new ADRs |
| `docs/adrs/NNNN-slug.md` | Proposed |
| `docs/adrs/NNNN-slug-accepted.md` | Accepted |
| `docs/adrs/NNNN-slug-rejected.md` | Rejected |
| `docs/adrs/README.md` | This playbook |

Number sequentially from `0000`. Prefer capability-named titles, not phase numbers.

## Index

| ADR | Status | Title |
|-----|--------|-------|
| [0000](./0000-use-adrs-accepted.md) | Accepted | Use ADRs for architectural decisions |
| [0001](./0001-ubiquitous-language-accepted.md) | Accepted | Ubiquitous language for Lizard domain concepts |

Keep this table current when you accept, reject, or supersede an ADR.
