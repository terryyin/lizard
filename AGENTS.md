# AGENTS.md

Index for Codex and other AI coding agents. Skill contracts: `.cursor/skills/`; rules: `.cursor/rules/`.

Lizard is an extensible cyclomatic complexity analyzer for many programming languages. It also does copy-paste detection and other static analysis.

Start with `.cursor/agent-map.md` for repo navigation, focused commands, and default indexing notes.

Run repo tooling with `nix develop -c …`. **Git commands do not need the Nix prefix** — run `git` directly.

Repo conventions live in `.cursor/rules/`. Cursor injects `alwaysApply: true` rules automatically. **Codex / Claude Code:** read these always-applied rules before coding — `basic-development.mdc`, `problem-decomposition.mdc`, `planning.mdc`, `gsd-coexistence.mdc`, `architecture-decisions.mdc`, `issue.mdc` — then `lizard-rule.mdc` when adding or changing a language reader.

Planning lives under `.planning/` (GSD + local). Canonical coexistence:
`.cursor/rules/gsd-coexistence.mdc`. Decomposition and slice quality:
`.cursor/rules/problem-decomposition.mdc`; planning artifacts and lifecycle:
`.cursor/rules/planning.mdc`.
Legacy holdouts may remain under `ongoing/` — do not put new plans there.

## Principles

Portable digest (details live in the cited always-applied rules):

1. High cohesion — one concept, one place (`planning.mdc`, post-change-refactor)
2. Keep it simple — minimum code; no speculative structure
3. Capability naming — no GSD phase numbers in product artifacts (`planning.mdc`, ADR-0001)
4. Test observables via high-level entry points (`analyze_file`, CLI) (`basic-development.mdc`)
5. Do not mock parsing logic; mock only filesystem boundaries (`basic-development.mdc`)

## Planning and slice delivery

- **Layout (GSD-aligned):** non-executable story decompositions under `.planning/seeds/`; executable work under `.planning/phases/NN-slug/` or `.planning/quick/NNN-slug/`, plus GSD `PROJECT` / `ROADMAP` / `STATE` / `codebase/`. See `planning.mdc` and `gsd-coexistence.mdc`.
- **Hard decomposition grammar:** problem → 3V story → Behavior/Structure execution leaf; stop-safe, one evaluable outcome at the current resolution (`problem-decomposition.mdc`) — applies to GSD PLANs too.
- **Time budget (self-enforced):** story hypotheses are roughly 30 minutes to a few hours; execution leaves target ~5 min including tests; >5 min → scrutinize; >10 min → hard finer-decompose unless a stated good reason (`problem-decomposition.mdc`).
- **History:** keep resume-useful planning artifacts while a plan is in progress; **clean up** spent history when the plan is fully executed into code/permanent docs.
- **Execution wrap-up (required):** Jidoka → post-change-refactor → pep8 → full pytest → update plan → commit → push (**execute-plan**; also `/gsd-execute-phase`). Skills emit completion markers for handoff.
- **Story shaping:** use **story-decomposition** for broad or unclear requirements; one non-executable decomposition seed contains ordered candidate stories.
- **GSD** for milestones (`/gsd-onboard`, `/gsd-plan-phase`, `/gsd-execute-phase`, …); **slice-planning** + **execute-plan** for one selected story under `.planning/quick/`.
- **Non-compatible local overlays** (must keep): documented in `.cursor/rules/gsd-coexistence.mdc`.

## Architectural decisions

- Human propose / discuss / approve: `docs/adrs/README.md`
- Current recommendations: `docs/adrs/*-accepted.md` (read explicitly)
- Agent use / cite / conflict / maintain: `.cursor/skills/adr-awareness/SKILL.md`
