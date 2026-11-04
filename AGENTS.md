# AGENTS.md

Index for Codex and other AI coding agents. Start with
`.agents/agent-map.md` for repository navigation and focused commands.

Lizard is an extensible cyclomatic complexity analyzer for many programming
languages. It also does copy-paste detection and other static analysis.

Project-local Open Dough lifecycle skills live under `.agents/skills/dough-*`;
Claude Code receives the same payload under `.claude/skills/dough-*`.

## Environment and tests

All coding agents should run local project tooling through `scripts/dev` from
the repository root. It uses offline Nix and disables pip package indexes by
default, so agents can use it on a hotspot or when the connection is unknown.

```bash
./scripts/dev <command>
```

If dependencies are missing, report the blocker. Run `nix develop` for online
setup only after the user confirms an unmetered connection or authorizes the
downloads. Then return to `scripts/dev` for project tooling.

Run `git` commands directly.

During focused work, targeted pytest files are enough. Before completing a
change, run the full suite:

```bash
./scripts/dev python -m pytest
```

Run all tests and format checking:

```bash
./scripts/dev make
```

Adding or changing a language reader: use the `lizard-language-support` skill
(`.agents/skills/lizard-language-support/SKILL.md`).

## Principles

1. Keep one concept in one place.
2. Prefer the minimum code needed for the requested behavior.
3. Test observables through high-level entry points such as `analyze_file` and
   the CLI.
4. Do not mock parsing logic; mock only filesystem boundaries.
5. Preserve unrelated working-tree changes.
6. Cover code and logic with unit tests. Prefer end-to-end, externally
   observable pre- and post-state change.

## Architectural decisions

- Human propose / discuss / approve process: `docs/adrs/README.md`
- Current recommendations: `docs/adrs/*-accepted.md`
- Agent use / cite / conflict / maintain:
  `.agents/skills/dough-adr-awareness/SKILL.md`
