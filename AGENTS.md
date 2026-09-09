# AGENTS.md

Index for Codex and other AI coding agents. Start with
`.cursor/agent-map.md` for repository navigation and focused commands.

Lizard is an extensible cyclomatic complexity analyzer for many programming
languages. It also does copy-paste detection and other static analysis.

Project-local Open Dough lifecycle skills live under `.agents/skills/dough-*`;
Claude Code receives the same payload under `.claude/skills/dough-*`.

Run repository tooling with `nix develop -c …`. Git commands do not need the
Nix prefix.

Project-specific conventions live in `.cursor/rules/`:

- `basic-development.mdc` for tests and tooling.
- `lizard-rule.mdc` when adding or changing a language reader.

## Principles

1. Keep one concept in one place.
2. Prefer the minimum code needed for the requested behavior.
3. Test observables through high-level entry points such as `analyze_file` and
   the CLI.
4. Do not mock parsing logic; mock only filesystem boundaries.
5. Preserve unrelated working-tree changes.

## Architectural decisions

- Human propose / discuss / approve process: `docs/adrs/README.md`
- Current recommendations: `docs/adrs/*-accepted.md`
- Agent use / cite / conflict / maintain:
  `.agents/skills/dough-adr-awareness/SKILL.md`
