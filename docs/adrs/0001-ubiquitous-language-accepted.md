# 0001 — Ubiquitous language for Lizard domain concepts

**Status:** Proposed  
**Date:** 2026-08-18  
**Decision makers:** Terry  
**Consulted:** (existing product language in README, CLI, and code)

## Context

Lizard’s product vocabulary should be **consistent**: each idea has one name,
and each name means one thing. Humans, CLI help, APIs, tests, and coding agents
then share those terms.

This ADR is the **canonical ubiquitous language**. The glossary below is
the source of truth; prefer these meanings in CLI copy, APIs, tests, and
code identifiers. This glossary is amended in place. Add or change domain
terms here; do not supersede this ADR with a new one.

## Metrics

| Term | Meaning |
|------|---------|
| **CCN** | Cyclomatic complexity number of a **function**. CLI flag `-C` / `--CCN`. Distinct from **modified CCN**. |
| **Modified CCN** | CCN variant that counts a `switch` as one (`-m` / `--modified`). |
| **NLOC** | Lines of code **without comments** in a function (non-comment lines). Distinct from raw file line count. |
| **Token count** | Number of tokens in a function. |
| **Parameter count** | Number of parameters of a function. CLI flag `-a`. |
| **Nesting depth** | Maximum nesting of control structures in a function. |
| **Warning** | A function that exceeds a configured limit (CCN, parameters, NLOC, …). Non-zero process exit when warnings exist. |
| **Forgive** | Suppress warnings for a function (`#lizard forgives`, optionally for named metrics). Distinct from **whitelist**. |
| **Whitelist** | File listing functions whose warnings are ignored (default `whitelizard.txt`). |

## Analysis pipeline

| Term | Meaning |
|------|---------|
| **Language reader** | Parser for one source language. Inherits **CodeReader**. Registered in `lizard_languages/__init__.py`. |
| **CodeReader** | Base class for language readers (token generation + parse). |
| **Token** | Atomic unit produced by a **token generator** (`generate_tokens`). |
| **CodeStateMachine** | State machine that consumes tokens to recognize functions and control flow. |
| **CLikeReader** / **CLikeStates** | Shared reader/states for C-like languages. |
| **Function** | Analyzed unit with CCN, NLOC, token count, parameter count, name, location. |
| **FileAnalyzer** | Analyzes one file through a language reader and **extensions**. |
| **analyze** | Batch analysis of paths. Distinct from **analyze_file**. |
| **analyze_file** | Analyze one file (or source via `analyze_source_code`). Primary test/API boundary. |
| **Extension** | Plugin under `lizard_ext/` — extra metric or output formatter. Wired from `lizard.py`. |

## Alignment policy

- Features, tests, CLI option names, and modules follow this glossary.
- Agents treat this ADR as binding for naming choices. Humans and agents share
  an explicit dictionary instead of inferring synonyms.
- Names describe **capability**, not GSD phase or issue numbers.

## Prerequisites / Assumptions

- ADR-0000 (use ADRs) is Accepted.

## Related

- Supersedes: (none)
- Superseded by: (none)
- Links: playbook [README.md](./README.md); ADR-0000
  [use-adrs-accepted.md](./0000-use-adrs-accepted.md); navigation
  `.cursor/agent-map.md`
