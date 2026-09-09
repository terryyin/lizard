# Refactor Checks

Inspect these checks in order during the skill's read-only decision pass.
Edit only after its scope and subsystem gates permit the candidates.

## Duplication

- **"New" duplication** means at least one copy is newly introduced or closely
  related to newly introduced code — not that every copy is new. Collapse it
  even when the other side already existed.
- Look for copy-pasted blocks and parallel structures with cosmetic differences
  that the change introduced or made visible (new code repeating logic that
  already lived elsewhere).
- The same concept in two representations counts as duplication, not just
  literal copies.
- **Action:** collapse onto a single representation. Prefer reusing an existing
  helper in the right layer (service, composable, step definition) over
  inventing a new one.

## Domain naming

- Read every new or renamed identifier — files, modules, classes, functions,
  variables, tests, feature files, fixtures.
- Ask: does the name match what a domain reader expects? Does it match this
  project's domain vocabulary?
- **Action:** rename when intent is unclear, misleading, mixes layers, or leaks
  development sequence numbers. Name product code by capability; keep sequence
  numbers in planning artifacts.

## Shotgun surgery

- Shotgun surgery: **one logical concept** (e.g. a version string) forces edits
  in many places for one purpose.
- Give the concept **one** representation. The next change of that shape should
  touch that place — not be scattered again.
- Acceptable extra touchpoints: tests that assert the concept, and generated
  code derived from it. Do not hardcode the same value in product paths,
  fixtures, E2E config, and feature files in parallel.
- **Action:** consolidate now behind one seam (one constant, config, or module).
  Leave only low-likelihood one-offs unabstracted.

## Dead or redundant code

Apply the [scope and justification boundary](../SKILL.md#discover-scope)
to code the current change introduced or exposed. Remove:

- Code with no caller.
- Unreachable branches.
- Pairs of edits that cancel each other (added then worked around, flags that
  never flip).
- Production code only exercised by unit tests — no real caller from a
  controller, mounted component, CLI command, MCP tool, or other entry.
- Unit tests that overlap another test on the same observable surface (same
  input/output, same entry point).
- Tests that pin internal structure rather than observable behavior — prefer
  the test that drives a stable boundary (controller, mounted component,
  end-to-end scenario).

Remove speculative code within that boundary; do not retain it for an
unspecified future need.

## File size

For every file in the current diff and every file proposed for editing:

```bash
wc -l <path>
```

Use this project's file-size limits and exemptions for plans, generated artifacts,
production code, and tests. Split files exceeding their applicable limit.
Do not impose limits copied from another project's stack.

- Split along **cohesive seams** — one concept per module, not arbitrary line
  cuts.
- Update imports. Keep the public API stable for callers outside the change.
