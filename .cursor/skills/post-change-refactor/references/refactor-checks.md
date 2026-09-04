# Refactor Checks

Apply these checks in order after the preflight gates record an edit candidate.

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
  helper in the right layer (language reader, extension, shared tokenizer
  utility in `code_reader.py`) over inventing a new one.

## Domain naming

- Read every new or renamed identifier — files, modules, classes, functions,
  variables, tests, fixtures.
- Ask: does the name match what a domain reader expects? Does it match Lizard's
  ubiquitous language (ADR-0001: CCN, NLOC, language reader, token, state
  machine, extension)?
- **Action:** rename when intent is unclear, misleading, mixes layers, or leaks
  GSD phase numbers / sequence info. Names describe **capability**, not
  development history. GSD phase numbers belong only under `.planning/`.

## Shotgun surgery

- Shotgun surgery: **one logical concept** (e.g. a default threshold, a language
  keyword list, an extension option name) forces edits in many places for one
  purpose.
- Give the concept **one** representation. The next change of that shape should
  touch that place — not be scattered again.
- Acceptable extra touchpoints: tests that assert the concept. Do not hardcode
  the same value in reader, extension, CLI options, and test fixtures in
  parallel.
- **Action:** consolidate now behind one seam (one constant, config, or module).
  Leave only low-likelihood one-offs unabstracted.

## Dead or redundant code

Remove aggressively whatever the change introduced or exposed that is not
justified by the current change or the immediate next slice:

- Code with no caller.
- Unreachable branches.
- Pairs of edits that cancel each other (added then worked around, flags that
  never flip).
- Production code only exercised by unit tests — no real caller from the CLI
  (`lizard` command), `analyze_file` API, language reader pipeline, or
  extension hook.
- Unit tests that overlap another test on the same observable surface (same
  input/output, same entry point).
- Tests that pin internal structure rather than observable behavior — prefer
  the test that drives a stable boundary (`analyze_file`, `analyze_source_code`,
  CLI output) per `basic-development.mdc`.

When in doubt, **delete**. The next slice will reintroduce only what it needs.

## File size

For every file in the current diff and every file proposed for editing:

```bash
wc -l <path>
```

(`wc` is a host command — do not start a nix shell just to count lines.)

- Files **over 250 lines** must be split (applies to test code too).
- Split along **cohesive seams** — one concept per module, not arbitrary line
  cuts.
- Update imports. Keep the public API stable for callers outside the change.
