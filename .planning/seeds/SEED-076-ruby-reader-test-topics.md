---
id: SEED-076
status: active
planted: 2026-11-26
---

# SEED-076 Ruby reader test organization

The developer deferred this during SEED-075#ruby-percent-literal-context: the post-change refactor stopped on the file-size gate, and the developer chose "Defer as follow-up" on 2026-11-26. See the retained occurrence in [DearDough DD-490](../../DearDough.md#dd-490--planned-proof-home-was-already-oversized-without-a-reorganization-decision).

<a id="ruby-reader-test-topics"></a>

### Split the Ruby reader tests by topic

**Identity:** SEED-076#ruby-reader-test-topics

**Goal:** Lizard maintainers changing the Ruby language reader can find and extend its tests in topic files within the 250-line refactoring limit, so future Ruby reader changes stop stalling at the file-size gate.

**Scope:** Split `test/test_languages/testRuby.py` (822 lines at `fa0d2441`) along its existing test classes into cohesive topic files: tokenizing, def/CCN parsing, comment line continuation, keywords as method names, blocks that end a function early, and percent literals. Existing `testRubyRegex.py`, `testRubyParser.py` and `testRubyControlFlow.py` stay as they are. Test-only: no reader behavior change. The same tests keep running and passing.

**Key examples:**

- After the split, every resulting Ruby test file is at most 250 lines, and `nix develop -c python -m pytest test/test_languages` selects the same Ruby test cases, all passing.
