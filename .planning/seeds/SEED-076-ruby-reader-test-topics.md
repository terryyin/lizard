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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/045-ruby-reader-test-topics/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"af18f416b87e00c67ec6acf63147bb6808815875a2b8296d4cf0204cd77234c4","plan":"f8b37f33991fb6c32cacfaa0e171e04b9ebe2e9e38425aab313d000179391b04"}}
```

**Goal:** Lizard maintainers changing the Ruby language reader can find and extend its tests in topic files within the 250-line refactoring limit, so future Ruby reader changes stop stalling at the file-size gate.

**Scope:** Split `test/test_languages/testRuby.py` (822 lines at `fa0d2441`) along its existing test classes into cohesive topic files: tokenizing, def/CCN parsing, comment line continuation, keywords as method names, blocks that end a function early, and percent literals. A topic still over the limit after that split, as the blocks-that-end-a-function-early class (about 420 lines) is, splits further by its own sub-topics, such as control-flow blocks, endless methods and method definitions, and RSpec examples. Existing `testRubyRegex.py`, `testRubyParser.py` and `testRubyControlFlow.py` stay as they are. Test-only: no reader behavior change. The same tests keep running and passing.

**Key examples:**

- Before the split, `testRuby.py` is 822 lines and one class, `Test_Ruby_blocks_that_end_a_function_early`, spans about 420 lines on its own.
- After the split, every resulting Ruby test file is at most 250 lines, and `nix develop -c python -m pytest test/test_languages` selects the same Ruby test cases, all passing.
- After the split, no test case is dropped, renamed into a duplicate, or skipped: the collected Ruby test IDs differ only in their module paths.
