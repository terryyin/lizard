---
id: SEED-073
status: active
planted: 2026-11-24
planted_during: master CI repair after shared implied-semicolon scan
scope: restore green master tests for Svelte under ND/NS
---

# SEED-073: Fix Svelte implied-semicolon CI failure

<a id="fix-svelte-implied-semicolons-ci"></a>

### Fix Svelte implied-semicolon CI failure

**Identity:** SEED-073#fix-svelte-implied-semicolons-ci
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"b7c2c038ebdc99b15645eeab8322cbc68c7cd6c7f769af7891e3a4852bc28b24"}}
```

**Goal:** Master CI passes again: analyzing a `.svelte` file with nesting-depth
or nested-structures extensions no longer raises
`TypeError: ImpliedSemicolons.__init__() takes 1 positional argument but 2 were given`,
and existing Svelte ND/NS tests keep their expected nesting metrics when a
script end ends the last statement.

**Beneficiary:** Maintainers and CI consumers blocked on the red master build
after the shared implied-semicolon scan landed without updating the Svelte
reader.

**Scope:** Adapt `lizard_languages/svelte.py` to the post-#582
`implied_semicolons(tokens) -> (tokens, finder)` contract and the
`before_token` / `read` finder API, preserving the rule that a script end ends
its last statement. Existing failing tests in `test/test_languages/testSvelte.py`
(and the `.svelte` path in `testJavaScript.py`) are the proof boundary. No new
language features or CLI changes.

**Key examples:**

- Expected: `nix develop -c python -m pytest test/test_languages/testSvelte.py`
  passes, including script-end nesting cases that previously crashed.
- Actual on `d244838c` / CI run 45369247088: six failures with
  `ImpliedSemicolons.__init__() takes 1 positional argument but 2 were given`
  from `svelte._Statements.__init__` calling `super().__init__(context)`.

**Source:** Dashboard instruction to fix the CI build on master (launch
`67410972-d326-4921-ac71-2f7048298868`).
