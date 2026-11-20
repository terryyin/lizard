---
id: SEED-068
status: active
planted: 2026-11-20
planted_during: SEED-066 wrap-up
scope: JavaScript reader — parenthesized assignment without a semicolon hides the next statement
---

# SEED-068: Parenthesized assignment without `;` hides the next `if`

<a id="js-asi-parenthesized-assignment"></a>

### Parenthesized assignment without `;` hides the next `if`

**Identity:** SEED-068#js-asi-parenthesized-assignment
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/035-js-asi-parenthesized-assignment/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"39533d61a0797ce2a7729187da6b91e5e393935553ba549a3bee5bfa5dc0b666","plan":"d1112fd26ea989070e556877a5b51643e985db5b562d5125708bf5801fae8339"}}
```

**Goal:** In JavaScript/TypeScript without semicolons, an assignment of a parenthesized expression ends at its line break, so a structure keyword (`if`, `while`, …) starting the next line counts toward the enclosing function's nesting depth (ND) exactly as it does after `;`.

**Beneficiary:** Users analyzing no-semicolon JavaScript/TypeScript (e.g. `js-yaml`) whose ND is one level too low for functions where such an assignment is directly followed by a nested structure. Contributes to matching ND between semicolon and no-semicolon styles.

**Scope:**

- Required: when `name = (…)` (plain or member target, `.js` and `.ts`) ends at a line break with no `=>` and the next line starts with a structure keyword, that keyword's ND level belongs to the enclosing function; CCN, NS and token counts stay as today apart from the missing `;` token.
- Preserved: arrow functions whose parameter list spans lines, `x = (a,\n b) => …`, and typed arrows `x = (a): T => …` remain functions. A line break before `=>` cannot occur in valid JavaScript, so it needs no handling.
- Deferred: other ASI discrepancies (SEED-069 covers the shared scan's cost, not this behavior).

**Key examples:**

- Minimal: `function f(m) {` / `r = (m < 2)` / `if (r) w()` / `}` reports ND 1, the same as with `r = (m < 2);` (today 0 vs 1). CCN 2 and NS 1 unchanged.
- Nested: `r = (m < 2)` followed by `if (r) { if (m) { w() } }` reports ND 2 (today 1).
- Other structure keyword: `r = (m < 2)` followed by `while (r) w()` reports ND 1 (today 0); the same `.ts` source reports ND 1.
- js-yaml shape: `handleYamlDirective`, whose `state.checkLineBreaks = (minor < 2)` is followed by `if (minor !== 1 && minor !== 2) { … }`, reports ND 2 like its semicolon version (today 1); CCN 7 unchanged.
- Unaffected today and after: a plain statement between, `r = (m < 2)` / `w()` / `if (r) w()`, already reports ND 1.

**Source:** Reported in PR #578's description (one of 3 remaining ND mismatches across 11 no-semicolon npm packages); deferred from SEED-066 on 2026-11-20.
