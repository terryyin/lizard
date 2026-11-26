---
id: SEED-075
status: active
planted: 2026-11-24
---

# SEED-075 Reader corrections found in PR review

Source review [SEED-074](https://github.com/terryyin/lizard/blob/6f33c36786a1d85d49ca7b908964c512e6a64804/.planning/seeds/SEED-074-process-prs-past-24h.md#process-prs-past-24h) at
frozen master `6ff51711d14d7aebca1d72690130fab6e1889fe8`.

<a id="r-formal-parameter-boundaries"></a>

### Count R variadic and quoted formals with nested defaults

**Identity:** SEED-075#r-formal-parameter-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/042-r-formal-parameter-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"e78db6151563775d44c06ac4acb44db04abf705eb7d845fcd76a89f729b1fb9f","plan":"707d9a6c1470362f804dc1b9bbf55374b2466efe02e9abc335c0ac1a7b502ac4"}}
```

**Goal:** R users get accurate parameter counts and readable signatures for functions with nested defaults, variadic parameters, and backtick-quoted names.

**Scope:** Follow [#643](https://github.com/terryyin/lizard/pull/643)'s balanced reading of calls, indexing, and braced expressions in defaults. Each top-level formal contributes one slot. Report the names in declaration order, with `...` as one entry and a backtick-quoted name returned as the text inside its backticks. Keep the quoted spelling and complete defaults in the displayed signature. Preserve empty and ordinary declarations, body CCN, and recognition of the following function. Raw-string support is owned by [the R raw literal story](#r-raw-literal-delimiters). Other language readers and new recovery behavior for malformed R are outside scope. [R Language Definition](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Function-definitions)

**Finding:** At `ecf44a70dd83a17fed5b47fca9f1fa5b5bc0fb90`, #643 collects both declarations in `function(a = list(1, 2), ...)`, but reports just `a`. It also omits a backtick-quoted formal with a nested default. When the quoted name contains a comma and `)`, #643 splits its text and closes the signature early.

**Key examples:**

- `function(a = list(1, 2), b = 3)` reports two parameters named `a` and `b`. The same declaration with `...` in place of `b = 3` reports `a` and `...`.
- ``function(a = {list(1, 2)[1]}, ..., `x, ) y` = list(1, 2))`` reports three parameters named `a`, `...`, and `x, ) y`. The long name contains both complete defaults and the written backticks. With one `if` in the body and `g <- function(z) { z }` on the next line, `f` has CCN 2 and `g` starts on line 2 with parameter `z`.
- `function()` reports zero parameters. `function(x, y = 2)` reports `x` and `y` and displays each signature token once.

**Plan:** [Count R variadic and quoted formals with nested defaults](../slice-plans/042-r-formal-parameter-boundaries/PLAN.md)


<a id="plsql-formal-parameter-boundaries"></a>

### Preserve quoted PL/SQL formals and their declarations

**Identity:** SEED-075#plsql-formal-parameter-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/043-plsql-formal-parameter-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"8aa674ca3d8e4e4ad20448b67c06044b3cc7b20382efbcafa7fbdcec87810b41","plan":"d86c1c7fe719615cea87e8b9cff2e5406c6f74c0cd965399647f7164c1db3e6c"}}
```

**Goal:** PL/SQL users get reliable parameter counts for procedures and functions. API callers can read each formal's declared name and full declaration even when the signature contains quoted names or nested defaults.

**Scope:** In a PL/SQL formal list, only commas at the outer level separate formals. Each formal contributes one count, its name in source spelling (including quotes for a quoted name), and its complete token-spaced declaration in `full_parameters`, with the mode, type, and default when present. Keep the declarations visible in `long_name` and keep the bodies and subsequent declarations discoverable at their source positions. The duplicated-parameter-list extension uses the resulting names under its existing policy. This story covers procedures and functions, including nested declarations. Package specifications, malformed-list recovery, and the separate nonstrict Boolean-casing story are outside its delivery scope. [Original proposal #644](https://github.com/terryyin/lizard/pull/644)

**Key examples:**

- Analyzing `PROCEDURE f("x name" IN NUMBER, b OUT VARCHAR2)` reports two names, `"x name"` and `b`, and the full declarations `"x name" IN NUMBER` and `b OUT VARCHAR2`.
- Analyzing a function with `a NUMBER DEFAULT GREATEST(ABS(-1), 2), b NUMBER` reports two formals and keeps the whole call in the declaration for `a`. The following procedure `g()` is discovered on its own line with zero parameters and an empty declaration list.
- Analyzing a nested procedure with `"a,)" IN NUMBER := GREATEST(1, 2), tail OUT VARCHAR2` reports two formals. The quoted punctuation and nested comma stay inside the first declaration. The enclosing and following procedures retain their own signatures and source boundaries.
- With the duplicate-list extension enabled, two files with the same five names match even if their modes, types, or defaults differ. A third file with a different quoted name has its own match. The occurrence and file counts are both `2, 2, 1`.

**Plan:** [Preserve quoted PL/SQL formals and their declarations](../slice-plans/043-plsql-formal-parameter-boundaries/PLAN.md)


<a id="plsql-nonstrict-case"></a>

### Keep PL/SQL Boolean operator casing equivalent in nonstrict mode

**Identity:** SEED-075#plsql-nonstrict-case
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/044-plsql-nonstrict-case/PLAN.md"}
```

**Goal:** PL/SQL users get the same CCN for uppercase, lowercase and mixed-case control flow in default and nonstrict modes.

**Scope:** Correct the specific language reader behavior reproduced during review of [#645](https://github.com/terryyin/lizard/pull/645) at `a86cc28766a7dbc86140715848b546434a8aa040`.

**Finding:** On #645, otherwise identical IF x AND y and If x And y statements score 3 and 2 with the nonstrict extension. The reader's uppercase copies escape removal of the logical-operator category.

**Plan:** [Keep PL/SQL Boolean operator casing equivalent in nonstrict mode](../slice-plans/044-plsql-nonstrict-case/PLAN.md)
