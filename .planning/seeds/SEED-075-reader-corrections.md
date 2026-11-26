---
id: SEED-075
status: active
planted: 2026-11-24
---

# SEED-075 Reader corrections found in PR review

Source review [SEED-074](https://github.com/terryyin/lizard/blob/6f33c36786a1d85d49ca7b908964c512e6a64804/.planning/seeds/SEED-074-process-prs-past-24h.md#process-prs-past-24h) at
frozen master `6ff51711d14d7aebca1d72690130fab6e1889fe8`.

<a id="plsql-nonstrict-case"></a>

### Keep PL/SQL Boolean operator casing equivalent in nonstrict mode

**Identity:** SEED-075#plsql-nonstrict-case
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/044-plsql-nonstrict-case/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"77f5d8c9dfbf780dd44faed55f8fd75231e9749581a7113658d989aa8270bee0","plan":"545ec674ef4cd056701d68ec7a214286755f3204c37caad3b4f965259890236b"}}
```

**Goal:** PL/SQL users get the same CCN for uppercase, lowercase and mixed-case control flow in default and nonstrict modes.

**Scope:** PL/SQL condition keywords (`IF`, `ELSIF`, `WHEN`, `WHILE`, `FOR`) and Boolean operators (`AND`, `OR`) count the same in any letter case. With the nonstrict extension, Boolean operators stop counting in every casing. Follow [#645](https://github.com/terryyin/lizard/pull/645)'s direction of treating keyword case as insignificant, and correct its nonstrict result. Function names, parameters, labels and other identifiers keep their source spelling, and an identifier that contains a keyword, such as `If_And_Ready`, is not a condition. Preserve the reader's handling of `END IF`, `END LOOP` and `EXIT WHEN`. COBOL keeps the same uppercase and lowercase condition copies. COBOL, other language readers, and counting `NOT` in nonstrict mode are outside this story.

**Finding:** On #645 at `a86cc28766a7dbc86140715848b546434a8aa040`, otherwise identical `IF x AND y` and `If x And y` statements score 3 and 2 with the nonstrict extension. The reader keeps uppercase copies of the conditions, and nonstrict removes only the lowercase logical-operator category. Trunk has the same uppercase nonstrict result and does not count mixed-case keywords at all.

**Key examples:**

- Procedure `If_And_Ready(p_And NUMBER, y NUMBER)` has a labelled `LOOP` with `EXIT WHEN p_And > 9`, then `IF p_And > 0 AND y > 0 THEN ... ELSIF p_And < 0 OR y < 0 THEN ... END IF`, `END LOOP outer_loop`, and a `WHILE y > 0 AND NOT p_And = 1 LOOP ... END LOOP`. Written in uppercase, lowercase, or with capitalized keywords, it reports CCN 8 by default and 5 with nonstrict. The function name stays `If_And_Ready`.
- Trunk reports 8/8, 8/5 and 2/2 for those three casings. #645 reports 8/8, 8/5 and 8/5.
- A procedure with `FOR i IN 1..3 LOOP NULL; END LOOP;` and a `CASE` with two `WHEN` branches reports CCN 4 in every casing, with or without nonstrict.

**Plan:** [Keep PL/SQL Boolean operator casing equivalent in nonstrict mode](../slice-plans/044-plsql-nonstrict-case/PLAN.md)
