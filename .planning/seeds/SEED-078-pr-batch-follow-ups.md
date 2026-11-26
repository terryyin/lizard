---
id: SEED-078
status: active
planted: 2026-11-26
---

# SEED-078 Follow-ups from the SEED-077 PR and issue batch

Source review: SEED-077#process-prs-past-48h at frozen master `f7a11e5248a388c1cee706b5aa0d2f234837c44d`.

<a id="nd-without-logical-operators"></a>

### Measure ND from control structures only

**Identity:** SEED-078#nd-without-logical-operators
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/048-nd-without-logical-operators/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"f1c6adc0e8adf7ef4080fa738de67848bab31919e70b7d130e03b1471f483576","plan":"da5122d6b84d773d0da3156a639736780176a570dc7a620ce74d655034a902bd"}}
```

**Beneficiary:** Users who gate code on nesting depth (`-END`), including the reporter of [#693](https://github.com/terryyin/lizard/issues/693) using it in a safety-critical pipeline.

**Goal:** ND reports only the nesting of control structures, as [ADR 0001](../../docs/adrs/0001-ubiquitous-language-accepted.md) describes, so `&&`/`||` in a condition no longer adds a level.

**Scope:** The developer approved this as a deliberate metric change on 2026-11-26.

- Required behavior: `&&` and `||` never open an ND level, inside or outside a condition, for every language whose nesting depth counts them today (the default C-family set and Groovy's own set). Control structures and their `else if` handling keep their current ND.
- Update the tests that pin the old behavior.
- Note the change in the release notes, because existing ND values and `-N` thresholds drop.
- Scope limits: CCN, NS (`-ENS`) and cognitive complexity are unchanged. The ternary `?`/`?:` is not a logical operator and keeps its current ND contribution. Python's `and`/`or` already add no ND.

**Key examples:**

- From #693, in C: `plain` (an `if (a)` inside a `for`) and `logical` (the same with `if (a && b)`) both report ND 2. `logical` reports more today (3 for `for(;;){ if (a && b) { x(); } }`).
- `int c = a && b;` alone in a function reports ND 0. It reports 1 today.
- `if (a || b && c)` inside a loop reports the same ND as `if (a)` there.
- The same functions report the same NS with `-ENS` before and after the change.

<a id="cognitive-ternary-in-control-headers"></a>

### Count ternaries in control headers for all brace languages

**Identity:** SEED-078#cognitive-ternary-in-control-headers
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/049-cognitive-ternary-in-control-headers/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"a60c3aa43b7b973c9a716576681f57156353570078c81f79fdc8b74ce882db4b","plan":"a7d0075eb2d337f3613e676b5732b4e8f5e9690881f950f67c8c2f98a5eb609e"}}
```

**Beneficiary:** Users of `-Ecognitive` on brace languages, including JavaScript, TypeScript, JSX, Vue, C, Java and C#.

**Goal:** A ternary in a control header adds its cognitive increment at that control structure's nesting level, in every brace language that already counts ternaries.

**Scope:** The header consumes `?` before a ternary is counted, so the increment is missing. [#684](https://github.com/terryyin/lizard/pull/684) restores it only for plain JavaScript. It belongs to every brace language whose cognitive profile counts a ternary, parenthesized headers and brace headers alike. Supersede #684 when this lands.

The structure's nesting penalty starts at its body, so a header ternary is not nested under that structure. A ternary inside it still takes the usual nesting increment. The same rule covers every control header the brace counter already tracks, including `else if`, `switch` and `catch`. `if`, `for` and `while` are the cases this story promises.

In a `for` header, `;` between the three clauses makes ternaries siblings. A `;` or `,` nested inside braces, an object, a class body, a template interpolation, or a JSX attribute expression does not end the enclosing header ternary. [#684](https://github.com/terryyin/lizard/pull/684)'s `.js` tests keep the scores they assert.

This story does not change which `?` is a ternary. Optional chaining, nullish coalescing, `?:`, nullable types and Java wildcards stay as they are. A TypeScript conditional type already adds one point outside a header, and in a header it adds that same point.

Scope limits: CCN, ND, NS, and which functions are reported. Languages that do not count ternaries. The regex-`?`-after-a-comment miscount, owned by [#687](https://github.com/terryyin/lizard/pull/687), which already happens outside headers. Structures already ignored before the function body, such as default arguments. Retuning a ternary that is already counted outside a header.

**Key examples:**

- `if (a ? b : 0) {}` followed by one ternary in a `for` header scores 4, not 2, in `.js`, `.ts`, `.jsx`, `.vue`, `.c`, `.java` and `.cs`. It scores 2 today.
- That `if (b ? 1 : 0)` nested inside another `if` scores 5, not 3. The outer `if` is +1, the inner `if` is +2, and the header ternary is +2.
- `if (options ? (options.exact ? options.enabled : fallback) : fallback)` scores 4: the `if` and the outer ternary are +1 each, and the inner ternary is +2.
- `for (let i = urgent ? 0 : 1; i < (urgent ? 10 : 5); i++)` scores 3. The two ternaries are siblings. It scores 1 today.
- An `if` whose condition is `check(prefix ? template : useDefault ? fallback : route)`, with the template `` `${prefix}/${route}` ``, scores 4. The interpolation does not end the outer ternary.
- `if (layoutFits(visible ? <Card title={title} /> : compact ? <Compact /> : null))` in `.jsx` scores 4, not 1.
- `if a ? 1 : 0 { }` in Swift scores 2, not 1. `a ? true : false` stays uncounted, as it already is in a return.
- `if (a?.b)` stays 1. `if (input as (T extends string ? boolean : number))` scores 2: the `if`, plus the point that cast already adds in a return.
