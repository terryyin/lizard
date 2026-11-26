---
id: SEED-075
status: active
planted: 2026-11-24
---

# SEED-075 Reader corrections found in PR review

Source review [SEED-074](https://github.com/terryyin/lizard/blob/6f33c36786a1d85d49ca7b908964c512e6a64804/.planning/seeds/SEED-074-process-prs-past-24h.md#process-prs-past-24h) at
frozen master `6ff51711d14d7aebca1d72690130fab6e1889fe8`.

<a id="kotlin-adjacent-nested-comments"></a>

### Resume Kotlin tokenization after each complete block comment

**Identity:** SEED-075#kotlin-adjacent-nested-comments
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/039-kotlin-adjacent-nested-comments/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"6442bac3c44d29c69c5f1ded186902878caa1ad18dfe20771669b45d174dc5b3","plan":"9b7bd0708e0934516ed5152bd8406503aceff9dc2b30d928afe20370e3c15dc3"}}
```

**Goal:** Kotlin users get accurate function metrics when consecutive nested comments contain quotes or apparent executable code.

**Scope:** Correct the specific language reader behavior reproduced during review of [#647](https://github.com/terryyin/lizard/pull/647) at `a30a5c63f1e7b2e2b2bdc8cf7dbf282beeceb2f1`. Follow #647's direction: Kotlin block comments nest, and the text a whole comment covers adds no tokens. Resume scanning for strings, line comments and comments right after each complete block comment, so a quote or backtick inside one comment cannot hide the next comment's opener. Preserve #647's nested-comment and comment-marker-in-literal behavior and unrelated reader behavior. Out of scope: other SEED-075 reader corrections, non-Kotlin languages, Kotlin string templates, and unterminated block comments.

**Finding:** Put `/* " */` before `/* outer /* inner */ if (hidden) {} */` followed by `val s = "x"` and one real if. #647 still reports CCN 3 instead of 2. A trivia match that starts inside an already consumed comment can skip the following nested comment. Master reports CCN 3 for the same input because it lacks nested-comment support.

**Key examples:**

- Adjacent comments: `fun f() {` / `/* " */` / `/* outer /* inner */ if (hidden) {} */` / `val s = "x"` / `if (real) {}` / `}` / `fun g() {}` → f has CCN 2 and NLOC 4; g starts at line 7 with CCN 1; file NLOC 5. (#647 and master: f CCN 3.)
- The same shape with `"""` or `'` inside the first comment and a following raw string or character literal → f CCN 2.
- A backtick inside the first comment and a backtick identifier after the nested comment → f CCN 2 (already true on #647).
- #647's nested cases, `/* outer /* inner */ if (fake) {} */` and deeper or multi-line nesting, before a real if → f CCN 2, and the following function keeps its start line.
- `/*` inside an ordinary string, raw string, backtick identifier or line comment opens no comment (#647's literal cases).

**Plan:** [Resume Kotlin tokenization after each complete block comment](../slice-plans/039-kotlin-adjacent-nested-comments/PLAN.md)


<a id="r-raw-literal-delimiters"></a>

### Keep valid R raw-string delimiter forms opaque

**Identity:** SEED-075#r-raw-literal-delimiters
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/040-r-raw-literal-delimiters/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"eea5bb40b2a74bb0bab22cdf47123566949abd261ab72f7a9fa21c81f55f4f38","plan":"20ab9b96cdbddcd2f97635e913018cd04388478409dd7d8c17fe58499f92a24e"}}
```

**Goal:** R users get correct CCN and following function locations for raw literals with either quote style and supported delimiters.

**Scope:** Correct the specific language reader behavior reproduced during review of [#638](https://github.com/terryyin/lizard/pull/638) at `ad503916d19b37ca0eedf208f35dbbda589dcc71`. Follow #638's direction: an R raw string is one token, so its text adds no decisions and its newlines keep following functions on their real lines. Make that hold for every form the R lexer accepts: an `r` or `R` prefix, a double or single quote that must match at the close, zero or more dashes, and a `(`, `[`, `{` or `|` delimiter that closes with its counterpart, the same dashes and the same quote. Preserve #638's double-quote bracket, brace, dash and multiline behavior, ordinary strings, identifiers named `r`, and unrelated reader behavior. Out of scope: other SEED-075 reader corrections, non-R languages, and unterminated raw strings.

**Finding:** A valid single-quoted `r'(some ' if (hidden) {} ' text)'` adds a false decision on master and #638. The R lexer accepts both quote characters. The current R lexer also supports a vertical-bar delimiter and #638 leaves that form unchanged. [Language reference](https://raw.githubusercontent.com/wch/r-source/trunk/src/main/gram.y).

**Key examples:**

- Each source is `f <- function() { x <- <raw>; if (a) {} }` then `g <- function() { 1 }` on the next line, with `<raw>` hiding an `if (x) {}` behind an embedded quote. Expected: f CCN 2 and g at line 2 with CCN 1.
  - `r"(text " if (x) {} " end)"` → #638 passes; master f CCN 3.
  - `r'(some ' if (hidden) {} ' text)'` → master and #638 f CCN 3.
  - `R'[some ' if (hidden) {} ' text]'` and `r'--{some ' if (hidden) {} ' text}--'` → master and #638 f CCN 3.
  - `r"|text " if (x) {} " end|"`, `r'|some ' if (hidden) {} ' text|'` and `r"--|text " if (x) {} " end|--"` → master and #638 f CCN 3.
- A single-quoted multiline raw string `r'-(line` / `' if (x) {} '` / `last)-'` followed by `; if (a) {} }` and `g` → f CCN 2 and g starts at line 4 (master and #638: f CCN 3).
- A close that lacks the opening dashes or quote ends nothing: `r"-(a)" if (x) {} )-"` is one raw string, f CCN 2 (#638 passes; master f CCN 3).
- An ordinary string such as `"r\"("` and an identifier `r` used in `if (r)` open no raw string: f CCN 2.

**Plan:** [Keep valid R raw-string delimiter forms opaque](../slice-plans/040-r-raw-literal-delimiters/PLAN.md)


<a id="plsql-national-literals"></a>

### Keep national-character PL/SQL alternative literals opaque

**Identity:** SEED-075#plsql-national-literals
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/041-plsql-national-literals/PLAN.md"}
```

**Goal:** PL/SQL users get the same metrics for Q and NQ literals containing apparent executable code.

**Scope:** Correct the specific language reader behavior reproduced during review of [#639](https://github.com/terryyin/lizard/pull/639) at `887ad0bff86a05c71eca26d9eb6eba6a0b7dde4f`.

**Finding:** In a function returning `nq'[it's if x then end if;]'`, #639 reports CCN 2. The equivalent Q form passes and both prefixes are valid Oracle syntax. [Language reference](https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/Literals.html).

**Plan:** [Keep national-character PL/SQL alternative literals opaque](../slice-plans/041-plsql-national-literals/PLAN.md)


<a id="r-formal-parameter-boundaries"></a>

### Count R variadic and quoted formals with nested defaults

**Identity:** SEED-075#r-formal-parameter-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/042-r-formal-parameter-boundaries/PLAN.md"}
```

**Goal:** R users get the number of formal parameter slots and readable names when defaults contain nested expressions.

**Scope:** Correct the specific language reader behavior reproduced during review of [#643](https://github.com/terryyin/lizard/pull/643) at `ecf44a70dd83a17fed5b47fca9f1fa5b5bc0fb90`.

**Finding:** For `function(a = list(1, 2), ...)`, #643 reports one parameter. The public full-parameter strings contain two slots but the common name extraction ignores the ellipsis. A quoted formal paired with a nested default is also omitted by #643.

**Plan:** [Count R variadic and quoted formals with nested defaults](../slice-plans/042-r-formal-parameter-boundaries/PLAN.md)


<a id="plsql-formal-parameter-boundaries"></a>

### Preserve quoted PL/SQL formals and their declarations

**Identity:** SEED-075#plsql-formal-parameter-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/043-plsql-formal-parameter-boundaries/PLAN.md"}
```

**Goal:** PL/SQL users get correct formal parameter counts around nested defaults without losing quoted names or the full parameter declaration API.

**Scope:** Correct the specific language reader behavior reproduced during review of [#644](https://github.com/terryyin/lizard/pull/644) at `bb2bf6b13c0fbbe36772d07ebec341ad19205478`.

**Finding:** For `f("x name" IN NUMBER, b OUT VARCHAR2)`, master counts 2 and #644 counts 1. #644 also strips the type and mode from full_parameters on ordinary typed parameters.

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
