---
id: SEED-075
status: active
planted: 2026-11-24
---

# SEED-075 Reader corrections found in PR review

Source review [SEED-074](SEED-074-process-prs-past-24h.md#process-prs-past-24h) at
frozen master `6ff51711d14d7aebca1d72690130fab6e1889fe8`.

<a id="ruby-percent-literal-context"></a>

### Preserve Ruby modulo alongside percent literals

**Identity:** SEED-075#ruby-percent-literal-context
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/036-ruby-percent-literal-context/PLAN.md"}
```

**Goal:** Ruby users can count decisions after modulo or modulo assignment while literal punctuation still protects text.

**Scope:** Correct the specific language reader behavior reproduced during review of [#646](https://github.com/terryyin/lizard/pull/646) at `50e9f92334eede41a4287b8ab12ae90cda23bb27`.

**Finding:** On #646, `def f(a); a%=2; if a==0 then a else 0 end; end` reports CCN 1. Master reports 2 and Ruby accepts the source. The broad delimiter rule consumes the assignment and part of the condition as a percent literal.

**Plan:** [Preserve Ruby modulo alongside percent literals](../slice-plans/036-ruby-percent-literal-context/PLAN.md)


<a id="source-documentation-directives"></a>

### Keep Perl and Ruby documentation from acting as Lizard directives

**Identity:** SEED-075#source-documentation-directives
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/037-source-documentation-directives/PLAN.md"}
```

**Goal:** Perl and Ruby users keep correct function lists and source locations when embedded documentation contains text that resembles analyzer directives.

**Scope:** Correct the specific language reader behavior reproduced during review of [#636](https://github.com/terryyin/lizard/pull/636) at `7e89c51a83bdc2254ac832872e01acf5d97c4430`, [#642](https://github.com/terryyin/lizard/pull/642) at `0bf972c66cce3e4b6cb44d8fa450ca2b5a303ddb`.

**Finding:** With GENERATED CODE between a Perl =head1 and =cut or a Ruby =begin and =end, the proposed reader stops before the later executable function. The same sources retain that function on master. Both proposals route opaque documentation through the shared comment directive handler.

**Plan:** [Keep Perl and Ruby documentation from acting as Lizard directives](../slice-plans/037-source-documentation-directives/PLAN.md)


<a id="perl-word-operator-context"></a>

### Count Perl word operators only in operator positions

**Identity:** SEED-075#perl-word-operator-context
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/038-perl-word-operator-context/PLAN.md"}
```

**Goal:** Perl users get the extra CCN for short-circuit logic while calls named `and` or `or` keep their original CCN.

**Scope:** Correct the specific language reader behavior reproduced during review of [#634](https://github.com/terryyin/lizard/pull/634) at `3ab95dca71d145de86dd79ba264644c096a926cf`.

**Finding:** The valid subroutine `sub f { return $obj->and(); }` changes from CCN 1 to 2 on #634. Both real word operators and an actual method name reach the same condition set.

**Plan:** [Count Perl word operators only in operator positions](../slice-plans/038-perl-word-operator-context/PLAN.md)


<a id="kotlin-adjacent-nested-comments"></a>

### Resume Kotlin tokenization after each complete block comment

**Identity:** SEED-075#kotlin-adjacent-nested-comments
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/039-kotlin-adjacent-nested-comments/PLAN.md"}
```

**Goal:** Kotlin users get accurate function metrics when consecutive nested comments contain quotes or apparent executable code.

**Scope:** Correct the specific language reader behavior reproduced during review of [#647](https://github.com/terryyin/lizard/pull/647) at `a30a5c63f1e7b2e2b2bdc8cf7dbf282beeceb2f1`.

**Finding:** Put `/* " */` before `/* outer /* inner */ if (hidden) {} */` followed by `val s = "x"` and one real if. #647 still reports CCN 3 instead of 2. A trivia match that starts inside an already consumed comment can skip the following nested comment.

**Plan:** [Resume Kotlin tokenization after each complete block comment](../slice-plans/039-kotlin-adjacent-nested-comments/PLAN.md)


<a id="r-raw-literal-delimiters"></a>

### Keep valid R raw-string delimiter forms opaque

**Identity:** SEED-075#r-raw-literal-delimiters
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/040-r-raw-literal-delimiters/PLAN.md"}
```

**Goal:** R users get correct CCN and following function locations for raw literals with either quote style and supported delimiters.

**Scope:** Correct the specific language reader behavior reproduced during review of [#638](https://github.com/terryyin/lizard/pull/638) at `ad503916d19b37ca0eedf208f35dbbda589dcc71`.

**Finding:** A valid single-quoted `r'(some ' if (hidden) {} ' text)'` adds a false decision on master and #638. The R lexer accepts both quote characters. The current R lexer also supports a vertical-bar delimiter and #638 leaves that form unchanged. [Language reference](https://raw.githubusercontent.com/wch/r-source/trunk/src/main/gram.y).

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
