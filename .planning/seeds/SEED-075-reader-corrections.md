---
id: SEED-075
status: active
planted: 2026-11-24
---

# SEED-075 Reader corrections found in PR review

Source review [SEED-074](https://github.com/terryyin/lizard/blob/6f33c36786a1d85d49ca7b908964c512e6a64804/.planning/seeds/SEED-074-process-prs-past-24h.md#process-prs-past-24h) at
frozen master `6ff51711d14d7aebca1d72690130fab6e1889fe8`.

<a id="source-documentation-directives"></a>

### Keep Perl and Ruby documentation from acting as Lizard directives

**Identity:** SEED-075#source-documentation-directives
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/037-source-documentation-directives/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"e7a52546cfd11df35302a668517242fc6b31026bda2c60d015577f2a4b236f93","plan":"f05b561eac0c25f579086939b1dff08e54565d73a63c8f8bdd0e8e05bd2a8828"}}
```

**Goal:** Perl and Ruby users get metrics for executable code only: embedded documentation and trailing data sections add no phantom functions, decisions or code lines, and text in them that resembles an analyzer directive never hides, ends or forgives the real code around it.

**Scope:** Deliver the intent of [#636](https://github.com/terryyin/lizard/pull/636) at `7e89c51a83bdc2254ac832872e01acf5d97c4430` and [#642](https://github.com/terryyin/lizard/pull/642) at `0bf972c66cce3e4b6cb44d8fa450ca2b5a303ddb`, correcting the behavior reproduced in their review.

- Required: Perl POD (a column-zero `=word` command through `=cut`, or to end of file when unterminated) and text after a line-level `__DATA__` or `__END__` are documentation or data, not code. Ruby `=begin`…`=end` blocks and text after a column-zero `__END__` line are the same. Following code keeps its original line numbers.
- Required: Lizard directives (`GENERATED CODE`, `#lizard forgive…`) take effect only from actual language comments, as they do today. Documentation and data text carries no directive.
- Limits: Perl and Ruby only. No new directive syntax and no change to how actual comments are recognized. Heredocs and other string forms stay as they are.
- Assumption: Identifiers that merely start with `__DATA__` or `__END__` are ordinary code.

**Finding:** With GENERATED CODE between a Perl =head1 and =cut or a Ruby =begin and =end, the proposed reader stops before the later executable function. The same sources retain that function on master. Both proposals route opaque documentation through the shared comment directive handler.

**Key examples:**

- Perl `=head1 NOTES` / `GENERATED CODE` / `=cut`, then `sub real { if ($x) { 1 } }` → `real` is reported at its source line with CCN 2. (#636 stops before it.)
- Ruby `=begin` / `GENERATED CODE` / `=end`, then a `def real` containing an `if` → `real` is reported at its line with CCN 2. (#642 stops before it.)
- A POD or `=begin` section containing `#lizard forgive`, followed by a function over the CCN threshold → that function still warns.
- `=pod` / `sub fake { if ($x) {} }` / `=cut` / `sub real { return 1; }` → only `real`, starting at line 4, with file NLOC 1. The same holds for `=head1` and `=over`, for unterminated POD, and for `sub fake` after `__DATA__` or `__END__`.
- Ruby `=begin example` with a fake `def` inside, or a fake `def` after `__END__` → only the real method. `=begin` / `if fake` / `=end` inside a method adds no decision.
- An actual `# GENERATED CODE` comment or `#lizard forgive` comment in Perl or Ruby source → it behaves as on master.
- `sub __DATA__value { 1; }` in Perl, or `__END__value` inside a Ruby method → ordinary code.

**Plan:** [Keep Perl and Ruby documentation from acting as Lizard directives](../slice-plans/037-source-documentation-directives/PLAN.md)


<a id="perl-word-operator-context"></a>

### Count Perl word operators only in operator positions

**Identity:** SEED-075#perl-word-operator-context
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/038-perl-word-operator-context/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"42baa1ed1b0963bbcff9173a951848ec533940e9ce92f78e814dcc6f448f600f","plan":"3c52c772ab4cfdb3c9972625e28764aeb652249aaabad113942bd89b0854162a"}}
```

**Goal:** Perl users get the extra CCN for short-circuit `and` / `or` while calls named `and` or `or` keep their original CCN.

**Scope:** Correct the specific language reader behavior reproduced during review of [#634](https://github.com/terryyin/lizard/pull/634) at `3ab95dca71d145de86dd79ba264644c096a926cf`. Count word-form short-circuit `and` and `or` as decisions when they are operators. Do not count those words when they name a method after `->` or a package-qualified call after `::`. Preserve symbol-form `&&` / `||`, quoted text that contains the words, nonstrict removal of logical-operator decisions, and unrelated reader behavior. Follow #634's direction for which word forms to add. Out of scope: other SEED-075 reader corrections, non-Perl languages, and Perl's `not` / `xor`.

**Finding:** The valid subroutine `sub f { return $obj->and(); }` changes from CCN 1 to 2 on #634. Both real word operators and an actual method name reach the same condition set. Master leaves both the method call and real `$a and $b` at CCN 1.

**Key examples:**

- Method call: `sub f { return $obj->and(); }` → CCN 1 (master keeps 1; #634 reports 2).
- Word-form operators: `sub check { return $a and $b; }` → CCN 2; the same shape with `or` → CCN 2; `$a and $b or $c` → CCN 3.
- Package-qualified call: `sub f { return Package::and(); }` → CCN 1.
- Quoted words: `sub check { return "and or"; }` → CCN 1.
- Symbol forms unchanged: `sub check { return $a && $b; }` and `$a || $b` → CCN 2.
- Nonstrict mode: a real `$a and $b` loses the extra logical-operator decision; a method named `and` stays CCN 1.

**Plan:** [Count Perl word operators only in operator positions](../slice-plans/038-perl-word-operator-context/PLAN.md)


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
