---
id: SEED-074
status: active
planted: 2026-11-24
---

# SEED-074 Process open PRs from the past 24 hours

<a id="process-prs-past-24h"></a>

### Process open PRs from the past 24 hours

**Identity:** SEED-074#process-prs-past-24h
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"unselected"}
```

**Beneficiary:** Lizard maintainers triaging contributions and users affected by inaccurate analyzer metrics.

**Goal:** Give each qualifying open PR a disposition supported by evidence against current master. Land contributions that meet the project's scope, correctness, performance and CI requirements.

**Scope:** GitHub pull requests open at intake and created or updated from 2026-11-23 14:11:53 UTC through 2026-11-24 14:11:53 UTC. Twelve already merged PRs in the same window are historical dispositions and require no delivery work. Initial master is 6ff51711d14d7aebca1d72690130fab6e1889fe8.

**Key examples:** A supported language construct reports its intended metrics, and following ordinary functions retain their metrics. A redundant proposal is identified by checking its reproducer on master. Contributions with regressions or unmet qualification gates are held with a specific reason.

**Source:** Dashboard launch 5ee20d6d-3522-4f9c-a8e5-d5b86034fbaf.

#### Pull requests

- [#647](https://github.com/terryyin/lizard/pull/647) Keep nested Kotlin block comments out of executable metrics
- [#646](https://github.com/terryyin/lizard/pull/646) Recognize Ruby percent literals with arbitrary punctuation delimiters
- [#645](https://github.com/terryyin/lizard/pull/645) Count mixed-case PL/SQL condition keywords
- [#644](https://github.com/terryyin/lizard/pull/644) Keep PL/SQL default calls within their formal parameter
- [#643](https://github.com/terryyin/lizard/pull/643) Count R formal parameters around nested default expressions
- [#642](https://github.com/terryyin/lizard/pull/642) Exclude Ruby embedded documentation and data sections from metrics
- [#641](https://github.com/terryyin/lizard/pull/641) Recognize R shorthand anonymous function syntax
- [#640](https://github.com/terryyin/lizard/pull/640) Include TTCN-3 altstep bodies in function metrics
- [#639](https://github.com/terryyin/lizard/pull/639) Keep PL/SQL alternative-quoted literals opaque
- [#638](https://github.com/terryyin/lizard/pull/638) Recognize R raw string delimiters during tokenization
- [#637](https://github.com/terryyin/lizard/pull/637) Keep Kotlin multiline string contents out of code metrics
- [#636](https://github.com/terryyin/lizard/pull/636) Exclude Perl POD and data sections from source metrics
- [#635](https://github.com/terryyin/lizard/pull/635) Measure Solidity constructors and special entry points
- [#634](https://github.com/terryyin/lizard/pull/634) Count Perl word-form short-circuit operators
- [#633](https://github.com/terryyin/lizard/pull/633) Keep Structured Text functions open after variable declarations
- [#632](https://github.com/terryyin/lizard/pull/632) Respect case and word boundaries in Structured Text condition tokens
- [#631](https://github.com/terryyin/lizard/pull/631) Read Structured Text block comments through their closing delimiter
- [#630](https://github.com/terryyin/lizard/pull/630) Count Erlang short-circuit Boolean operators
- [#629](https://github.com/terryyin/lizard/pull/629) Recognize quoted Erlang atoms as function names
- [#628](https://github.com/terryyin/lizard/pull/628) Exclude single-percent Erlang comments from code metrics
- [#627](https://github.com/terryyin/lizard/pull/627) Treat backslashes literally in Fortran character strings
- [#626](https://github.com/terryyin/lizard/pull/626) Count GDScript word-form Boolean operators
- [#625](https://github.com/terryyin/lizard/pull/625) Respect raw and escaped Dart string delimiters
- [#624](https://github.com/terryyin/lizard/pull/624) Recognize C# verbatim and raw string boundaries
- [#623](https://github.com/terryyin/lizard/pull/623) Match PHP heredoc and nowdoc labels at line boundaries
- [#622](https://github.com/terryyin/lizard/pull/622) Keep Java text block contents out of complexity metrics
- [#621](https://github.com/terryyin/lizard/pull/621) Recognize Zig multiline string lines during tokenization
- [#620](https://github.com/terryyin/lizard/pull/620) Ignore control keywords inside R backtick identifiers
- [#619](https://github.com/terryyin/lizard/pull/619) Recognize custom delimiters in C++ raw string literals
- [#618](https://github.com/terryyin/lizard/pull/618) Recognize Kotlin backtick names containing spaces
- [#617](https://github.com/terryyin/lizard/pull/617) Tokenize Scala multiline string literals as a whole
- [#616](https://github.com/terryyin/lizard/pull/616) Keep Rust raw string contents out of complexity metrics
- [#615](https://github.com/terryyin/lizard/pull/615) Keep Lua length and integer-division operators in the token stream
- [#614](https://github.com/terryyin/lizard/pull/614) Match Lua long-string and comment bracket levels
- [#613](https://github.com/terryyin/lizard/pull/613) Restore Svelte nesting extensions after the statement reader API change

#### Intake and qualification

The 35 proposals address maintenance, metric defects or small extensions to
existing language readers. None introduces a public option or API signature.
GitHub had no comments, submitted reviews or inline threads on the frozen open
heads at intake.

| PR | Evidence and disposition |
| --- | --- |
| [#613](https://github.com/terryyin/lizard/pull/613) | Already fixed by `834313ae591cdf4553aa97cd113db4e88d5f7b09`. All three submitted analyzer tests pass against `6ff51711d14d7aebca1d72690130fab6e1889fe8`, covering ND, NS and both together at a Svelte script-to-markup boundary. Keep the existing shared statement reader integration. |
| [#614–#627](https://github.com/terryyin/lizard/pulls?q=is%3Apr+author%3AAschenbath) | The failures in the original GitHub matrices reproduced the Svelte constructor error. The exact-head branch updates were requested to pick up the master fix and obtain fresh matrices. |
| #614–#647 | Each proposal has at least one authored analyzer assertion that fails on the frozen trunk. The authored regressions pass when each proposal is applied independently to that trunk. |

Twelve PRs in the intake window were already merged before this session:
#582, #589, #590, #593, #594, #597, #599, #608, #609, #610, #611 and #612.

#### Qualification results

GitHub accepted 14 branch updates guarded by their original head SHAs.
The refreshed heads each preserve the proposal and add only the then-current master.
14 of the refreshed heads have finished the applicable green matrix.

A combined compatibility rehearsal of 18 proposals passed full pytest with
3,789 passing and seven skipped. Coverage runs exceeded the existing Svelte
wall-clock assertions. The combined results do not qualify individual
proposals for performance.

The twelve-proposal follow-up included ordinary sources as well as the
behavior-change cases. The 95% interval was −12.47% to +5.20%
for affected API workloads and −3.74% to +9.23% for the broad
CLI. Both intervals crossed the paired runner's default screening bound.
The focused #626/#630 pair produced −7.46% to +1.63% for the affected API
and −2.91% to +6.51% for the broad CLI. The paired runner reported
a pass for the API and
an unresolved result for the CLI.

| PR | Disposition | Review observation |
| --- | --- | --- |
| [#614](https://github.com/terryyin/lizard/pull/614) | Pending affected-workload and CLI performance qualification. | Long Lua comments and strings honor the matching bracket level. The following function stays visible. Shares tokenizer code with #615. |
| [#615](https://github.com/terryyin/lizard/pull/615) | Pending affected-workload and CLI performance qualification. | Length and floor division preserve loop-body decisions and the subsequent function. Shares tokenizer code with #614. |
| [#616](https://github.com/terryyin/lizard/pull/616) | Pending affected-workload and CLI performance qualification. | Rust raw C strings stay opaque beside nested comments and a real decision. Raw byte and ordinary raw string tests pass. |
| [#617](https://github.com/terryyin/lizard/pull/617) | Pending affected-workload and CLI performance qualification. | Scala triple-quoted text retains the next expression-bodied function. Timing on ordinary Scala and recovered code needs verification. |
| [#618](https://github.com/terryyin/lizard/pull/618) | Pending affected-workload and CLI performance qualification. | A Kotlin extension function with a backtick-quoted name retains its real CCN and following function. A combined rehearsal conflicts with #637. |
| [#619](https://github.com/terryyin/lizard/pull/619) | Pending affected-workload and CLI performance qualification. | Custom and empty C++ raw delimiters pass. The ordinary C control retains its metrics. The shared C-like tokenizer needs affected-consumer timing. |
| [#620](https://github.com/terryyin/lizard/pull/620) | Pending affected-workload and CLI performance qualification. | Control words inside R backticks do not affect CCN. A later control and function remain visible. |
| [#621](https://github.com/terryyin/lizard/pull/621) | Pending affected-workload and CLI performance qualification. | Zig multiline string lines remain in the source span without leaking decisions. |
| [#622](https://github.com/terryyin/lizard/pull/622) | Pending affected-workload and CLI performance qualification. | A Java text block with an escaped delimiter and CRLF preserves real code and the following method. |
| [#623](https://github.com/terryyin/lizard/pull/623) | Pending affected-workload and CLI performance qualification. | An indented PHP heredoc followed by a comma keeps the surrounding code, including a later real decision. The baseline omitted both functions in that case. |
| [#624](https://github.com/terryyin/lizard/pull/624) | Pending affected-workload and CLI performance qualification. | C# ordinary verbatim and raw strings and the dollar-before-at verbatim form improve. At-before-dollar interpolation retains its existing gap. |
| [#625](https://github.com/terryyin/lizard/pull/625) | Pending affected-workload and CLI performance qualification. | Dart ordinary and raw triple-quoted strings retain subsequent functions and decisions. The isolated timing screen needs a representative Dart workload. |
| [#626](https://github.com/terryyin/lizard/pull/626) | Pending broad CLI performance qualification. The affected API benchmark passed. | GDScript word and symbol forms agree in default and nonstrict modes. |
| [#627](https://github.com/terryyin/lizard/pull/627) | Pending affected-workload and CLI performance qualification. | Fortran doubled quotes and a literal backslash retain the real decision and following subroutine. The affected workload needs timing with ordinary Fortran. |
| [#628](https://github.com/terryyin/lizard/pull/628) | Pending affected-workload and CLI performance qualification. | Erlang single-percent comments reduce NLOC without moving the following function. |
| [#629](https://github.com/terryyin/lizard/pull/629) | Pending affected-workload and CLI performance qualification. | Quoted Erlang atoms name functions, and calls do not create a new function. Multiple clauses follow the current bare-name behavior. |
| [#630](https://github.com/terryyin/lizard/pull/630) | Pending broad CLI performance qualification. The affected API benchmark passed. | Erlang short-circuit operators count in default mode and are omitted in nonstrict mode. |
| [#631](https://github.com/terryyin/lizard/pull/631) | Pending affected-workload and CLI performance qualification. | Structured Text block comments protect a fake decision and leave code after the closer visible. The combined rehearsal conflicts with #632. |
| [#632](https://github.com/terryyin/lizard/pull/632) | Pending affected-workload and CLI performance qualification. | Mixed-case Structured Text preserves operator-prefix identifiers and expected CCN. The combined rehearsal conflicts with #631. |
| [#633](https://github.com/terryyin/lizard/pull/633) | Pending affected-workload and CLI performance qualification. | Sequential Structured Text variable blocks leave the function open for its real decision and final terminator. |
| [#634](https://github.com/terryyin/lizard/pull/634) | Deferred to [SEED-075](SEED-075-reader-corrections.md#perl-word-operator-context). | A valid Perl method call named and increases CCN by one. The separate correction owns lexical context. |
| [#635](https://github.com/terryyin/lizard/pull/635) | Pending affected-workload and CLI performance qualification. | A parameterized Solidity fallback reports the parameter and preserves the following ordinary function. The tested function-typed variable remains outside the function list. |
| [#636](https://github.com/terryyin/lizard/pull/636) | Deferred to [SEED-075](SEED-075-reader-corrections.md#source-documentation-directives). | A GENERATED CODE marker in POD suppresses later code. The separate correction owns opaque document content. |
| [#637](https://github.com/terryyin/lizard/pull/637) | Pending affected-workload and CLI performance qualification. | A Kotlin triple-quoted string with a trailing quote run leaves a real decision and following function visible. The combined rehearsal conflicts with #618. |
| [#638](https://github.com/terryyin/lizard/pull/638) | Deferred to [SEED-075](SEED-075-reader-corrections.md#r-raw-literal-delimiters). | Single-quoted R raw strings and the current lexer’s pipe-delimited form still expose control words. The separate correction owns the missing delimiters. |
| [#639](https://github.com/terryyin/lizard/pull/639) | Deferred to [SEED-075](SEED-075-reader-corrections.md#plsql-national-literals). | The equivalent PL/SQL Q and NQ literals have different CCN. The separate correction owns the national-character form. |
| [#640](https://github.com/terryyin/lizard/pull/640) | Pending affected-workload and CLI performance qualification. | A parameterized TTCN-3 altstep with runs on is measured alongside the following ordinary function. |
| [#641](https://github.com/terryyin/lizard/pull/641) | Pending affected-workload and CLI performance qualification. | Shorthand matches the long-form reader on the nested example. Both forms can end an enclosing function before a later real decision. The requested top-level and callback examples pass. |
| [#642](https://github.com/terryyin/lizard/pull/642) | Deferred to [SEED-075](SEED-075-reader-corrections.md#source-documentation-directives). | A GENERATED CODE marker inside Ruby embedded documentation suppresses subsequent methods. The separate correction owns opaque document content. |
| [#643](https://github.com/terryyin/lizard/pull/643) | Deferred to [SEED-075](SEED-075-reader-corrections.md#r-formal-parameter-boundaries). | Nested R defaults preserve later ordinary formals. A trailing ellipsis and a quoted formal are omitted from the parameter count. The separate correction owns these slots. |
| [#644](https://github.com/terryyin/lizard/pull/644) | Deferred to [SEED-075](SEED-075-reader-corrections.md#plsql-formal-parameter-boundaries). | A quoted PL/SQL formal drops out of the count and ordinary full_parameters lose mode and type. The separate correction owns the public parameter observations. |
| [#645](https://github.com/terryyin/lizard/pull/645) | Deferred to [SEED-075](SEED-075-reader-corrections.md#plsql-nonstrict-case). | PL/SQL mixed case counts by default. Uppercase and mixed case still produce different CCN with nonstrict enabled. The separate correction owns case equivalence. |
| [#646](https://github.com/terryyin/lizard/pull/646) | Deferred to [SEED-075](SEED-075-reader-corrections.md#ruby-percent-literal-context). | Ruby modulo assignment hides a real decision. The separate correction owns the operator and literal boundary. |
| [#647](https://github.com/terryyin/lizard/pull/647) | Deferred to [SEED-075](SEED-075-reader-corrections.md#kotlin-adjacent-nested-comments). | A quote in one Kotlin comment can cause the scanner to skip the next nested comment. The separate correction owns the adjacent-comment case. |
