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
