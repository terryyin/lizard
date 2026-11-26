---
id: SEED-077
status: active
planted: 2026-11-26
---

# SEED-077 Process open PRs and issues from the past 48 hours

<a id="process-prs-past-48h"></a>

### Process open PRs and issues from the past 48 hours

**Identity:** SEED-077#process-prs-past-48h

**Beneficiary:** Lizard maintainers triaging contributions, and users affected by inaccurate analyzer metrics or report output.

**Goal:** Give each listed open PR and issue a disposition backed by evidence against current master. Merge contributions that meet the project's scope, correctness, performance and CI requirements.

**Scope:** Open, non-draft GitHub pull requests and open issues created or updated from 2026-11-24 06:47:08 UTC through 2026-11-26 06:47:08 UTC. Initial master is f7a11e5248a388c1cee706b5aa0d2f234837c44d. The developer authorized merging qualified PRs, posting acknowledgement comments for material corrections, and reviewing in parallel worktrees. #613–#647 continue from their [SEED-074](https://github.com/terryyin/lizard/commit/5e087cea) dispositions. Queued SEED-075 corrections stay their owners unless this story completes them.

**Performance policy:** Each affected language's cumulative accepted set is measured against the initial master on its affected API workload. The whole accepted set is also measured on a broad CLI workload. Each workload's paired 95% upper slowdown bound must be below the runner's 3% default. A failing group is bisected before any of its members merge.

**Key examples:** A supported construct reports its intended metrics, and following ordinary functions keep theirs. A redundant or duplicate proposal is identified against master or its sibling PR. A contribution with a regression or unmet gate is held, with a specific reason or a linked correction.

**Source:** Dashboard launch 3af417fe-be15-4827-8c1b-800781d0605d (Claude Code).

#### Issues

- [#693](https://github.com/terryyin/lizard/issues/693) ND: Should && / || in a condition add a nesting level?

#### Pull requests

- [#613](https://github.com/terryyin/lizard/pull/613) Restore Svelte nesting extensions after the statement reader API change
- [#614](https://github.com/terryyin/lizard/pull/614) Match Lua long-string and comment bracket levels
- [#615](https://github.com/terryyin/lizard/pull/615) Keep Lua length and integer-division operators in the token stream
- [#616](https://github.com/terryyin/lizard/pull/616) Keep Rust raw string contents out of complexity metrics
- [#617](https://github.com/terryyin/lizard/pull/617) Tokenize Scala multiline string literals as a whole
- [#618](https://github.com/terryyin/lizard/pull/618) Recognize Kotlin backtick names containing spaces
- [#619](https://github.com/terryyin/lizard/pull/619) Recognize custom delimiters in C++ raw string literals
- [#620](https://github.com/terryyin/lizard/pull/620) Ignore control keywords inside R backtick identifiers
- [#621](https://github.com/terryyin/lizard/pull/621) Recognize Zig multiline string lines during tokenization
- [#622](https://github.com/terryyin/lizard/pull/622) Keep Java text block contents out of complexity metrics
- [#623](https://github.com/terryyin/lizard/pull/623) Match PHP heredoc and nowdoc labels at line boundaries
- [#624](https://github.com/terryyin/lizard/pull/624) Recognize C# verbatim and raw string boundaries
- [#625](https://github.com/terryyin/lizard/pull/625) Respect raw and escaped Dart string delimiters
- [#626](https://github.com/terryyin/lizard/pull/626) Count GDScript word-form Boolean operators
- [#627](https://github.com/terryyin/lizard/pull/627) Treat backslashes literally in Fortran character strings
- [#628](https://github.com/terryyin/lizard/pull/628) Exclude single-percent Erlang comments from code metrics
- [#629](https://github.com/terryyin/lizard/pull/629) Recognize quoted Erlang atoms as function names
- [#630](https://github.com/terryyin/lizard/pull/630) Count Erlang short-circuit Boolean operators
- [#631](https://github.com/terryyin/lizard/pull/631) Read Structured Text block comments through their closing delimiter
- [#632](https://github.com/terryyin/lizard/pull/632) Respect case and word boundaries in Structured Text condition tokens
- [#633](https://github.com/terryyin/lizard/pull/633) Keep Structured Text functions open after variable declarations
- [#634](https://github.com/terryyin/lizard/pull/634) Count Perl word-form short-circuit operators
- [#635](https://github.com/terryyin/lizard/pull/635) Measure Solidity constructors and special entry points
- [#636](https://github.com/terryyin/lizard/pull/636) Exclude Perl POD and data sections from source metrics
- [#637](https://github.com/terryyin/lizard/pull/637) Keep Kotlin multiline string contents out of code metrics
- [#638](https://github.com/terryyin/lizard/pull/638) Recognize R raw string delimiters during tokenization
- [#639](https://github.com/terryyin/lizard/pull/639) Keep PL/SQL alternative-quoted literals opaque
- [#640](https://github.com/terryyin/lizard/pull/640) Include TTCN-3 altstep bodies in function metrics
- [#641](https://github.com/terryyin/lizard/pull/641) Recognize R shorthand anonymous function syntax
- [#642](https://github.com/terryyin/lizard/pull/642) Exclude Ruby embedded documentation and data sections from metrics
- [#643](https://github.com/terryyin/lizard/pull/643) Count R formal parameters around nested default expressions
- [#644](https://github.com/terryyin/lizard/pull/644) Keep PL/SQL default calls within their formal parameter
- [#645](https://github.com/terryyin/lizard/pull/645) Count mixed-case PL/SQL condition keywords
- [#646](https://github.com/terryyin/lizard/pull/646) Recognize Ruby percent literals with arbitrary punctuation delimiters
- [#647](https://github.com/terryyin/lizard/pull/647) Keep nested Kotlin block comments out of executable metrics
- [#648](https://github.com/terryyin/lizard/pull/648) fix: keep Rust raw identifiers intact during tokenization
- [#649](https://github.com/terryyin/lizard/pull/649) Fix Lua metrics after floor division
- [#650](https://github.com/terryyin/lizard/pull/650) Fix Kotlin accessors for annotation use-site targets
- [#651](https://github.com/terryyin/lizard/pull/651) Fix JS/TS nesting after function expressions without semicolons
- [#652](https://github.com/terryyin/lizard/pull/652) Fix Lua function ranges for callbacks named it
- [#653](https://github.com/terryyin/lizard/pull/653) Fix Java metrics for text blocks
- [#654](https://github.com/terryyin/lizard/pull/654) Fix PHP and Java identifiers counted as nested structures
- [#655](https://github.com/terryyin/lizard/pull/655) Fix Vue metrics after mixed-case script end tags
- [#656](https://github.com/terryyin/lizard/pull/656) Fix Java signatures with wrapped wildcard type arguments
- [#658](https://github.com/terryyin/lizard/pull/658) Fix Vue and Svelte script ends after line comments
- [#659](https://github.com/terryyin/lizard/pull/659) Fix repeated scope names in nested Python functions
- [#660](https://github.com/terryyin/lizard/pull/660) Fix false recursion for JS/TS optional member calls
- [#662](https://github.com/terryyin/lizard/pull/662) Fix Lua metrics after the length operator
- [#663](https://github.com/terryyin/lizard/pull/663) Fix Scala metrics for question marks
- [#664](https://github.com/terryyin/lizard/pull/664) Count Lua control-flow words in complexity scores
- [#666](https://github.com/terryyin/lizard/pull/666) Keep Lua functions when names match Ruby block words
- [#670](https://github.com/terryyin/lizard/pull/670) Fix JS and TypeScript arrow ranges that end with a callback
- [#672](https://github.com/terryyin/lizard/pull/672) Fix Rust functions lost after declarations without bodies
- [#673](https://github.com/terryyin/lizard/pull/673) Print PHP parameter names once in function signatures
- [#674](https://github.com/terryyin/lizard/pull/674) Fix Java methods lost after field initializer calls
- [#676](https://github.com/terryyin/lizard/pull/676) Fix phantom JS/TS functions after parenthesized initializers
- [#678](https://github.com/terryyin/lizard/pull/678) Match Lua long comment and string delimiters
- [#679](https://github.com/terryyin/lizard/pull/679) Fix Go cognitive complexity across statement boundaries
- [#680](https://github.com/terryyin/lizard/pull/680) Fix Kotlin accessors when get and set are identifiers
- [#681](https://github.com/terryyin/lizard/pull/681) Count logical AND before parenthesized assignments in C and C++
- [#682](https://github.com/terryyin/lizard/pull/682) Fix modified complexity for Python names called match
- [#683](https://github.com/terryyin/lizard/pull/683) Fix PHP function names for reference returns
- [#684](https://github.com/terryyin/lizard/pull/684) Count ternary expressions in JavaScript control headers
- [#685](https://github.com/terryyin/lizard/pull/685) Fix Ruby metrics for star operator symbols
- [#687](https://github.com/terryyin/lizard/pull/687) Recognize JavaScript regex literals after comments
- [#688](https://github.com/terryyin/lizard/pull/688) Exclude clear rvalue references from C++ complexity
- [#689](https://github.com/terryyin/lizard/pull/689) fix: retain zero cognitive complexity when serializing declarations
- [#690](https://github.com/terryyin/lizard/pull/690) fix: render extension metric columns in HTML reports
- [#691](https://github.com/terryyin/lizard/pull/691) fix: preserve double quotes in CSV function names
- [#692](https://github.com/terryyin/lizard/pull/692) fix: emit function averages once in XML reports
- [#694](https://github.com/terryyin/lizard/pull/694) fix: secure index.py (CWE-20)
- [#700](https://github.com/terryyin/lizard/pull/700) Fix repeated scope names in nested GDScript lambdas
- [#713](https://github.com/terryyin/lizard/pull/713) Keep Java enclosing methods when a local class declares a field
- [#717](https://github.com/terryyin/lizard/pull/717) Keep PHP heredocs and interpolated strings intact
- [#718](https://github.com/terryyin/lizard/pull/718) Preserve Rust raw text and byte string boundaries
- [#720](https://github.com/terryyin/lizard/pull/720) End Lua function signatures at the closing parenthesis
- [#724](https://github.com/terryyin/lizard/pull/724) Remove repeated class names from Java local method signatures
- [#730](https://github.com/terryyin/lizard/pull/730) Keep Go function bodies after inline result types
- [#735](https://github.com/terryyin/lizard/pull/735) Keep PHP closure names tied to their assignments
- [#738](https://github.com/terryyin/lizard/pull/738) Restore Kotlin accessors after functional interface declarations

#### Review dispositions

Seven parallel reviews ran against the frozen master. The accepted PRs were integrated, with corrections, on top of the current master (`seed077-all3`). Full pytest (3989 passed, 7 skipped) and `make` pass there.

Corrections made during integration:

- #635: a special function starts only outside a function body and before `(`. A Go-like `SPECIAL_FUNCTIONS` hook replaced the per-token wrapper after it measured +9% CPU.
- #718: raw C strings `cr#"…"#`.
- #672: removed a workaround that #718 made unnecessary.
- #679: dropped the runtime-patched handling of Unicode line separators in comments.
- #659 and #700: one fix in the shared name builder.
- #724: adapted to that name-builder fix.
- #689: zero default through the analyzer's default mechanism.
- #691: keeps the CSV layout while doubling quotes. The PL/SQL formal CSV test expectation was updated to match.
- #655: removed an unused hook.
- #690: no `None` threshold comparison.
- #639: national-character prefix.
- #647: comment scan resumes after each literal. Master's SEED-075 implementation was kept when it conflicted.
- #621, #617, #625 and #627: moved test files into `test/test_languages/`.

| Disposition | PRs |
| --- | --- |
| Merged | #616 #617 #618 #620 #621 #623 #624 #625 #626 #627 #628 #629 #630 #631 #632 #633 #635 #637 #639 #640 #641 #647 #648 #649 #650 (via #680) #651 #652 #653 #654 #655 #658 #659 #660 #662 #663 #664 #666 #670 #672 #673 #674 #676 #678 #679 #680 #682 #683 #685 #687 #689 #690 #691 #692 #700 #713 #718 #720 #724 #730 #735 #738 |
| Held for quiet-machine timing | #619, #681 and #688, which touch the shared C-like tokenizer and the C++ rvalue scan. The developer chose to hold them. |
| Handed to the active SEED-075 owner | #638 → r-raw-literal-delimiters, now on master; #645 → plsql-nonstrict-case, planned by another session |
| Already fixed on master | #613 (834313ae); #634 #636 #642 #646 (SEED-075 corrections) |
| Duplicate | #614 of #678; #615 of #662; #622 of #653; #717 of #623 |
| Deferred | #643 → SEED-075#r-formal-parameter-boundaries; #644 → SEED-075#plsql-formal-parameter-boundaries, since completed on master; #656 → SEED-078#multiline-token-long-names; #684 → SEED-078#cognitive-ternary-in-control-headers |
| Declined | #694: the filename only selects a reader and is never used as a path |
| Developer-approved beyond direction | #690 |
| Issue | #693 → SEED-078#nd-without-logical-operators. The developer approved the metric change, and the reply was posted. |

#### Performance evidence

Unrelated work kept the host loaded throughout (load average 9–29). Three paired wall-clock rounds stayed mostly inconclusive at the 3% bound. The evidence is retained under the integration worktree's `.planning/evidence/`.

- Deterministic cProfile call counts per language and extension are within ±3%.
- Solidity's measured +9% per-token cost was corrected to about +3%, with an interval that includes zero.
- The warm CLI path has +0.4% calls. A one-time ~90 ms regex compile comes from six new tokenizer variants.
- The developer chose to merge everything except the C-family PRs before a quiet-machine timing.
