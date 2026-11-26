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

**Beneficiary:** Users who gate code on nesting depth (`-END`), including the reporter of [#693](https://github.com/terryyin/lizard/issues/693) using it in a safety-critical pipeline.

**Goal:** ND reports only the nesting of control structures, as [ADR 0001](../../docs/adrs/0001-ubiquitous-language-accepted.md) describes, so `&&`/`||` in a condition no longer adds a level.

**Scope:** The developer approved this as a deliberate metric change on 2026-11-26. Remove the logical-operator contribution from ND for every language that ND supports. Update the tests that pin the current behavior. Note the change in release notes, because existing ND values and thresholds drop. Out of scope: CCN, NS and cognitive complexity.

**Key examples:**

- From #693: `plain` (if inside a loop) and `logical` (the same with `a && b` in the if) both report ND 2. `logical` reports 4 today.
- A logical operator outside any condition adds no ND.
- `-ENS` results are unchanged.

<a id="cognitive-ternary-in-control-headers"></a>

### Count ternaries in control headers for all brace languages

**Identity:** SEED-078#cognitive-ternary-in-control-headers

**Beneficiary:** Users of `-Ecognitive` on C-like languages, including JavaScript, TypeScript, JSX, Vue, C, Java and C#.

**Goal:** A ternary inside an `if`/`for`/`while` header adds its cognitive increment in every brace language.

**Scope:** [#684](https://github.com/terryyin/lizard/pull/684) fixes this only when the language is plain JavaScript. The fix belongs in the shared brace counter (`cognitive_brace.py`). Merge or supersede #684 when this lands.

**Key examples:**

- `if (a ? b : 0) {}` followed by a ternary in a `for` header scores 4, not 2, in .js, .ts, .jsx, .vue, .c, .java and .cs.
- #684's JavaScript tests pass unchanged.

<a id="c-family-pr-timing"></a>

### Qualify and merge the held C-family PRs

**Identity:** SEED-078#c-family-pr-timing
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/047-c-family-pr-timing/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"662d13a9e831ff5b2a16845e3488986ec19590f6e77d9ca6f955a4338485dcc1","plan":"32baff18bdf1861ebf318e5c6ea7d031af7492f8523d53e0084e768ed6b5c1d1"}}
```

**Beneficiary:** C, C++, Java, C#, Objective-C and Dart users. These PRs fix C++ raw strings with custom delimiters ([#619](https://github.com/terryyin/lizard/pull/619)) and `&&` in rvalue references being counted as a decision ([#688](https://github.com/terryyin/lizard/pull/688)). [#681](https://github.com/terryyin/lizard/pull/681) builds on #688 and also counts `&&` before a parenthesized assignment.

**Goal:** Merge #688, #681 and #619 once paired timing on a quiet machine shows that they keep each affected workload within the 3% bound. If a valid measurement fails the bound, correct the PR first.

**Scope:**

- SEED-077 reviewed all three and accepted them for correctness. This story does not review them again. #681 contains #688, so merge #688 first. On master `6c28fc53`, each PR merges cleanly, and #619 merges cleanly on top of #681.
- **Timing gate.** The gate candidate is master with all three PRs merged. Compare it with current master in [paired runs](../../.agents/skills/lizard-process-pr/references/performance.md). For each workload, the paired 95% upper slowdown bound must be strictly below 3%. Time one PR alone only when the combined candidate fails, to find which PR causes the slowdown. A PR that passes on its own may then merge without the others. Timing each PR as a separate gate is not required.
- **Workloads:** the C/C++, Java, C# and Dart API workloads, `-Eignoreassert` on the C/C++ and Java corpora, `-Enonstrict` on the C/C++ corpus, and the broad CLI. Build the corpora from the base test snippets plus the PRs' own test cases. SEED-077's corpora and evidence were removed at its wrap-up, so they cannot be reused.
- **Quiet machine.** No other heavy measurement from this project runs at the same time. Record the load average before and after each run. A result passes, misses the bound, or is inconclusive (invalid setup), as `performance.md` defines them. Missing the bound does not prove that the PR caused the slowdown. Choose before launch whether a retry will be longer or run in a quieter window. Keep every earlier run, failed ones included.
- **Correction.** If a run misses the bound, profile the combined candidate. Correct a PR only when the profile finds the cost in that PR's code. If the profile finds nothing, the next step is a quieter retry. If the miss comes only from correct tokens that master skips, as in the dense-snippet fact below, do not correct the PR. Report the raw and per-token results to the developer for the merge decision. All three branches allow maintainer edits. Push a small correction to the contributor's branch without rewriting its history. If the correction is larger, record it as a linked correction story and leave that PR unmerged.
- **Merge.** Merge with a guard on the exact head SHA, after required hosted checks pass on that head. Thank the contributor afterwards, and mention any correction made.
- **Out of scope:** re-reviewing correctness, changing the 3% policy or the timing metric, and a changelog entry. The changelog is written at release time; none of SEED-077's 61 merges added one. When the release notes are written, they should say that #681 and #688 lower CCN on real C++ code. The contributor measured 1,025 of the functions in 4,005 C++ files losing 2,107 CCN points in total.

**Facts settled during refinement (2026-11-26):**

- Java, C#, Objective-C, Dart and TTCN readers replace `parallel_states`, so they never run `CppRValueRefStates`. The only changes that reach them are #619's tokenizer rule and #688's `lizardignoreassert` rewrite. The rewrite runs for every language, but only with `-Eignoreassert`.
- #619 adds the first capturing group to the shared token pattern. The comment there warns that subgroups are bad for performance. A local A/B of the tokenizer alone, over 1.45M characters of test sources, measured −0.4% (95% CI −1.6% to +0.7%). So the group is not the likely cause of SEED-077's +6% Java point estimate. That estimate was taken at load averages of 9 to 29.
- A dense C++ snippet built from the PRs' own examples tokenized 25.6% slower with #619 (95% CI 24.2% to 26.9%). On the same text, master emits 15,400 tokens and #619 emits 20,800. Master reads `R"tag(` as an ordinary string literal that swallows the code after it, so the cost is the extra, correct tokens, not a slower rule. A paired API run on that snippet missed the bound by a similar margin, for #619 alone and for the combined candidate. A corpus heavy with the fixed syntax therefore measures master's mistake, not the PR's cost.
- On a 16-core host, the load average was 2.9–6.6 during refinement.

**Key examples:**

- `R"d(… if (x) …)d"` in C++ adds no decision. The next function keeps its metrics.
- `void f(T&& x)` adds no CCN. `a && (b = c)` still counts one.
- Combined candidate vs master, C++ API: an upper bound of 2.1% passes. When every workload passes, #688, then #681, then #619 merge.
- The same workload at −2% to +5% under load misses the bound. The profile finds no added cost in `CppRValueRefStates`, so nothing is corrected, and the run is retried in a quieter window.
- A miss where the profile shows the rvalue scan's per-token cost: correct #688 on its branch and merge that correction into #681. #619 may merge alone if its own run passes.
