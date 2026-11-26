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

<a id="multiline-token-long-names"></a>

### Keep newlines out of long names and NLOC for multi-line type tokens

**Identity:** SEED-078#multiline-token-long-names
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/046-wrapped-type-token-reporting/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"aec57c3ebd925843921f291eab1015040ba9133cf3618bb1edd021ddef67bb4f","plan":"2a4dea3a25db3e5db5428b16271e4086e39a72b190e39c6d13f339bf6ac29e2f"}}
```

**Beneficiary:** Users who read or compare Lizard signatures and NLOC for wrapped generic types, including Java wildcard and C# and Kotlin nullable type arguments.

**Goal:** Lizard reports a single-line `long_name` and code-line NLOC for signatures with wrapped type arguments that the shared tokenizer already groups as one token. Users can compare the signatures without reformatting the source and still locate the functions by their physical line ranges.

**Scope:** Use the existing shared type-token rule and `line_counter` for every analysis that receives these tokens. Fold wrapping whitespace into a readable single-line signature. Count each nonblank continuation line toward function and file NLOC, and count every physical break toward source locations. Keep the type argument atomic so its question mark contributes no decision. Wrapping alone preserves the signature's token and parameter counts. Multiline string and comment handling and new type syntax are outside this correction. The shared repair supersedes the Java-only approach in [#656](https://github.com/terryyin/lizard/pull/656), which supplies the Java acceptance cases. Settle the PR disposition after the verified repair lands.

**Key examples:**

- Analyze Java `void m(Map<String,\n    ? extends List<String>> a) {}`, C# `void m(Dictionary<string,\n    int?> a) {}`, and Kotlin `fun m(a: Map<String,\n    Int?>) {}`. Each has the long name of its single-line form, function NLOC 2, one parameter, and CCN 1.
- Insert an empty or spaces-and-tabs-only line inside the grouped argument. Function and file NLOC stay at their wrapped values. The function end and the following function's start each advance by one physical line.
- Analyze #656's `GroupedValues.count` example. Its long name is `GroupedValues::count( Map <String, ? extends Number> values)`, function NLOC is 5, and file NLOC is 8. The original closing-brace line remains the function end.

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

**Beneficiary:** C, C++, Java, C#, Objective-C and Dart users. These PRs fix C++ raw strings with custom delimiters (#619) and `&&` in rvalue references being counted as a decision (#681, #688).

**Goal:** Merge [#619](https://github.com/terryyin/lizard/pull/619), [#681](https://github.com/terryyin/lizard/pull/681) and [#688](https://github.com/terryyin/lizard/pull/688) once paired timing on a quiet machine shows each affected workload within the 3% bound. Otherwise, correct them first.

**Scope:** SEED-077 reviewed all three and accepted them for correctness. #681 contains #688, so merge #688 first. The developer held them because the host load kept the paired timing inconclusive. The Java and C++ point estimates were about +6%. These changes touch the shared C-like tokenizer (#619 adds a back-reference alternative) and add an event filter for every C/C++ token (#681, #688). Time the C/C++, Java, C# and Dart API workloads, `-Eignoreassert`, nonstrict, and the broad CLI against current master. Use the corpora rebuilt from base test snippets plus the PRs' authored cases. Note for review: #681 and #688 shift CCN noticeably on real C++ code. Across 4,005 C++ files, the contributor measured 1,025 functions losing 2,107 CCN points in total.

**Key examples:**

- `R"d(… if (x) …)d"` in C++ adds no decision. The next function keeps its metrics.
- `void f(T&& x)` adds no CCN. `a && (b = c)` still counts one.
- Each affected workload's paired 95% upper bound is below 3%.
