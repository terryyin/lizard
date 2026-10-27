# Review and dispose of FabioMalpezzi's open PRs

**Identity:** SEED-010#review-fabio-malpezzi-prs
**Source:** [Refined story](../../seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-fabio-malpezzi-prs)

## Goal and scope

Give each PR in the execution-start open queue an evidence-backed disposition:
merge a worthwhile, fully verified revision and thank its author; explain an
unnecessary change; or defer a real problem to a linked correction story.
The source owns the strict slowdown limit below 3% and correction estimate
cutoff above fifteen minutes. Review and measurement time are not that cutoff.
This is a preparation plan, not execution authority.

Retain the source's exclusions. Do not promise all PRs will merge or assume
their proposed implementation is correct. Keep publication of planning records
separate from hosted PR merges; Dough Land does not merge PRs.

## Existing solutions and architectural constraints

PFE inspection located the following owners; change these concepts where the
evidence requires it rather than building a second parser beside them.

- JS inherits TypeScriptReader. TypeScriptStates composes function, binding,
  object and declaration handling; JSX/TSX have another tokenizer path.
  JavaScriptBindings already separates binding defaults from initializer
  expressions. Preserve that ownership when reviewing parameters and CCN.
- FunctionInfo.parameters derives public parameter counts from full_parameters.
  A parser-local count is not end-to-end proof, especially for the lone-dollar
  name disclosed by #532. A required core correction must cover other readers.
- js_style_regex_expression serves TS, TSX and Ruby; javascript_templates
  imports its REGEX_TOKEN. Regex context changes reach template scanning too.
- CCppCommentsMixin is shared across many readers. A JS comment correction
  must preserve the different C/C++ line-continuation rule.
- lizardns owns structural depth as an extension; Python's existing reader
  provides indentation context. Inspect function/file lifetime before choosing
  how to correct that extension; do not infer correctness from disabled tests.

Follow [ADR-0000: Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md) and
[ADR-0001: Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md).
Terry confirmed ADR-0001 Accepted on 2026-10-27, resolving its stale in-file
status. No new architectural direction or North Star topic is needed for this
review. Respect the public lizard facade and existing lizard_core ownership.
Reader changes use lizard-language-support; meaningful cross-component
changes recheck relevant ADRs and consumers.

## Preparation observations and decisive premises

Observed in the preparation checkout
`/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr`
at `62b6d76dd2c02cdd084f1b6f9fefff449eced3e8` on 2026-10-27:

- Connected GitHub search `repo:terryyin/lizard is:open author:FabioMalpezzi`
  returned the source's 19 PRs. The first language-tagged example from each
  body is retained in [review-inputs.json](review-inputs.json). This is a
  reproducible preparation snapshot, not the execution-start set or immutable
  evidence about a future PR head.
- The following consuming operation reached FunctionInfo through the public
  analyzer (and the actual NS extension for Python). All 19 examples showed
  the discrepancies recorded in their owning slices below. They establish
  symptoms, not complete correctness requirements or proof that a PR fixes them.
- `nix develop -c python -m pytest` passed: 1665 passed, 7 skipped.
  Existing tests passing while the examples fail means new regression proof
  must discriminate the corrections; a green baseline is not such proof.
- `nix develop -c python lizard.py lizard.py lizard_core lizard_languages lizard_ext`
  reached the broader CLI workload: 942 output lines and exit 1 from threshold
  warnings. A timed subprocess run took 0.163 seconds. Warm public-API runs
  of 1000 small examples completed in 0.051–0.067 seconds. These settle route
  availability only; they are not stable performance baselines or acceptance.

Replay the symptom observation from the repository root:

```sh
nix develop -c python - <<'PY'
import json
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_ext.lizardns import LizardExtension
path = '.planning/slice-plans/004-fabio-malpezzi-pr-reviews/review-inputs.json'
for case in json.load(open(path)):
    analyzer = (FileAnalyzer(get_extensions([LizardExtension()]))
                if case['filename'].endswith('.py') else analyze_file)
    result = analyzer.analyze_source_code(case['filename'], case['source'])
    print(case['pr'], [(f.name, f.parameter_count, f.cyclomatic_complexity,
                       f.start_line, f.end_line,
                       getattr(f, 'max_nested_structures', None))
                      for f in result.function_list])
PY
```

The execution checkout is selected by execution-start, not this preparation's
temporary path. Refresh/replay there before corrections. No assumption is made
that an old PR applies cleanly, its CI is green, or its branch is writable.
The first necessary authenticated correction publication and exact-head merge
are the permitted credentialed probes: inspect read-only metadata first, then
use the real operation once authorized. A permission or branch-policy refusal
holds that PR without merging or claiming completion; preserve its evidence
and report the actual missing permission. Do not use a throwaway remote write
to test access. Independent read-only reviews may continue.

## Review contract shared by every slice

Each numbered slice owns one complete PR disposition and one proof loop.

1. At the start of slice 1, freeze the refreshed queue (numbers, URLs, heads,
   bases and state) and starting target revision in this plan. Reconcile the
   slice list with that set, preserving every selected PR. Later arrivals are
   outside the source's commitment. Read each PR's complete diff, discussion,
   checks and dependency claims before its disposition.
2. Reproduce against the current target. Establish expected public metrics,
   names and boundaries from valid source semantics and project conventions.
   Assess user benefit. A report already fixed by earlier work receives fresh
   evidence and an unnecessary/duplicate disposition, not an automatic merge.
3. Inspect the entire affected concept and trace shared consumers, including
   untouched readers and tests. Use the installed consumer-trace procedure for
   actual corrections. Give located consumers run/reused/excluded dispositions.
   Reuse the existing owners above; no PR-specific switches or parallel counts.
4. Estimate needed correction and cleanup. If above fifteen minutes, reuse or
   queue a separate story via dough-product-backlog, with beneficiary,
   reproducer, expected result, remaining work and PR link. Revise the estimate
   when evidence changes; do not evade the cutoff by restarting or renaming work.
   Park or revert only attempt-owned incomplete corrections and leave the PR
   unmerged. A deferral is complete only with the linked backlog story.
5. Otherwise correct and independently refactor through the execution
   workflow's post-change-refactor gate. Prove a regression fails without the
   required behavior and passes with it at analyze_file/FileAnalyzer/CLI.
   Include relevant valid boundaries and already-accepted interacting examples.
   Do not mock parsing or weaken an existing behavioral assertion.
6. Measure both incremental and cumulative performance by the protocol below.
   Revalidate after every correction, dependency merge or upstream change that
   affects evidence. A candidate passing alone does not pass the combined set.
7. Before merging, run full `nix develop -c python -m pytest` and
   `nix develop -c make`, required by AGENTS.md and the source's merge gates.
   Confirm required hosted CI on the final candidate and current base.
   The configured workflow is python-app.yml; refresh its matrix during
   execution because the Python-support story is active. Local lint/tests
   do not substitute for required CI, or vice versa.
8. Re-read the final PR head/state and target before an exact-head guarded
   hosted merge. Do not force-push contributor history. If a correction cannot
   be published to its head, hold that merge and report the concrete route
   needed rather than silently substituting an unrelated trunk patch.
   After confirmed merge, post the source-authorized thank-you on that PR,
   acknowledging material corrections, then retain the comment URL. On retry,
   check whether the merge/message already exists to avoid duplicates.
9. Record source/head/base and final/merge SHAs, user benefit, observed versus
   expected result, quality findings and their resolution, proof commands and
   outcomes, performance evidence, and disposition in this plan while active.
   Before wrap-up removes it, preserve necessary evidence in the PR discussion
   for merged work or the linked follow-up story for deferred work; retain an
   enduring maintainer review record for other dispositions. Never delete the
   sole evidence needed to account for the selected review set.

A slice is not done while its disposition, required CI or thank-you is pending.
Hosted waits do not consume the correction estimate. Safe stopping points are
verified dispositions; they leave earlier accepted improvements useful.

## Performance proof

Before the first correction, retain a fixed corpus and its manifest/content
hashes outside disposable checkouts, with the literal harness and options.
Use both representative affected inputs (the full PR examples and nearby
regression/control cases, including JS/TS/JSX/TSX and shared Ruby/C++ behavior
as relevant) and a broader fixed copy of the starting revision's
lizard.py, lizard_core, lizard_languages and lizard_ext source tree.
Do not time a changing checkout as the input corpus.

Compare candidate versus its current target for incremental cost, and versus
the frozen starting target for cumulative cost. Use the same machine, Nix
environment, interpreter, options, extensions and input bytes on both sides;
record each. Include -ENS for #527 and affected extension cases. Use warmed
API analysis batches for affected inputs and complete CLI analysis for the
broader corpus, separating setup/installation time from measured analysis.

Calibrate batches to at least one second, warm up both revisions, and alternate
baseline/candidate order over at least 20 paired batches. Retain raw timings,
medians, variability, percentage change and a paired confidence interval.
Use a 95% upper confidence bound on relative slowdown below 3% to substantiate
the source's strict gate for each workload, both incremental and cumulative.
Do not average an affected-workload regression away with unrelated inputs.
If noise straddles 3%, repeat with longer batches or a quieter environment;
if unresolved, keep the merge pending and record what is unproved. Profile
a reproducible slowdown, then correct within the same correction-estimate rule
or defer a real problem to a linked story. Exactly 3% fails.

Each merging slice owns its cumulative comparison before merge. The last
remaining disposition also checks complete coverage of the frozen set and
retains a final combined-suite/performance summary, reusing still-valid evidence.
No change may merge first with combined validation deferred until the end.

## Ordered slices

For every slice below:
**Type:** Behavior. **Status:** planned.
The precondition is the selected PR at a recorded revision; the trigger is its
review on the current target; the result is its evidence-backed merged,
unnecessary or deferred disposition under the complete contract above.
Expected metrics below are review examples, not permission to merge a partial
fix. Verify the full report and edge cases before deciding.

The order puts shared lexical behavior early and claimed prerequisites before
dependents. Move a slice only for evidenced dependencies or learning while
preserving proof ownership. A deferred prerequisite does not justify merging
a broken dependent; assess a coherent independent correction or defer it too.

### 1. Python nesting boundaries — PR #527
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/527

Behavior: Review the reported python nesting boundaries problem and produce its
verified disposition under the shared review contract.

Baseline observation: Python NS for a, b, c is 1, 2, 6; expected 1, 1, 0.

Proof: Two successive functions and a comprehension/ternary have independent structural depth; also observe separate files through one analyzer, match/case, async statements and continuation lines. Focused command: `nix develop -c python -m pytest test/testNestedStructures.py test/test_languages/testPython*.py test/test_languages/testGDScript.py`.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 2. Regex literal context — PR #528
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/528

Behavior: Review the reported regex literal context problem and produce its
verified disposition under the shared review contract.

Baseline observation: a and the nested arrow have CCN 1; expected 2 each.

Proof: Regex literals after return and => preserve conditions, function ends and following files; division remains division. Exercise JS, TS, JSX, TSX and Ruby because the decorator is shared. Focused command: `nix develop -c python -m pytest test/test_languages/testJavaScriptRegex.py test/test_languages/testJavaScript.py test/test_languages/testTSX.py test/test_languages/testRuby.py test/testNestedStructures.py`.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 3. Objects inside arrow expressions — PR #531
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/531

Behavior: Review the reported objects inside arrow expressions problem and produce its
verified disposition under the shared review contract.

Baseline observation: box ends at line 4 and layout at 5; expected 2 and 4.

Proof: An object in an expression-bodied arrow does not close or extend surrounding functions; a function-type return annotation still allows its function body. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 4. Multiline arrow boundaries — PR #533
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/533

Behavior: Review the reported multiline arrow boundaries problem and produce its
verified disposition under the shared review contract.

Baseline observation: arrows end at 5 and 7, m has CCN 2, pick ends at 10; expected 4, 6, CCN 3, and line 11.

Proof: Check newline termination versus expression continuation, case labels, async methods after fields, NLOC and token ownership. Verify #531 first; test both with and without semicolons. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 5. Arrows in ternary branches — PR #534
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/534

Behavior: Review the reported arrows in ternary branches problem and produce its
verified disposition under the shared review contract.

Baseline observation: choose contains only one reported arrow; expected two, with CCN 1 and 2.

Proof: An enclosing ternary colon ends the first arrow, while a ternary inside an arrow retains its colon. Verify #531 first. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 6. Identifier parameters — PR #532
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/532

Behavior: Review the reported identifier parameters problem and produce its
verified disposition under the shared review contract.

Baseline observation: render and round each have one parameter; expected three and two.

Proof: Names beginning with _ or $, and names matching TS type keywords, count correctly while annotations do not. Explicitly assess the PR's disclosed lone-$ limitation through FunctionInfo, not just parser-local state. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 7. TypeScript this declarations — PR #535
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/535

Behavior: Review the reported typescript this declarations problem and produce its
verified disposition under the shared review contract.

Baseline observation: onClick has two parameters; expected one.

Proof: A leading this type declaration is excluded while actual arguments and body uses of this are preserved. Verify #532 first. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 8. Arrow parameter forms — PR #536
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/536

Behavior: Review the reported arrow parameter forms problem and produce its
verified disposition under the shared review contract.

Baseline observation: four reported arrows each have zero parameters; expected 2, 1, 2, 2.

Proof: Parenthesized, single-name, ternary-branch and typed-return arrows retain parameter counts and initializer CCN; compare with named arrow controls. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 9. Arrow parameters after member access — PR #537
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/537

Behavior: Review the reported arrow parameters after member access problem and produce its
verified disposition under the shared review contract.

Baseline observation: module.exports.add and load each have zero parameters; expected two.

Proof: Member access before an arrow does not turn its parameters into call arguments. Recheck against #536 and real member calls. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 10. Immediately invoked function boundaries — PR #538
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/538

Behavior: Review the reported immediately invoked function boundaries problem and produce its
verified disposition under the shared review contract.

Baseline observation: setup ends at line 4 with CCN 1 and m is absent; expected line 9, CCN 2, and m present.

Proof: Nested IIFEs keep outer function and class boundaries, following conditions and methods. Cover arrow and traditional-function IIFEs. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 11. Parenthesized expression metrics — PR #539
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/539

Behavior: Review the reported parenthesized expression metrics problem and produce its
verified disposition under the shared review contract.

Baseline observation: total has CCN 1 and merge CCN 2; expected three each.

Proof: An abandoned provisional arrow or method retains the enclosing expression's tokens and conditions; real arrows still own their defaults and body CCN. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 12. Switch label boundaries — PR #540
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/540

Behavior: Review the reported switch label boundaries problem and produce its
verified disposition under the shared review contract.

Baseline observation: only a is reported; expected both callbacks as well.

Proof: case/default colons allow callbacks and nested blocks without consuming them as types; TS annotations and ternaries remain distinct. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 13. Get and set member names — PR #541
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/541

Behavior: Review the reported get and set member names problem and produce its
verified disposition under the shared review contract.

Baseline observation: get/set methods are absent and create ends at 8; expected methods present and create ending at 7.

Proof: Methods, shorthand properties and arrow-valued properties named get/set coexist with actual accessor prefixes and a following function. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 14. Array values and computed names — PR #542
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/542

Behavior: Review the reported array values and computed names problem and produce its
verified disposition under the shared review contract.

Baseline observation: types ends at line 5; expected 7.

Proof: Nested arrays in property values and bracketed computed keys preserve their distinct boundaries and following functions. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 15. Object spread expressions — PR #543
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/543

Behavior: Review the reported object spread expressions problem and produce its
verified disposition under the shared review contract.

Baseline observation: onClick is absent and a callback is named withDefaults as a method.

Proof: Spreads of parenthesized expressions and calls retain nested callbacks without creating fictitious object methods. Recheck the overlap with #539. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 16. JavaScript line-comment termination — PR #544
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/544

Behavior: Review the reported javascript line-comment termination problem and produce its
verified disposition under the shared review contract.

Baseline observation: b has CCN 3; expected 4, with NLOC 8.

Proof: A backslash does not extend a JS line comment into the next line. Preserve C/C++ continuation behavior if shared comment logic changes. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 17. Conditions in template interpolations — PR #545
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/545

Behavior: Review the reported conditions in template interpolations problem and produce its
verified disposition under the shared review contract.

Baseline observation: label has CCN 1; expected 3.

Proof: Interpolation expressions contribute CCN in JS, TS, JSX and TSX while balanced scanning preserves literal text, nested templates, regex literals and function boundaries. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 18. Keyword member names — PR #546
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/546

Behavior: Review the reported keyword member names problem and produce its
verified disposition under the shared review contract.

Baseline observation: load has CCN 2; expected 1.

Proof: Member names catch/if/for/while/case add no conditions; corresponding control-flow constructs still do. Inspect condition-counter consumers and optional member access. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

### 19. Ternary and optional markers — PR #547
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/547

Behavior: Review the reported ternary and optional markers problem and produce its
verified disposition under the shared review contract.

Baseline observation: pick has CCN 1; expected 2.

Proof: A ternary without spaces counts in JS/TS; optional fields/parameters do not add conditions in TS/TSX. Preserve optional chaining and nullish coalescing. Run the JS/TS focused command below plus any additional shared consumers reached by the actual change.
Retain the slice's performance, CI and disposition evidence; applicable full
checks remain required before merging.

## Focused JS/TS proof

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testTSX.py
```

This group covers the shared runtime states and tokenizer consumers rather than
only added tests. #544 also runs C/C++ comment tests if shared code is touched;
#545 includes existing balanced-template tests in this group and regex guards;
#546 covers the core condition-counter consumers located by the actual change.
A core FunctionInfo change expands to parameter consumers across languages.
The full merge gate catches broader regressions but does not replace a missing
specific observation.

## Promise ownership

| Source promise | Owner and observation |
| --- | --- |
| Every selected PR accounted for | Slice 1 freezes the set; each corresponding slice records its disposition; last disposition checks completeness |
| Worthwhile correction and coherent affected design | Each slice's reproduction, benefit assessment, consumer trace and independent refactor |
| Correct public metrics and relevant boundaries | Each slice's examples and discriminating analyzer/CLI regression tests |
| Strictly below 3%, including combined effects | Every merging slice's paired incremental/cumulative workload comparison |
| Corrections above fifteen minutes deferred | Each slice's revised estimate and published linked backlog story |
| Final tested revision and required CI before merge | Each merging slice's final head/base record, full suite, format checks and hosted checks |
| Thank contributor after merge | That slice's confirmed merge and thank-you comment URL |
| Durable disposition evidence | Each slice's evidence retained before wrap-up removes planning history |

## Current decisions, sizing and concern review

There are 19 independent disposition loops, one per currently selected PR.
Combining them merely by language would hide independent correctness,
performance and merge gates. Each loop is bounded by one PR; correction work
is bounded by the source's estimate cutoff, with review, measurement and CI
time explicitly outside that cutoff. No additional slice-time policy is
invented. Larger repairs become follow-up stories without changing this review
mission's obligation to account for their PRs.

The cumulative design follows the existing shared tokenization, expression,
binding and metric owners; it does not prescribe nineteen isolated fixes.
Later slices must reconsider overlap with earlier accepted changes. No
preparatory Structure slice or new abstraction is justified before review.

Slice-plan refinement was not needed: each slice already has one disposition,
one proof loop and a safe stopping point; shared gates are specified once.
No remaining slice-specific planning concern was identified. Credentialed
publication/merge probes retain the stop behavior above and do not imply
current execution authority.

## Learnings

Preparation reproduced all first examples on the current checkout, while the
existing suite passed. Some earlier PRs have already been superseded (the
reports identify #527 replacing #503 and #528 replacing #504); only the
refreshed selected set is owned here. Claimed dependencies #533/#534 on #531
and #535 on #532 remain implementation claims to verify, not backlog supplier
agreements. PR #539 and #543 overlap in spread-expression handling.
