# Shared review contract

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
   Reuse the [existing owners](CONTEXT.md#existing-solutions-and-architectural-constraints); no PR-specific switches or parallel counts.
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
