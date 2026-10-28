# Complete PR review cycle

This contract applies independently to each story in the resplit FabioMalpezzi
review cohort. The original execution-start set is fixed in frozen-review-set.json;
each story owns only its named subset. Later PR arrivals are outside this scope.
Refresh a selected PR's state/head before action and retain a disposition if it
closes or is superseded. A dependency claim is a hypothesis to verify.

## One complete cycle per PR

1. Read the entire diff, discussion, reviews, checks and dependency claims.
   Reproduce through the public analyzer or CLI on the current target. Establish
   valid expected metrics, names/boundaries, whether the defect remains real,
   and whether its benefit justifies change. Record reasons for unnecessary work.
2. Inspect the whole affected concept and shared-language consumers, including
   untouched callers. Reuse existing owners and Accepted ADR-0000/0001. Do not
   add parallel parser counts or unrelated redesign. Meaningful changes retain
   installed consumer traces and each consumer's run/reused/excluded disposition.
3. Estimate complete correction and cleanup before product edits. Exactly 15 minutes
   remains eligible. Above 15 minutes, or a revised estimate above 15 minutes, create/reuse
   an evaluable canonical follow-up with beneficiary, reproducer, expected result,
   remaining work and source PR link; leave the inadequate PR unmerged. Preserve
   only owned incomplete work. Review/measurement time is excluded from the limit.
4. Correct eligible defects coherently. Prove discriminating public API/CLI
   regressions red then green, preserving relevant valid boundaries, accepted
   cases and other reached languages. Mock only filesystem boundaries. Retain
   actual setup failures and explain corrected fixtures; never weaken assertions
   to disguise a defect. Do not publish intentionally failing live tests.
5. Accept semantic proof against the source promises, including a fresh source-only
   comparison required by execution. Run fresh independent post-change refactoring
   through the execution workflow. No edits means no redundant verification.
6. Pass the final corrected revision's full `nix develop -c python -m pytest`,
   `nix develop -c make`, applicable formatting/hooks, and all required current CI.
   Refresh workflow/matrix/conditional-job applicability. Local and hosted proof
   complement each other; neither substitutes for the other.
7. Publish necessary corrections to the contributor head without force or lost
   history. Verify final head/base/tree and guard the hosted merge by exact head.
   Hold an unpublishable correction instead of substituting an unrelated trunk
   patch. A code, dependency or upstream change invalidates affected proof.
8. After confirmed merge, post the authorized thank-you acknowledging material
   corrections and retain its URL. Avoid duplicate messages on retry. Preserve
   durable commands, revisions, corpus, findings, tests and performance evidence
   in the discussion/record; rejected proposals keep their reason and follow-up.

Final disposition is one of: merged with acknowledgement; unnecessary/not worth
fixing with evidence; or a real defect whose inadequate proposal is left unmerged
with its correction above 15 minutes linked. A pending correction, refactor, performance,
CI, merge or acknowledgement is unfinished work in THIS story, never a QA phase
passed to another review story. A follow-up technical correction is a concluded
review disposition under the explicit time policy, not an unfinished acceptance.

## Performance protocol

Freeze exact candidate/target revisions, literal harness/options and immutable
source/corpus hashes before timing. Include the complete report/authored cases,
nearby regressions/controls and shared consumers. Use warmed affected API analysis
and relevant public-name/count consumers plus a broader fixed source workload.
The original broad 100-file corpus is reconstructible from starting revision 2e21e404;
earlier per-PR records retain its manifest and literal historical harnesses.

Use one machine/Nix environment/interpreter/options per comparison, separating
setup from analysis. Inspect cost/headroom before launch; keep owned heavy work
serial. Calibrate batches to at least one second, alternate at least 20 paired warmed
batches, and retain every duration, medians, variability, change and paired 95%
interval. The upper slowdown bound must be strictly below 3% for each required
workload. Exactly 3%, a larger result or unresolved noise does not pass. A broad
pass cannot average away an affected failure. Profile when useful, qualifying
causal inference. A justified longer/quiet retry is fixed prospectively; preserve
earlier inconclusive results. Do not repeatedly time invalid code to seek a pass.

Measure incremental versus current target and cumulative accepted-set cost versus
the starting baseline. Each merging PR owns combined validation BEFORE merge;
there is no end-of-cohort waiver. Include applicable extensions such as CLI-ENS
when a change reaches the accepted nesting path. Reuse evidence only while its
promise, code, input, setup and consuming boundary remain compatible. Supplemental
untimed cases must be labelled and included in corrected-candidate timing when
relevant. Current-target changes require reassessing the affected evidence.

## Focused JS/TS proof

Run the affected reader tests and expand to every actual shared consumer:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
```

This focused selection complements the required full suite and `make`.

## Runtime and proof integrity

Project tooling uses Nix; Git is exempt. ShellHook mutates .venv, so even read-only
Nix wrappers must be sequential. Snapshot facade imports can insert argv[0]'s
directory: use isolated real scripts and assert actual facade/reader/core/helper
paths before/after collection. Python 3.9 cannot use extractall(filter=...). Legacy
snapshot profile.py shadows stdlib cProfile; launch profiles from /private/tmp.
Retain exact patches as escaped JSON or immutable Git reconstruction; normalized
display diffs are never replay inputs. Check new/untracked evidence whitespace
and EOF, not only tracked `git diff --check`. Use the actual checkout's installed
product-backlog Git adapter before merges/rebases/picks touching backlog/done.

Existing owners are JavaScriptFunctions for signatures, JavaScriptBindings for
binding/default ownership, FunctionInfo for public parameter data, shared regex
and template scanners for lexical context, CCppCommentsMixin for comment consumers,
and lizardns/Python indentation for nesting. Recheck actual reach rather than
creating PR-specific switches. Follow docs/adrs/0000-use-adrs-accepted.md and
0001-ubiquitous-language-accepted.md; Terry confirmed ADR-0001 Accepted on 2026-10-27.

## Story independence and cohort completion

Each story can close when its own PRs have complete dispositions, even if another
review batch is deferred. No sibling is a prerequisite merely because of order.
Verify actual technical dependencies and record only justified supplier agreements.
Whichever review story finishes last also reconciles all 19 frozen PRs in story-map.md
and retains the final combined summary, reusing compatible prior evidence. This
does not defer any individual PR's pre-merge combined checks to that last story.
Later mapped plans await story refinement and realignment; this contract grants
neither Take nor execution. Current workflow is refreshed when that story starts.
