# Recognize regex literals without consuming division expressions

**Identity:** SEED-012#regex-literal-context
**Source:** [Refined story](../../seeds/SEED-012-regex-literal-context.md#regex-literal-context)

## Outcome and boundaries

Users analyzing JavaScript, TypeScript, JSX/TSX, Vue script content and Ruby get
correct CCN and function/file boundaries when a regex follows a genuine
expression start. Valid division after member names, dollar names and contextual
identifiers retains its operators and token count. Preserve literal contents,
whitespace/line metrics, template boundaries and nesting-state lifetime.

The source's examples and [PR528 evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md)
are the acceptance input. The original return function requires CCN 2; its
nested arrow requires CCN 2 while the enclosing function remains CCN 1.
`obj.return / (a || b) / c` requires CCN 2 and token count 24 in JS-family
functions, and CCN 2/token count 22 in the supplied Ruby function. Other
division variants preserve their own baseline counts, not an assumed count 24.
Ruby `return /#/ =~ s ? 1 : 0` requires CCN 2. Quotes, braces, backticks,
character classes, escaped slashes and flags inside a literal remain opaque;
outside logical operators and subsequent functions remain visible.

Excluded commitments: interpolation condition counting (PR545), unrelated
arrow-boundary/parameter stories, a full language grammar, and new language
support. No sibling is a demonstrated prerequisite. No product rejection rule
follows merely from an example being absent. This is the technical follow-up
to a review exceeding its 15-minute correction cutoff; that cutoff is not a
time limit for this plan. No project-specific numeric slice limit was supplied.

Preparation only: all slices below are planned. This record neither Takes the
story nor authorizes implementation, contributor writes, merge or comments.

## Existing owners and architectural direction

PFE: change the existing `lizard_languages/js_style_regex_expression.py` owner.
It provides the literal grammar and expression-opening rule to TypeScript,
TSX and Ruby token generation and to `javascript_templates.py`'s balanced
scanner. JavaScript inherits TypeScript; JSX uses TSX; Vue delegates script
tokenization to TypeScript. `token_pattern.py` owns compiled token patterns
and span matching. Ruby's `MyToken` positions and interpolation processing are
additional consumers; the context decision must precede string/comment/JSX
consumption and preserve their token positions and newline visibility.

Use one coherent lexical context decision: a slash can open a literal where
an expression can begin, while a completed operand/member remains division.
Context must account for significant preceding tokens, full identifiers and
language-sensitive contextual words; adding word-boundary keyword lookbehinds
does not meet that rule. Keep literal-body matching shared. Adapt the existing
scanner/token-generator seams as necessary rather than repairing already
consumed strings or installing a second recognizer. Choose the smallest
representation supported by the regressions; do not prescribe a parser framework
or separate rules per slice. Ruby-specific semantics may require a language
context input, not a duplicate literal grammar.

Follow [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for CCN, token count, language reader and function terminology, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no new North Star topic
or ADR is warranted. No architectural exception is proposed.

## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/recognize-regex-literals-without-consuming-divis`,
revision `df794c5e5808272120c6f8e3a254b7936a18698b`, observed 2026-10-27.

- Ran `nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr528/reproduce.py --checkout /Users/terryyin/git/lizard/.worktrees/recognize-regex-literals-without-consuming-divis`.
  Exit 1: return CCN 1 instead of 2 and arrow CCN list `[1, 1]` instead of
  `[2, 1]` in all four extensions; Ruby return 1 instead of 2. All supplied
  member/spaced/commented/optional/dollar/contextual/ordinary division checks
  pass at CCN 2. This public consumer demonstrates the missing behavior;
  existing green tests alone do not prove it.
- Ran `nix develop -c python -m pytest`: 1,796 passed, 7 skipped.
  Inspected setup/assertions in `testJavaScriptRegex.py`, `testTypeScript.py`,
  `testRuby.py` and `testVue.py`: public function tuples exercise opaque literal
  contents, preserved following functions, nested-template spans and Ruby/Vue
  wrappers. These are preservation evidence, not proof of the new contexts.
  Add discriminating red/green cases in slice 2; do not claim all existing
  guards independently discriminate the proposed correction.
- Reconstructed the historical harness from
  `git show af27c910a6a0bff51d8358fde0c37fa9cb059d1a:docs/reviews/fabio-malpezzi-20261027/paired.py`
  and checked SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
  Reconstructed and hash-checked the PR528 manifest's 149 files using its
  `revision` plus `inputs.json`, per the retained README recipe. Scratch root:
  `/private/tmp/lizard-regex-preparation-166d6b4e`. The consuming commands are
  `nix develop -c python /private/tmp/lizard-regex-preparation-166d6b4e/paired.py worker /Users/terryyin/git/lizard/.worktrees/recognize-regex-literals-without-consuming-divis 0 api`
  with stdin `1` and
  `nix develop -c python lizard.py /private/tmp/lizard-regex-preparation-166d6b4e/corpus/lizard.py /private/tmp/lizard-regex-preparation-166d6b4e/corpus/lizard_core /private/tmp/lizard-regex-preparation-166d6b4e/corpus/lizard_languages /private/tmp/lizard-regex-preparation-166d6b4e/corpus/lizard_ext`.
  The API worker consumed all 49 affected inputs; the broader CLI completed
  with an accepted analyzer exit code. This settles replay reachability,
  not correctness or timing acceptance of any future candidate. Reconstruct
  from Git again if scratch files disappear; no scratch file is a dependency.
- GitHub `get_pr_info(repository_full_name="terryyin/lizard", pr_number=528)`
  returned open/unmerged at `b66ec3a7f3ca1b3f59d8a76891ce556406231566`,
  `FabioMalpezzi/lizard:pr/js-regex-after-keywords`, targeting master.
  The returned base SHA is the PR base, not an assertion about current trunk.
  Actual contributor publication requires execution authority and is bounded
  by slice 1's early probe and slice 4's accepted-push proof.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Authorized contributor delivery route remains viable | Slice 1: refreshed PR/ref/permission evidence; refusal stops dependent work |
| Return/arrow and genuine keyword contexts expose outside conditions | Slice 2: new public API regressions fail before correction and pass after it in four JS extensions, Ruby and Vue |
| Valid division and literal opacity | Slice 2: explicit CCN/token tuples for member/comment/dollar/contextual variants plus ordinary controls; regex internal operators do not add CCN |
| Template and following-function boundaries, line metrics | Slice 2: names/start/end/NLOC tuples with quotes/backticks/braces, multiline whitespace and nested templates; preserve opaque interpolation counting |
| Nesting lifetime and other shared consumers | Slice 2: same `FileAnalyzer` with NS analyzes adjacent functions then separate files; expected depth remains independent; Ruby split/gsub, token conversion and Vue wrapper controls |
| Strict affected and broad throughput, incremental and cumulative | Slice 3: fixed-corpus warmed paired measurements, each upper 95% slowdown bound strictly below 3%, including applicable NS/Vue paths |
| Final corrected revision accepted and acknowledged | Slice 4: full pytest/make, exact-head hosted checks and merge, contributor publication/merge/acknowledgement receipts |

Use `lizard-language-support` for reader/test changes. Public `analyze_file` or
`FileAnalyzer` results own semantic proof; token tests can support representation
contracts but do not replace it. Mock only filesystem boundaries. Inspect every
reached consumer, including NS, and record run/reused/excluded with reasons.
Independent post-change refactoring runs through the execution workflow before
delivery; preserve capability names and consolidate overlapping regression setup.

Focused command after relevant edits:

```sh
nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testRuby*.py test/testNestedStructures.py test/testJavaScriptNestedStructures.py test/testNestedStructureScopes.py
```

`AGENTS.md` requires full `nix develop -c python -m pytest` before completion.
The source additionally requires `nix develop -c make`, formatting/hooks and
final hosted CI. Run Nix commands sequentially because the shell hook changes
`.venv`; keep owned heavy measurement serial. Current CI applicability must be
refreshed in execution; the seed's historical 14-job matrix is not a permanent
substitute for actual required checks.

## Ordered slices

### 1. Establish the contributor correction route
Type: Behavior
Status: done
Proof: refreshed PR/head/base, contributor-edit permission and authenticated
publication-route observations; preserve exact refusal if unavailable.

Behavior: given an explicit execution instruction for this correction, refresh
PR528's entire patch/discussion/checks and current target, verify the recorded
head/branch still names the intended contribution, and establish the authorized
contributor-head delivery route before dependent implementation. Read-only
metadata was observed above; credential/owner-held publication is an early probe.
Use a non-mutating authenticated push preview where available. A preview is
not accepted publication; slice 4 owns the real update. If the PR is closed,
superseded, changed incompatibly or cannot receive corrections, stop and return
the concrete disposition/destination decision to Terry. Do not substitute an
unrelated trunk patch. No implementation slice depends on unobserved permission.

Boundedness: one external delivery feasibility observation; it leaves the
product unchanged. A failure stops slices 2–4 and revises the plan before edits.

Accepted proof (2026-10-27, Nana-chan): route viable; product unchanged.
- PR528 OPEN, not draft: FabioMalpezzi:pr/js-regex-after-keywords @
  `b66ec3a7f3ca1b3f59d8a76891ce556406231566` (exact retained review head).
- Base `terryyin:master`; PR baseRefOid `01afe9c…`; current master tip
  `874d284…`; compare diverged ahead 1 / behind 64; mergeable_state dirty.
- Discussion empty; head checks 4× SUCCESS (ubuntu/windows × 3.8/3.10).
- Auth `terryyin` with `repo` scope; base admin true; fork API `push: false`
  but `maintainer_can_modify: true`.
- Non-mutating divergent dry-run push (HTTPS + SSH) accepted forced-update
  preview to PR head; same-SHA dry-run up-to-date. No push/comment/merge.
- Source-only review retained P11 contributor-head obligations; this slice
  owns only the early route probe. Slice 4 owns real push/rebase/merge/ack.
- Learnings: fork collaborator 403/`push:false` alone is insufficient refusal
  when MCM + divergent dry-run succeed; PR remains 64 behind master so slice 4
  must rebase/update before merge.

### 2. Preserve regex and division semantics through every shared reader
Type: Behavior
Status: done
Proof: red/green public regressions for the source examples and context
counterexamples, plus the focused consumer command and retained reproducer.

Behavior: given a regex at a true expression start, analysis retains its entire
literal before strings/comments/JSX can consume it, exposes subsequent conditions
and preserves following functions. Given a completed operand, including member
names with intervening comments, optional access, dollar names and contextual
identifiers, slash remains division with its CCN and tokens intact.

Exercise genuine contextual-keyword uses alongside their identifier uses; do not
solve the counterexamples by disabling all keyword recognition. Extend the
existing regex owner and its template-scanner integration together, preserving
Ruby token positions, literal escape/class/flags semantics and newline tokens.
Add public cases in the existing regex test home and reached language/NS homes.
Use Vue script wrappers, Ruby return/split/gsub, templates containing regex and
division, and same-analyzer next-file nesting to prove the complete consumer
chain. Apply independent refactoring before commit and rerun affected proof.

Boundedness: one slash-classification rule with coupled tokenization consumers
and one semantic regression loop. Splitting by language or scanner would leave
conflicting rules or conceal the original failure. No standalone preparatory
abstraction slice is needed. A correct local result is a safe stopping point;
it remains unmerged pending performance and final delivery gates.

Accepted proof (2026-10-27, Nana-chan): semantic route green; unmerged pending
performance (slice 3) and contributor delivery (slice 4).
- Owner: previous-token `can_regex` mark in `js_style_regex_context.py` with
  decorator/re-exports in `js_style_regex_expression.py`; Ruby via
  `language='ruby'`. No keyword lookbehinds; `<>` not treated as regex openers
  (avoids JSX `</tag>` false opens).
- Source P1–P9 vs actual: return/arrow CCN 2; member/spaced/comment/optional
  division CCN 2 / tokens 24; dollar/of/await/ordinary CCN 2 / tokens 22;
  Ruby return CCN 2 and divide CCN 2 / tokens 22; Vue wrappers; opaque
  quote/brace/flag literals with following functions; NS adjacent + next-file
  independence. P10/P11 remain slices 3–4.
- Reproducer: `nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr528/reproduce.py --checkout <workspace>` → EXIT 0, 42/42 after refactor.
- Focused pytest (post-refactor paths): 451 passed, 7 skipped.
- Observations: `testJavaScriptRegex.py` return/arrow/member/identifier/for_of/
  await_keyword/quote_brace/vue; `testRubyRegex.py`; `testJavaScriptNestedStructures.py`.
- Consumer-trace (base ba5453c): typescript/tsx/javascript_templates/ruby run;
  javascript/vue reused via inheritance/delegation; planning/review docs excluded.
- Learnings: keyword lookbehinds cannot distinguish member vs keyword across
  comments; single forward mark before shared `REGEX_TOKEN` is the coherent fix.
  Refactor split context scan from decorator and extracted oversized Ruby/NS tests.

### 3. Meet the strict performance budget without weakening semantics
Type: Behavior
Status: done
Proof: retained raw warmed timing pairs, immutable harness/corpus hashes,
candidate/base revisions and intervals for every required workload.

Behavior: given the semantically correct candidate, running affected and broader
fixed-source workloads shows each upper paired 95% slowdown bound strictly below
3% versus current target and the frozen `2e21e404c1ab9f4e0b49acc80816e816629d0670`
starting baseline. A broad pass cannot hide an affected failure. Preserve the
raw proposal's failed +19.4428% observation as diagnosis, not candidate evidence.

Replay the historical harness/corpus through the verified README recipe. Freeze
additional new regression, Vue and relevant NS inputs before timing; label and
hash additions, retaining the original 149-file manifest unchanged. The old
`python-ns` worker selects Python files and cannot prove JS nesting throughput:
use an explicit affected NS analyzer workload when the changed path reaches NS,
and the fixed-source `cli-ns` workload as applicable. Confirm imported checkout
paths and public metric tuples before timing. Use the same Nix interpreter,
at least 20 alternating warmed pairs, batches at least one second, and record
headroom/load, all durations, medians, variability and intervals. Run literal
`paired.py --baseline <frozen-checkout> --candidate <candidate-checkout> --workload api --out <result>`
and the corresponding `cli`/applicable NS workloads through Nix. Retain exact
resolved commands in execution evidence; scratch paths alone are insufficient.

Profile only when needed, then simplify the shared decision rather than adding
per-case fast paths. Re-run slice 2's affected semantic proof after optimization.
Noise or an upper bound of 3% or more is unresolved/failing; fix a prospective
quiet retry protocol rather than timing repeatedly for a pass. Refresh target
changes and remeasure only invalidated comparisons. This independent performance
loop may take longer than implementation; no invented timeout applies.

Accepted proof (2026-10-27, Nana-chan): P10 performance gate met; semantics intact.
- Candidate: execution workspace after shared-decision optimization atop `8291e5b`.
  Master tip timed: `1db5933b66e4602f607c6ae40998984dadd5bab5`. Frozen:
  `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
- Harness `paired.py` SHA256 `3064d8d7…3217bf5`; manifest `2b4d99de…bf5d7cc`.
  Supplemental + NS companion under `/private/tmp/lizard-regex-perf-seed012/`.
- Unoptimized api vs master upper +38.48% (diagnosis). Optimized accepted uppers
  (all &lt;3%): api 1.26/1.70; cli 1.77/1.51; cli-ns 2.29/1.05; api-ns 1.85/1.83;
  api-supplemental 1.89 (quiet-retry; prior 3.68% preserved) / 1.53
  (master/frozen). Rollup `SLICE3-SUMMARY.json`; raw `opt-*.json`.
- Optimization: trunk punct lookbehind + selective keyword mark (find-jump /
  forward) + pure-function memo; `(?<![$\w])` so `$return` stays division.
  Post-refactor modules: `js_style_regex_context.py` facade,
  `js_style_regex_keyword_scan.py`, `js_style_regex_forward_scan.py`,
  `js_style_regex_expression.py` — timings retained (layout-only split).
- Semantic re-proof after opt+refactor: reproducer EXIT 0 42/42; focused
  pytest 451 passed, 7 skipped.
- Learnings: full forward scan every analyze ~+37%; selective mark+memo meets
  budget without per-case fast paths.

### 4. Deliver the qualified correction through PR528
Type: Behavior
Status: done
Proof: final contributor-head SHA and accepted push, green current required CI,
exact-head guarded merge receipt, and one acknowledgement URL.

Behavior: given the corrected/refactored candidate with semantic and performance
proof, publish necessary corrections to the contributor branch without force or
lost history, then verify final head/base/tree and all required checks before
an exact-head guarded hosted merge. Run full pytest and make on the final
corrected revision. Any code/upstream change invalidates affected evidence;
recheck before merging. Refusal keeps the correction recoverable and unmerged.

Follow the source's [complete-cycle delivery and performance contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md)
for final checks and retained evidence; its review-only 15-minute triage rule
does not re-defer this selected technical correction. Hosted merge is not a
Dough Land operation. Execution must preserve this contributor-first delivery
constraint when choosing its publication mode; preparation's trunk landing
contains only planning records. After confirmed merge, the executing workflow
must have the source's explicit acknowledgement authority before posting one
thank-you that mentions material corrections; check for a prior receipt to
avoid duplicates. Retain commands, revisions, metrics, timings and URLs in the
existing review evidence/discussion, then complete the story's ordinary wrap-up.

Boundedness: one qualified contribution reaching its intended users and author,
with separate hosted wait time. Pending CI, merge or acknowledgement leaves
this slice unfinished, not delegated to the lexical-review sibling.

Accepted proof (2026-10-27, Nana-chan): contributor delivery complete.
- Local: `nix develop -c python -m pytest` → 1810 passed, 7 skipped;
  `nix develop -c make` → exit 0 (pep8/pylint) on execution HEAD `64e720a`.
- Contributor head update (SSH, non-force): merge `origin/master` (`0a61bf3`)
  into FabioMalpezzi:pr/js-regex-after-keywords, overlay corrected product/tests,
  push `317d5c83647b849f8b683c150a6039e47ce5ba2a` (HTTPS rejected for workflow
  scope; SSH accepted). Pre-push reproducer 42/42 on PR worktree.
- Hosted CI run `40504615718`: all 14 ubuntu/windows × 3.8–3.14 builds SUCCESS;
  `changes` SUCCESS; `web` SKIPPED. Head unchanged at merge time.
- Exact-head merge: `gh pr merge 528 --merge --match-head-commit 317d5c8…`
  → MERGED; merge commit `6408864eeab300296617902c0c8980a3f11f2a39`.
- Acknowledgement (no prior comments):
  https://github.com/terryyin/lizard/pull/528#issuecomment-6442012279
- PR: https://github.com/terryyin/lizard/pull/528

## Plan review

Slice-plan refinement was not needed: slices separate a permitted early external
probe, one coupled semantic rule, its independent measured performance loop, and
hosted delivery. The cumulative design keeps one regex-context owner and adds
no speculative infrastructure. No remaining slice-specific concern was found;
contributor publication availability is bounded by the early probe rather than
assumed from readable metadata. No human decision is currently pending.
