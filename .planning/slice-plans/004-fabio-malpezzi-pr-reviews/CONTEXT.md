# Review context

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
  the discrepancies recorded in [their owning slices](SLICE-PROOF.md). They establish
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

## Execution context

Execution started 2026-10-27 in the established linked worktree
`/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr`,
branch `codex/review-correct-and-merge-fabiomalpezzi-s-open-pr`, Story Branch Mode.
Publisher: `dashboard-territory.local-lizard`; agent: Rio-chan.
Starting revision: `e8d62aeaf00d2d0bcb19cc3d11b7ba1843c2f372`.
Published claim and first delivery base: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
The authorized remote is origin; integration target is master. The default
checkout is `/Users/terryyin/git/lizard`; this execution owns only its linked
worktree. Fetch confirmed the claim on origin/master at startup.
The refreshed open queue contains the same 19 PRs. Exact heads, source branches,
PR base revisions and states are frozen in [execution-queue.json](execution-queue.json).
The frozen starting target is `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
No commit hooks are installed; Makefile style checks are check-only, with no
configured selective formatter. Nix's shellHook prepared this checkout's .venv;
`nix develop -c python -m pytest --version` passed (pytest 8.4.2).
Replanning retains existing planned-work authority; the source's fifteen-minute
correction cutoff governs each review. Claim CI on master is unobserved here.
CI selector: python-app.yml, verified active, workflow ID 38306292, Python application.

CI observer: coordinator `rio-chan-pr-review`, directory
`/tmp/dough-ci-501/watch-zjUjlZ`, PID 58494, stream session 38538,
yielded cell 35. It observes this execution branch. Slice1 was accepted at
`af27c910a6a0bff51d8358fde0c37fa9cb059d1a` on that branch, suffix base
`2e21e404c1ab9f4e0b49acc80816e816629d0670`; managed delivery reused this stream.
At slice2 startup, the previously published base is af27c910. No rebase occurred.
The first two stream launches with login shells produced no observer/mailbox;
their cells 10 and 27 were closed after confirming no matching process/request.
Launch with `login:false` succeeded. Keep that setting for shell operations.
Contributor correction publication uses the isolated owned candidate checkout
`/private/tmp/lizard-pr527-corrected` at PR #527's frozen head, without changing
the selected execution branch. It has no development dependency installation;
only already-validated tests were copied and committed through agent-commit.
It is not a second execution workspace. Non-forced Git push advanced the fork
head from a529750 to 5067a85; GitHub confirms that exact head and open state.
PR527 then merged as e871ae448d16893301856b41124aba34496e9536; the contributor
candidate checkout was clean and its commit is contained on origin/master.
That temporary candidate checkout has been retired. A separate clean read-only
trunk-evidence checkout at /private/tmp/lizard-review-trunk-evidence supports
identity queries; it is now detached at7ac449f and has no runtime installation.
Trunk advanced to7ac449f through the separate Python-support story. Its diff
changes CI/claims/records and a metadata test; analyzer sources are unchanged.
Refresh future merge gates to Ubuntu/Windows×Python3.8–3.14 (14jobs).

Slice2 delivered `16eed2248cf873d4b7bec42792198378a4e73ad8` on the execution
branch, suffix base af27c910; observer reused, no rebase. It publishes the
SEED-012 regex correction story and complete PR528 deferral evidence. This is
slice3's previously published base. PR528 remains open at frozen b66ec3a7.


Preparation reproduced all first examples on the current checkout, while the
existing suite passed. Some earlier PRs have already been superseded (the
reports identify #527 replacing #503 and #528 replacing #504); only the
refreshed selected set is owned here. Claimed dependencies #533/#534 on #531
and #535 on #532 remain implementation claims to verify, not backlog supplier
agreements. PR #539 and #543 overlap in spread-expression handling.
