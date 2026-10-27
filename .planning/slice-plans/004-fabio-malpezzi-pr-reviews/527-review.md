# PR #527 review evidence

Status: merged e871ae448d16893301856b41124aba34496e9536; all gates proved; slice delivered at af27c910a6a0bff51d8358fde0c37fa9cb059d1a.
Source: https://github.com/terryyin/lizard/pull/527
Frozen head: a529750edc95d44bc71fbcd88d567bbbef865ae5.
Reviewed corrected head: 5067a85ee8d95870d1f55561b1d468cd5421169f.
Target and cumulative baseline: 2e21e404c1ab9f4e0b49acc80816e816629d0670.

## Expected and observed behavior

The current target reports Python nesting depths 1,2,6 for the three source
functions; valid structures require 1,1,0. The author's production patch yields
1,1,0, preventing false warnings for independent functions/comprehensions.
It reuses PythonReader indentation and the NS extension's existing metric owner,
consistent with ADR-0001. No public contract or other reader is changed.

Public FileAnalyzer observations cover nested functions (inner1/outer2), two
successive files through one analyzer (depth1 each), independent class methods,
match/case, soft match variables, async with/for, ternary/comprehension exclusions,
continuation lines and multiline fstrings. Actual CLI --NS1 produces no warnings;
--NS0 warns for both depth1 functions and exits1.

The author's patch remains unchanged in production. Reenabled fixtures now use
valid Python except clauses; additional lifetime/soft-keyword guards are present.
Independent refactoring split C++ basic, C++ scope/literal and Python suites into
files below250 lines, preserving every assertion. Correction estimate and actual
active edits are about13 minutes; review and measurement are excluded.

## Proof

- Red original production: `nix develop -c python -m pytest test/testNestedStructures.py -q`
  =>14failed,38passed,2skipped. Original production was compiled into a read-only
  comparison; no parsing mocks. Log /tmp/lizard-review527-red-final.log.
- Final focused: `nix develop -c python -m pytest test/testNestedStructures.py test/testNestedStructureScopes.py test/testPythonNestedStructures.py test/test_languages/testPython*.py test/test_languages/testGDScript.py -q`
  =>170passed,2skipped. Observations: testPythonNestedStructures.py tests
  test_structures_do_not_leak_between_files, test_nested_function_restores_outer_structures,
  test_match_variable_is_not_a_structure plus the reenabled/author tests.
- `nix develop -c python -m pytest test/testExtension.py test/test_options.py -q`
  =>22passed, including simultaneous-extension and threshold consumers.
- Final `nix develop -c python -m pytest` =>1687passed,7skipped in2.46s.
- Final `nix develop -c make` =>exit0: full coverage/test, pycodestyle and pylint.
  Pylint is configured --exit-zero; score9.11/10, unchanged. Logs:
  /tmp/lizard-527-final-pytest.log and /tmp/lizard-527-make.log.
- `git diff --check` =>pass.

Actual FileAnalyzer setup is in testHelpers.get_python_function_list_with_extension;
it invokes get_extensions([NestedStructure()]) and analyzes real source. No
fixture supplies the resulting metric. Refactor suite discovery rerun passed.

Consumer-trace terms: max_nested_structures, get_python_function_list_with_extension,
process_cpp, process_python and both test classes. options.py, CLI, testExtension,
Python tests/testHelpers are run or reuse the observations above. Halstead and
FunctionStatementCount helper callers use different extensions, so the changed
NS path does not execute. Lifecycle/planning/history matches consume no NS metric.
Full classified traces: /tmp/lizard-review527-consumer-trace-full.log and
/tmp/lizard-review527-refactor-trace-classified.md. No uncovered consumers.

## Performance

Literal harness, fixed corpus manifest/input bytes and raw timings are retained
under docs/reviews/fabio-malpezzi-20261027. External fixed corpus and baseline:
/Users/terryyin/.cache/lizard-pr-reviews-20261027/{corpus,baseline}.
Starting manifest SHA256:
f8df53399291c86afae3622acbf91e953eef3b1b4de1817ca8433a9de4cedd8e.
119files: starting target's analyzer sources and19affected source examples.

Territory.local arm64,48GiB, Python3.9.11 in the same checkout Nix .venv for both
revisions. Initial load6.50/5.58/6.78; no observed memory pressure failure. Sequence
paired workloads,20alternating pairs, warmed both, batches calibrated above1s.
Full tests ran briefly during initial API calibration; no other owned heavy
commands ran during CLI measurement. Other active story worktrees are known;
their workload is unknown. Reconsider on contention, failures or wider CI change.
Nix entries must run sequentially because shellHook repeats dependency setup.

Commands from execution checkout:
`nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline --candidate /Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr --workload python-ns --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/527-python-ns.json`
and the same command with workload cli-ns and out527-cli-ns.json.

| Workload | Baseline/candidate median seconds | SD baseline/candidate | Slowdown | Paired95%CI |
| --- | --- | --- | --- | --- |
| Python NS API,6940loops | 1.14102/1.12773 | .00832/.00873 | -1.165% | [-1.532%,-.750%] |
| Full CLI -ENS,7loops | 1.34461/1.34277 | .01693/.02808 | -.136% | [-1.384%,.543%] |

Both upper bounds are strictly below3%. Incremental and cumulative comparisons
are identical for this first candidate; no earlier PRs have merged. Unchanged
default analyzer path does not load NS. No product edits occurred after timing;
only tests were moved. Git diff01afe9c..2e21e40 has no product/test/workflow changes,
so the corrected fork head and current-target candidate have identical code.

## Acceptance and publication

Independent original-source commitments were returned by source_review_1 in the
conversation, separate from implementation/refactoring. Reproduction, user value,
whole-concept review, source-dependent correction cutoff and regression/performance
proof are met above; final CI/guarded merge/thank-you remain with slice1. Other
PRs remain with their ordered slices. The source's unnecessary/long-correction
alternatives were considered and do not apply to this real13minute correction.
Combined-set promise is currently this one candidate; revalidate after relevant
upstream change. Independent refactor_527 returned ## REFACTOR COMPLETE.

Read-only GitHub metadata: maintainer_can_modify=true; connector fork push=false.
The necessary publication used Git over SSH, no force, preserving original head
as ancestor. agent-commit produced5067a85. Actual push succeeded and GitHub reports
that exact open/mergeable head. Required hosted CI must validate this revision;
the old a529750 run40309841538(success) is superseded and cannot accept5067a85.

Final hosted run40448476204 passed all four Ubuntu/Windows by Python3.8/3.10 jobs.
Exact-head guarded merge accepted5067a85 as e871ae448d16893301856b41124aba34496e9536.
Thank-you and review evidence: https://github.com/terryyin/lizard/pull/527#issuecomment-6433608236.
The final artifact refactor review then found that the API performance workload
omitted nearby added boundary controls. This was a coordinator acceptance error:
the merge preceded complete performance proof. G1 in PLAN.md owns the required
expanded frozen-control measurement before slice acceptance or further delivery.

G1 proof now obtained: the same20-pair Python NS protocol analyzes22 additional
fixed controls (all Python-suite source literals plus independent class methods)
alongside the original example. Manifest141files/hash
7b1fdaba93f8a159404627b2b7c64160dce6a0fc8714dd1d3cff0ba3e50f4412.
Same baseline/candidate/interpreter and warmed protocol;578loops perbatch.
Medians1.309062/1.256235s,SD.112729/.091238s,median slowdown-4.035%,paired95%CI
[-6.7677%,-.6803%]. Its upper bound is below3%, despite observed host variation.
Raw timings, controls and expanded manifest are under the durable evidence's
controls/ directory. The full CLI fixed-input tree is unchanged and its prior
complete measurement remains applicable. No production changes or required
behavioral proof were invalidated. This closes the missing performance gate;
it does not erase the incorrect merge-before-full-proof ordering.

Final independent artifact refactor review accepted the expanded corpus/interval,
reused prior behavioral and CLI proof, and returned ## REFACTOR COMPLETE.
Guarded fresh delivery first refused a missing quoted Reported field in G1,
before commit; correcting that record produced the accepted af27c910 increment.
Publication target is the execution branch, observer reused/registered. The PR
thank-you comment now links immutable af27c910 evidence and explicitly records
both the control result and the earlier acceptance-ordering error.
