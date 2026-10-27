# Final candidate qualification — slice 5

The qualified correction was merged through PR536, followed by one authorized
acknowledgement. The coordinator owns delivery and plan/obligation status;
this record retains the accepted qualification and actual public receipts.

## Exact revision and preserved history

- Contributor head / expected merge guard:
  `1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94`.
- Qualified pre-merge master and refreshed PR base:
  `9979f106a955aef2460ed6fe7ebb5782b755bcad`.
- Candidate tree: `1317dac0f4b28b401dd4078dc75bd0a3756dd9a0`.
- PR test merge: `20096f8f01497d8e244e086595ab0d8e4aa8bdc6`, parents
  actual master then expected head; its tree equals the candidate tree.

The coordinator integrated raw contributor history `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`
and current master through the installed history-preserving publication and
backlog adapters. `3ba2eb69b228b5103570122c4660b7666d1b797e` first retained
the raw contribution; `1b8b56e` then retained current master. Conflicting raw
reader and duplicate ES6-test bytes were resolved to the accepted `f6d5b09`
versions. Both original raw head and master are ancestors of the final head:

```sh
git merge-base --is-ancestor 92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f HEAD
git merge-base --is-ancestor 9979f106a955aef2460ed6fe7ebb5782b755bcad HEAD
```

Both exit0. `git diff --name-status f6d5b09f68759bf5904d23a4361c0ea0ad02e0d3 HEAD`
contains only the backlog and SEED-017 preparation seed/plan. All product,
tests, dependencies, Nix locks and workflow bytes remain accepted bytes.
The measured product-manifest identity remains
`e90d76c23366e9b30c1fe86f09cb110c3877ba3d0798ab3169c9c2d0309a6ee0`.

Accepted coordinator publication receipts are retained in [local JSON](slice5-local.json):
both origin increments are `published`, with no rejected/forced push. The
actual contributor command was:

```sh
git push --porcelain git@github.com:FabioMalpezzi/lizard.git 1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94:refs/heads/pr/js-arrow-parameters
```

Coordinator observed exit0, `92fff8a..1b8b56e`, `Done`. Qualification freshly
ran `git ls-remote git@github.com:FabioMalpezzi/lizard.git refs/heads/pr/js-arrow-parameters`
and observed the exact expected head. The pre-merge PR refresh found it open
and maintainer-editable, with zero existing acknowledgement comments.

## Local checks and observing assertions

One outer `nix develop -c python -` invocation received the exact
[retained stdin payload](slice5-local-runner.py) using heredoc label `PY`.
Inside that single Nix environment, the literal child commands were
`/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro/.venv/bin/python -m pytest`
then `make`. The complete stdout, argv, timings, host observations and clean
before/after identity are retained in [slice5-local.json](slice5-local.json).
The payload checks expected head before launch and equal head/tree plus clean
working tree after both terminal results. No second Nix entry overlapped it.

| Command | Terminal result | Observed boundary |
| --- | --- | --- |
| Full pytest | exit0; 1944 passed, 7 skipped, 24 warnings in13.37s | All1951 collected tests, including reader/core/CLI/output/extension/website consumers |
| `make` | exit0 in22.45s; its suite1944 passed,7 skipped in14.35s | Coverage run/report94%, pycodestyle, configured `pylint --exit-zero` |

`testJavaScriptArrowParameters.py` observes reported/named/anonymous arrows,
aliases, wrappers, sigils and actual outputs. `testJavaScriptFormalSlots.py`
observes exact count3/names `tail` for empty patterns across seven readers;
receiver/trailing exclusions; no callback annotation phantoms; outer function
return arrows; generics/default CCN; and real CSV/checkstyle/HTML, warning,
duplicate-key, CLI and Flask JSON consumers. Setup supplies source, thresholds
and real temporary files; no parser or analyzer result is mocked. Shared
parameter-name and other-language suites remain included in the full run.

The accepted consumer traces in [slice3 proof](slice3-proof.md) and
[slice4 proof](slice4-proof.md) remain applicable: every reached runtime
consumer is run/reused/excluded there. Full pytest freshly runs those suites;
the unchanged frontend's `fn[field]` rendering reuses inspected evidence.
Frozen65 API/actual CLI observations, all101 tuple parity, broad100 default/ENS
byte parity and all8 strict performance gates reuse slice4's unchanged source,
inputs, harness, configuration and actual target. No timing was repeated.
Member-assignment count0 and abandoned outer CCN2 remain the exact authorized
exclusions; the promised wrapper count2 remains covered. Browser rendering
and deployed-site behavior are not claimed.

Expensive-proof basis: Territory.local,16 logical cores, initial load
3.32/3.50/3.56 and about16.3GB free+inactive pages. Before make, refreshed load
was3.46/3.52/3.57 with about15.9GB free+inactive. Prior suite~12s and expected
make~30s supported one serial fullpytest→make run; no competing owned heavy
work was known. Failure/contention or changed workload would require
reassessment. The pre-existing platformdirs resolver warning did not prevent
terminal success. No test or source assertion was weakened.

## Current hosted qualification

[CI JSON](slice5-ci.json) retains exact run/head/attempt, all jobs and steps,
PR/base/test-merge identity, rules and concrete web-detector log lines.

- [PR run40538066647](https://github.com/terryyin/lizard/actions/runs/40538066647),
  attempt1, `pull_request`, exact expected head: completed/success.
- [Push run40537742798](https://github.com/terryyin/lizard/actions/runs/40537742798),
  attempt1, `push`, exact expected head: completed/success.

Each run contains exactly14 successful builds: Ubuntu24.04 and Windows across
Python3.8–3.14. Every build's actual `Run tests` step succeeds. Each `changes`
job succeeds; each conditional `web` is skipped. The matrix name set and all
16 jobs were checked for each run, rather than accepting an aggregate alone.
All32 exact-head check runs are terminal:30 successes and2 justified skips.
Legacy status contexts are empty and supply no additional success evidence.

`python-app.yml` triggers branch pushes and PRs to master. Its web job requires
`changes.outputs.web == 'true'`. PR detector logs compare actual9979→1b8b56e
and emit `web=false`; push detector logs compare3ba2eb69→1b8b56e and likewise
emit `web=false`. The actual PR diff intersects none of `WEB_PATHS` in
`scripts/web_changes.py`, including website/templates, dependencies/Nix files,
website tests, detector or workflow. Both other workflows require release tags;
neither applies to this branch/PR event. Master is unprotected with required
status enforcement off, no named contexts and an empty ruleset list. The
story's stronger applicable-CI requirement is nevertheless fully observed.

Discovery used `github_fetch_commit_workflow_runs` for the exact head, then
GET `/actions/runs?head_sha=<exact-head>&per_page=100` for all events, exact
run metadata, jobs, check-runs and detector logs through the GitHub connector.
Both runs were terminal before qualification required a wait; no native
`gh run watch` or extra polling/watch was needed. No owned command remains live;
the coordinator's observer was neither changed nor retired.

## Accepted live receipts

Immediately before merging, the coordinator independently refreshed the open
PR at exact head `1b8b56e`, mergeable=true, and all 14 PR builds plus changes
successful/web skipped. The GitHub connector merge used
`expected_head_sha=1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94` and
`merge_method=merge`. GitHub returned `merged: true`,
`sha: 1bc7cbd64bef352300195c66888139bdf8d8d8e0`, and
`Pull Request successfully merged`.

[Actual merge 1bc7cbd](https://github.com/terryyin/lizard/commit/1bc7cbd64bef352300195c66888139bdf8d8d8e0)
was confirmed by `git fetch origin master` (9979→1bc7cbd). Its tree is
`1317dac0f4b28b401dd4078dc75bd0a3756dd9a0`, identical to the qualified
candidate/test-merge tree. Original contributor and current-master histories
are retained. Exact public receipt fields also appear in [local JSON](slice5-local.json).

Only after confirmed merge and a fresh empty comment query, the coordinator
posted [one thank-you](https://github.com/terryyin/lizard/pull/536#issuecomment-6447350182):
comment 6447350182, author `terryyin`, created `2026-10-27T16:21:57Z`.
Its exact body is:

> Thanks for the clear report and regression cases! Merged after unifying arrow parameter capture with the existing signature logic, counting empty binding slots, and handling callback/return annotations. The corrected revision passes the full suite, `make`, hosted CI, and all eight performance gates.

A fresh comments read contained exactly that one comment, confirming the
receipt and preventing a duplicate acknowledgement. No live action was
performed by this implementation agent. Accepted product, semantic, consumer
and performance proof is unchanged and reused; no tests or timings were rerun
after receipt settlement. G7's qualification, history-preserving publication,
exact-head merge and acknowledgement observations have no remaining gap.

Fresh non-inheriting `slice5_source_review` read only the original seed section
and explicit execution amendment. Its launch and individual commitments remain
in session history. Coordinator comparison accepts the source journeys and
qualifications through the actual assertions/unchanged source proof above,
the independently refreshed hosted jobs and the observed merge/comment receipts.
Canonical G7 closure and slice delivery remain coordinator-owned.
Independent `delivery_refactor` completed with one guidance-only correction:
the shared review README no longer repeats a stale queued-correction count;
the story map remains authoritative. Its 99-disposition
[consumer trace](slice5-refactor-consumers.md) and record/link/EOF checks pass.
No runtime boundary changed or required renewed tests or measurements.
