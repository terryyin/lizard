# Explicit CI test case counts

**Identity:** SEED-052#publish-explicit-ci-test-counts
**Source:** [See Lizard test case counts in Intent](../../seeds/SEED-052-publish-explicit-ci-test-counts.md#publish-explicit-ci-test-counts)
**Source review:** [source-review.md](./source-review.md)
**Status:** In progress; Slice 1 proof retained, Slice 2 next.

## Goal and scope

Publish explicit case evidence for `test` and `website/frontend/features` so
Intent shows runner case totals and compares builds using the same unit.
Preserve outcomes, file breakdowns, execution times, missing parts, and the
selected run attempt's identity. Publish only the existing representative
ubuntu-24.04 / Python 3.13 analyzer leg and existing Cucumber job.

No analyzer changes, additional matrix proof producers, file-unit publication,
website-selection policy change, or new upstream Intent behavior is included.
The current Open Dough installation is 0.3.94, already above the source's
0.3.93 minimum; no guidance update is needed on this observed baseline. An
older execution checkout uses `dough-update` before copying the collector.

## Existing solution and decisions

PFE: the existing runner converters already emit one outcome per pytest
`testcase` or Cucumber scenario and optional times per repository path.
`scripts/ci-proof/` owns publication; `.github/workflows/python-app.yml` owns
producer selection and collection. Reuse these, replacing the old collector
with the seven unchanged released modules from the installed
`dough-test-optimization` [copy recipe](../../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md#1-copy-the-collector-into-this-project).
Do not add a parallel counter, schema, or unit conversion in either converter.

The common rule is one runner result per declared case; the two converters
only interpret their respective report formats. Missing reports do not supply
zero-success fragments. Unit-less history stays legacy evidence because its
counting unit cannot safely be inferred.

The ADR index and both Accepted records were checked. This preserves the
existing CI boundary, requires no new architectural decision or North Star
topic, and uses capability names under [ADR 0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

The largest replacement collector is 249 lines; all seven are below the
250-line refactor check. Existing workflow: 176 lines; converter test file:
102 lines. Keep added proof cohesive and under that limit; if it grows, split
converter proof from collector-pipeline proof by responsibility.

## Outside-in proof ownership

| Source promise | Owner | Observable proof |
| --- | --- | --- |
| Pytest cases, including failures/skips, retain totals and file/time detail | Slice 2 | Runner XML → existing converter CLI → copied fragment CLI with `--unit case --by-file --times` → summary CLI; four results in two files yield 2 passed / 1 failed / 1 other, 375 ms, one untimed case |
| Cucumber scenarios retain outcomes and execution times | Slice 2 | Cucumber JSON → existing converter CLI → same collector route; background excluded, three scenarios yield 1 / 1 / 1, hook/step durations retained, untimed scenario retained |
| Both configured locations and attempt/revision are preserved | Slice 2 | Combined summary has exactly those entries, each with `case` evidence and matching repository/run/attempt/revision; hosted artifact agrees with the representative runner logs |
| Missing report remains missing, including skipped website job | Slice 2 | No website fragment plus explicit case declaration yields missing case part; converter failure creates no valid fragment; existing job conditions remain in place |
| Intent shows case counts and same-unit differences | Slices 1 and 2 | Slice 1 establishes deployed reader/proof route; Slice 2 observes migrated artifact in Intent and compares case evidence (4→5 and 3→4 fixture totals give +1 each) |
| Legacy history remains readable with no incompatible count/time delta | Slices 1 and 2 | Retain a pre-migration artifact; observe legacy labeling and comparison suppression against migrated case evidence through the established Intent route |

New local tests drive the project CLIs through subprocesses in temporary
directories and inspect their emitted JSON. Reuse the converter tests' report
shapes, without mocking converter/collector internals. A focused proof must
fail against the old collector (which rejects `--unit`) and distinguish an
omitted producer unit or untyped expected declaration. Do not copy upstream's
whole collector test suite into Lizard.

## Decisive premises and observations

All observations below were made on 2026-11-10 in the preparation workspace at
`6bdfb38691b413aeb268652694ed198d40ad6a0f`, with Nix Node v24.21.0.
Temporary files were under `.planning/` and removed before recording this plan.

| Premise / consuming operation | Observed form and result | Remainder / owner |
| --- | --- | --- |
| Released collector consumes the converter results with the proposed flags | Seven installed 0.3.94 modules copied together into a temporary collector; existing converters consumed actual XML/JSON fixtures; `node <copy>/ci-proof-summary.mjs fragment --outcomes <tsv> --times <times> --by-file --unit case --entry <location> --part 1 --of 1 --attempt 2 --revision <HEAD> --out <fragments>`, then `summarize --fragments <fragments> --expect test=1:case --expect website/frontend/features=1:case --repository terryyin/lizard --run-id 42 --attempt 2 --revision <HEAD> --out <summary>` emitted schema 2; analyzer 2/1/1, 375 ms, one untimed, two files; website 1/1/1, 375 ms, one untimed, one file | Settled for local publisher approach; hosted runner execution is Slice 2 acceptance |
| The old publisher cannot provide the proposed explicit declaration | Same fragment command against `scripts/ci-proof/ci-proof-summary.mjs` exited nonzero for unknown `--unit` | Settled; the future permanent test still needs its own discriminating red/green observation |
| An absent report remains a missing case part | Removed only the website fragment from the scratch directory and reran the exact summary command: website evidence retained `unit: case`, counts 0/0/0, part 1 `missing` | Settled |
| Current artifact retrieval works for Lizard's judged workflow | `gh run list --repo terryyin/lizard --workflow python-app.yml --limit 3 --json databaseId,headSha,status,conclusion` succeeded; `gh run download 42729391425 --repo terryyin/lizard --name ci-proof-summary-attempt-1 --dir <scratch>` succeeded. Schema 1 artifact at `4853c9caeccd8c0ed6f529502127b17818950089`: analyzer 3095 passed / 0 failed / 7 other, 16833 ms, website missing | Settled for GitHub read access and legacy baseline, not for a new attempt |
| Deployed Intent consumes case evidence and suppresses incompatible comparisons | Supplied dashboard origin `http://127.0.0.1:4173` returned its page with HTTP 200. Installed recipe describes schema 2 consumption; upstream source has unit/legacy reader and Intent regressions, but these were not executed in the deployed profile | Open; Slice 1 is the bounded early probe. Page reachability and source inspection alone do not establish rendering/comparison behavior |

## Ordered slices

### 1. Establish the deployed Intent proof route

**Type:** Behavior — bounded infrastructure probe.
**Status:** done.

**Question:** Does the actual Intent instance used for Lizard support declared
case evidence and the legacy comparison boundary, and what repeatable route
will observe the migrated artifact?

Before changing the publisher, use the authenticated dashboard's Lizard Intent
CI status and an existing artifact to record the shown run/revision, proof
locations and legacy labels. Identify the deployed release and its supported
case/legacy comparison behavior. Reuse a matching retained deployed observation
if it already covers the same reader and settings; otherwise use that release's
isolated reader/Intent fixture route with the locally generated summaries.
Do not inject synthetic proof into shared project history or alter the live
dashboard's source/settings. Use `dough-manual-testing` for the active slice's
dashboard observation.

Record the exact route/command, deployed revision, settings, fixture input,
and observed counts and comparison result as retained proof here. The fixture
must reach the reader and comparison surface used by the deployed profile;
a development-only replay does not establish that correspondence by itself.
Future real Lizard publication remains Slice 2's acceptance.

**Stop:** If the reader predates unit support, required authentication is
unavailable, or no isolated route can reach the comparison, stop dependent
Slice 2 and record the missing operation and owner. Do not repair upstream
Intent under this story. Reassess readiness on that evidence before continuing.

**Sizing:** One bounded read/replay and one proof route, no product edits.
The unpaid prefix (page and GitHub artifact reads) is already observed; the
authenticated deployed integration is deliberately the early probe.

**Accepted proof:** Deployed Intent **0.3.94** at
`/Users/terryyin/.open-dough/dashboard/deployments/a36e5bcf5395-qhVyNw`
(`vite preview` on `http://127.0.0.1:4173`) supports case evidence and the
legacy comparison boundary for Lizard.

Live Lizard Intent CI status (authenticated, no settings change):
`?project=lizard&perspective=intent`, CI status tab. Shown run
`42739727768` attempt 1, revision `1140217` (Take queued work:
SEED-052#publish-explicit-ci-test-counts). Settings from
`.planning/open-dough.json`: locations `test` and
`website/frontend/features`. Proof table:
`test` → `3095 passed and 7 other legacy test results` / `16.6 s`;
`website/frontend/features` → `nothing counted` / `Part 1 missing`.

Isolated route (same deployment's reader + comparison surface, lizard-shaped
summaries generated with installed 0.3.94 collector
`.claude/skills/dough-test-optimization/scripts/ci-proof-summary.mjs`, then
`checkedRetainedSummary` → `ciProofSchema` →
`overallCountsWords` / `comparisonEntriesOf` /
`ciProofCellsOf` from that deployment):

- Case baseline (4 at `test`, 3 at website) vs case branch (5 and 4): overall
  `7 → 9 test cases (+2)`; entry rows `test` `4 → 5 test cases (+1)`,
  `website/frontend/features` `3 → 4 test cases (+1)`.
- Schema-1 legacy (3102 labelled `tests`, website part missing) vs case
  branch: overall
  `3,102 legacy test results → 9 test cases (legacy unit unknown)` with cue
  `no overall count`; entry `test`
  `3,102 legacy test results → 5 test cases (legacy unit unknown)` with cue
  `units not comparable` (no invented count or comparable time delta).
- Reader unit fixtures also pass:
  `node --test tests/support/dashboard-ci-proof-units-read.test.mjs
  tests/support/intent-ci-comparison-overall-counts.test.mjs` (7/7) from the
  deployment root.

Repeatable Slice 2 Intent route: live CI status URL above for the published
attempt's artifact, plus the same collector → deployed reader/comparison
commands with Lizard CLI-produced summaries for same-unit +1 and
legacy-vs-case no-delta. Stop conditions did not trigger.

### 2. Publish case evidence for both existing runner reports

**Type:** Behavior.
**Status:** planned.

**Precondition:** Slice 1 has established the consuming Intent profile/route.
**Trigger:** The judged CI workflow publishes proof for a selected attempt.
**Result:** Both existing producers emit explicit cases, with preserved
outcomes, breakdowns, times and identity; declared but unreported parts stay
missing; compatible Intent comparisons show case deltas.

Copy `ci-proof-summary.mjs`, `ci-proof-options.mjs`,
`ci-proof-contract.mjs`, `ci-proof-evidence.mjs`, `ci-proof-figures.mjs`,
`ci-proof-fragments.mjs`, and `ci-proof-collect.mjs` unchanged from the
installed released recipe into `scripts/ci-proof/`. Keep the copies
self-contained; CI must not import installed skills.

In `.github/workflows/python-app.yml`, add `--unit case` to both fragment
commands and declare `--expect test=1:case --expect
website/frontend/features=1:case`. Preserve representative-leg selection,
failure publication conditions, artifact names/retention, and website skipping.
The workflow path itself is in `scripts/web_changes.py`'s web boundary, so
this migration's workflow change selects the website job without a policy edit.

Add the pipeline proof above through the copied CLI and verify the workflow's
producer/declaration wiring. Trace affected consumers with the installed
consumer-trace procedure, including existing converters and `test_ci_proof.py`.
Run post-change refactoring before verification/commit under execution's gates.

**Local checks:** `nix develop -c python -m pytest test/test_ci_proof.py`
(plus any cohesive new CI pipeline test file), then
`nix develop -c python -m pytest` before completion, required by `AGENTS.md`.
Use the execution host's current load/memory evidence to sequence the full
suite; no new timing limit is imposed.

**Hosted and Intent proof:** After authorized execution publication, obtain
the migrated `Python application` run's artifact and representative pytest and
Cucumber report/log totals. Verify the summary's units, counts, files, times,
identity and missing parts. Observe those values through Slice 1's Intent
route; replay its same-unit +1 comparison and legacy-vs-case no-delta example
using summaries generated by Lizard's actual copied CLI. Retain the run IDs,
revisions, inputs, exact commands/steps and results. A green workflow or local
JSON alone does not discharge the Intent observation. Own hosted failures
through execution's CI repair procedure; stop for a human only if repair
requires an upstream scope or credential decision.

**Sizing and stopping point:** One format migration with one end-to-end proof
loop. Collector replacement, two producer flags, declarations and regression
proof ship together; no intermediate incompatible publisher is delivered.

## Plan review

Slice-plan refinement was not needed: the initial deployed-reader probe bounds
the one unresolved integration premise; the migration, its two report formats,
and their common proof contract remain one cohesive Behavior slice.
No additional boundary, cumulative-design, or sizing concerns were identified.
The deployed consumer premise remains explicitly unobserved until Slice 1;
the early probe may stop execution, and grants no upstream change authority.

Preparation only: this plan grants neither Take nor execution. No source review,
execution-complete record, or completed slices are claimed by this preparation.
