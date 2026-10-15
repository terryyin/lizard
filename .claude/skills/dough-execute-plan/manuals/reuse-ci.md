# Reuse exact-commit CI validation

Adopt this optional gate in the project's CI when a named validation unit is
equivalent on its trusted source refs and target. Installation does not edit CI
or enable reuse. Keep deployments, side effects, target secrets/environments,
and branch-specific checks outside that unit. Follow the project's decisions;
an unresolved equivalence decision requires ordinary validation.

The key is the exact accepted SHA plus validation unit, configuration and
context. Declare all dependency/toolchain, environment and external inputs;
pin them or set a meaningful validity bound. A label or hash alone does not
prove equivalence. Change the context/configuration identity or request fresh
validation when those inputs change. No tree/parent equivalence qualifies.

CI reads the current relevant attempts when its target gate runs. Missing,
pending, failed, unreadable, expired, mismatched, ambiguous or incomplete facts
choose fresh execution. A newer relevant attempt prevents an earlier success
from qualifying; a proved later direct successful attempt can qualify. Source
IDs are opaque. Providers supply ordering independently of those IDs. This is
a decision from readable evidence, without a reservation against later reruns.

`scripts/ci-reuse-gate.mjs` reads JSON from stdin or `--input <file>` and emits
a decision, optionally to `--output <file>`. Its input is
`{sha,targetRef,policy,query,now?}`. Policy declares string `repo`, `workflow`,
`unit`, `configuration`, `context`, `targetRef`, allowed `refs`/`events`, and
positive `maxAgeMs`. Query declares `complete: true`, `freshOverride: false`
and every relevant current attempt, including unsuccessful attempts. Each
fact supplies exact `sha`, trusted `repo`/`workflow`/`ref`/`event`, opaque
`runId`/`attemptId`, inspectable `url`, `current: true`, numeric provider `order`,
`outcome`, `direct`, unit/configuration/context, and millisecond `validatedAt`
and `validUntil`. Unknown or contradictory facts are a miss. The provider owns
proof of completeness, currentness, trust, ordering and direct execution; the
shared rule owns eligibility. Never feed an observer's preferred-success or
startup-history selection into this lookup.

## GitHub Actions adoption

This recipe uses the selected workflow's exact-SHA run listing, then its current
attempt and covered job log. It permits at most three 100-run pages and one
100-job page per attempt; incomplete or capped (1,000-run) searches are misses.
It checks the current attempt again after acquisition. Strict attempt-start
timestamps order attempts, including reruns of older runs; tied times miss.
Only one successful covered job with one emitted `DOUGH_CI_DIRECT` JSON line
qualifies. Echoed command text, skipped jobs and earlier reused decisions do
not establish direct validation. Emit the marker only after the covered command
succeeds. Trust only source refs/events and repository writers allowed to make
that assertion; do not include untrusted pull requests or fork refs.

Put policy beside CI, for example `.ci/reuse-policy.json` (adapt every value):

```json
{
  "repo": "owner/project", "workflow": "ci.yml", "unit": "covered",
  "configuration": "covered-checks-v1", "context": "pinned-inputs-v1",
  "targetRef": "main", "refs": ["trusted-validation", "main"],
  "events": ["push"], "maxAgeMs": 3600000
}
```

The following jobs are an adoption template, not a CI migration. Keep the
project's actual triggers, checkout/authentication, supported Node 20+ setup and required
checks. `GH_TOKEN` needs Actions read access. Open Dough's GitHub reader uses
its installed request owner; prepare that owner's normal GitHub prerequisites
in the CI environment as described by [runtime setup](../references/runtime-setup.md).
The skill path below uses `.agents`; `.claude` provides the same commands.

```yaml
jobs:
  decision:
    runs-on: ubuntu-24.04
    outputs:
      reuse: ${{ steps.gate.outputs.reuse }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '24' # choose the project's pinned supported version
      - id: gate
        env:
          GH_TOKEN: ${{ github.token }}
          # Set true for an explicit fresh request. Missing input also misses.
          DOUGH_CI_FRESH: 'false'
        run: node .agents/skills/dough-execute-plan/scripts/ci-reuse-github-command.mjs .ci/reuse-policy.json decision.json
      # Preserve decision.json as an artifact if the project needs it after
      # step-summary retention; its JSON contains source and target identities.
  covered:
    needs: decision
    if: needs.decision.outputs.reuse != 'true'
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '24' # choose the project's pinned supported version
      - run: npm ci && npm test
      - name: Record direct proof after successful execution
        run: |
          node --input-type=module -e '
            import fs from "node:fs";
            const p = JSON.parse(fs.readFileSync(".ci/reuse-policy.json"));
            console.log("DOUGH_CI_DIRECT " + JSON.stringify({
              sha: process.env.GITHUB_SHA, unit: p.unit,
              configuration: p.configuration, context: p.context
            }));'
  target-only:
    needs: decision
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - run: ./ci/target-obligations.sh
  aggregate:
    needs: [decision, covered, target-only]
    if: always()
    runs-on: ubuntu-24.04
    env:
      DECISION: ${{ needs.decision.result }}
      REUSE: ${{ needs.decision.outputs.reuse }}
      COVERED: ${{ needs.covered.result }}
      TARGET: ${{ needs.target-only.result }}
    steps:
      - run: |
          # reuse-ci-aggregate:start
          test "$DECISION" = success
          test "$TARGET" = success
          if test "$REUSE" = true; then
            test "$COVERED" = skipped
          else
            test "$COVERED" = success
          fi
          # reuse-ci-aggregate:end
```

Retain source run/attempt, ref, URL, context and reason on the target evidence
page. `ci-reuse-github-command.mjs` writes that JSON to `decision.json` and the
decision job summary, alongside the target's own run/attempt/SHA. All other
required jobs still affect the workflow conclusion. Protect the aggregate as
required if appropriate; a skipped covered job alone is never success proof.

## Custom provider adoption

The [custom provider adoption manual](custom-reuse-ci.md)
includes an installed runnable local provider that executes covered commands,
records direct facts, calls this same gate and creates a distinct aggregate
target attempt. Translate its journal read to the project's complete authenticated
source query and its command scheduling to the provider's existing jobs. Keep
the `ciAdapter` request/response contract unchanged; source provenance belongs
on the target attempt's URL/report. A project without reuse policy continues
ordinary CI and needs no additional observer configuration.

## Publication and completion

Story Branch fast-forward to validated A and eligible exact-A Trunk publication
use the same gate. Integration merge M, retry merge N, closure-added SHA and a
rebased Trunk increment B need their own exact-SHA evidence or fresh work.
Ordinary Trunk increments, repairs and closures publish directly to the target;
reuse adds no branch push or branch-validation prerequisite.

Dough still observes the real target workflow/attempt at the accepted SHA.
The completion receipt identifies that target attempt; its URL/report exposes
the distinct source evidence. Never relabel a source attempt as a target run or
treat a source success as confirmation of remote publication. Publication must
be accepted, every covered and target-only obligation must succeed, and required
observer shutdown must be confirmed before retirement. On failed/unconfirmed
publication, incomplete/failed completion or unconfirmed shutdown, retain the
affected workspace and branch, plus any observer still needed by its unresolved
state, under the existing lifecycle guidance.
