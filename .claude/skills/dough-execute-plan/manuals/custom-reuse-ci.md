# Custom CI with validation reuse

[`scripts/ci-reuse-custom-example.mjs`](../scripts/ci-reuse-custom-example.mjs)
is a runnable local provider, separate from the observer adapter. It executes
the covered command, retains direct validation facts, calls the same
[`ci-reuse-gate.mjs`](../scripts/ci-reuse-gate.mjs) used by the GitHub recipe,
and executes target-only work even on a reuse hit. No external service is
needed to try it. The [reuse manual](reuse-ci.md) defines the equivalence,
trust, context and validity requirements; resolve those project decisions
before enabling reuse in CI.

The example supports **serialized scheduling operations** against one journal;
adapter reads may run while a command executes. It atomically replaces journal
and report files. Its journal creation sequence proves ordering independently
of opaque IDs, including reruns; older attempts of the same run become history.
Discovery exposes the provider's latest aggregate target per SHA by that order,
so a newer pending or failed target cannot borrow an earlier green target.
Historical reports remain inspectable through their original URLs.
A source's pending attempt is visible before execution, and direct success is
written only after the actual covered command exits successfully. A newer
unresolved attempt therefore cannot borrow an older success. Reused target
attempts never assert direct validation.

To try it in this project:

1. Give CI a checkout and command that validate exactly the supplied full SHA.
   Define one reusable unit and explicit context/configuration identities.
   Pin dependency/toolchain/external inputs or set a meaningful `maxAgeMs`;
   change their identity or request fresh validation when they change. Keep
   deployment, target secrets and branch-specific work in `targetOnly`.
2. Save the job below as `.ci/source-job.json`, adapting identities, SHA,
   commands and policy. Commands are argument arrays, launched from CI's current
   working directory without a shell. Use a persistent absolute journal path
   accessible to CI and the observer, for example `/tmp/project-ci.json` for
   this local trial. A real provider retains reports at its own inspectable URL.
3. Run source scheduling, then create `.ci/target-job.json` for the accepted
   target SHA with `ref: "main"` and the target's **own** `runId`/`attemptId`.
   Run target scheduling after the remote has accepted that SHA.
4. Set `ciAdapter` in `.planning/open-dough.json` as shown below, using the
   [existing command contract](custom-ci.md#request-and-response-contract).
   Discovery filters the
   requested repository and branch and returns genuine aggregate target attempts,
   newest first, each naming its `branch`, its workflow as `check` and its event
   as `context` (for example `{"check": "project-checks", "context": "push"}`
   under `integratedStateCompletion`).
   Source-only records are excluded even when their ref is the requested target.
   It returns no source facts in extra protocol fields.

```json
{
  "repo": "owner/project", "workflow": "project-checks",
  "ref": "trusted-validation", "event": "push",
  "sha": "0123456789012345678901234567890123456789",
  "runId": "pipeline:opaque/source", "attemptId": "try:alpha",
  "covered": ["npm", "test"],
  "targetOnly": ["node", "ci/target-obligations.mjs"],
  "freshOverride": false,
  "policy": {
    "repo": "owner/project", "workflow": "project-checks", "unit": "covered",
    "configuration": "checks-v1", "context": "pinned-inputs-v1",
    "targetRef": "main", "refs": ["trusted-validation", "main"],
    "events": ["push"], "maxAgeMs": 3600000
  }
}
```

```sh
node .agents/skills/dough-execute-plan/scripts/ci-reuse-custom-example.mjs source /tmp/project-ci.json .ci/source-job.json
node .agents/skills/dough-execute-plan/scripts/ci-reuse-custom-example.mjs target /tmp/project-ci.json .ci/target-job.json
```

```json
{
  "ciAdapter": ["node", ".agents/skills/dough-execute-plan/scripts/ci-reuse-custom-example.mjs", "adapter", "/tmp/project-ci.json"]
}
```

The same command is installed under `.claude/skills`. Each scheduled attempt has
its own opaque identity and a JSON report URL. Inspect that report's `target`,
`decision.source` (original run, attempt, SHA, ref, URL and context),
`decision.reason`,
and covered/target-only outcomes. A hit suppresses covered execution only;
target-only failure fails the aggregate and exits nonzero. Missing, unreadable,
contradictory or expired facts, changed context, new SHA and `freshOverride: true`
run the covered command fresh. Omitting `policy` runs ordinary covered work;
it adds no required project setting. Failed fresh work cannot produce success.

For a real service, replace only journal acquisition and scheduling with the
provider's existing authenticated API and job controls. Prove complete current
attempts (including pending and failures), provider-owned ordering, trusted
refs/events and validity before feeding facts to the shared gate. Preserve
the real target's identity and evidence URL through the unchanged adapter.
Never sort opaque IDs or relabel a source attempt as the target. This example
does not establish support for every external provider or add concurrent
scheduler infrastructure. Both delivery modes still require accepted
publication and successful target completion/shutdown before retirement.
