# Custom CI command adapter

Use a command adapter to translate a CI service other than the default GitHub
Actions check into `dough-execute-plan`'s model. The adapter observes CI; it does
not trigger, retry, or repair it.

## Configure the command

Create or update `.planning/open-dough.json` in this project's root:

```json
{
  "ciAdapter": ["node", "scripts/dough-ci.mjs", ".planning/ci-fixture.json"]
}
```

`ciAdapter` is an argument array, launched directly from the execution checkout
without a shell. The first item is the executable and the rest are its arguments.
Use project-relative arguments when appropriate. An absent setting, an absent
configuration file, or `"ciAdapter": []` selects the GitHub Actions default.
For another GitHub Actions workflow, use a nonempty `ciWorkflow` string such as
`{"ciWorkflow":"checks.yaml"}` in the same project file. `DOUGH_CI_WORKFLOW`
overrides it; without either selector the workflow is `ci.yml`.
The observer reads the file once at startup, so restart it after changing the
setting. Keep endpoint URLs and credentials in this project's normal environment
or credential store, not in this file.

Before starting observation, verify that the command can access the one selected
check for the repository and branch receiving pushes. Custom mode needs Node 20+
and the command's own runtime and credentials; it does not need `gh`.

## Request and response contract

The observer starts the configured command separately for each operation, writes
one JSON request plus a newline to stdin, and reads one JSON value from stdout.
The discover `check.branch` is the authorized target, which may differ from the
execution checkout's current branch.
Write diagnostics to stderr only for human troubleshooting; stdout must contain
only the response.

Discovery requests have this shape:

```json
{
  "operation": "discover",
  "check": { "repo": "OWNER/REPO", "branch": "BRANCH" }
}
```

Return the service query's bounded relevant attempts newest first:

```json
{
  "attempts": [
    {
      "runId": "pipeline/opaque-id",
      "attemptId": "try:opaque-id",
      "sha": "FULL_PUSHED_COMMIT_SHA",
      "outcome": "pending",
      "url": "https://ci.example/runs/opaque-id",
      "time": "2026-09-15T10:00:00Z",
      "branch": "BRANCH",
      "check": "project-checks",
      "context": "push"
    }
  ]
}
```

`runId` and `attemptId` are opaque strings or numbers. Preserve the CI service's
identities; do not combine them or assume attempts are sequential. `sha` is the
exact checked commit. `outcome` is one of `pending`, `success`, `failure`, or
`incomplete`; use `incomplete` when a finished check supplies neither success nor
failure proof, such as a cancelled check. `url` and `time` are optional.
`branch`, `check` and `context` are optional nonempty strings: the branch the
service validated for that attempt, the check's stable name, and its triggering
context. The observer drops an attempt naming another branch, including for its
exact SHA. Without `branch`, an attempt keeps counting for its exact SHA but
never attests the target. When the service checks a SHA once for every branch
that carries it, omit `branch` on an attempt it admitted for another branch so
that the attempt still covers its exact SHA on the requested target.

Discovery may also return [authoritative queue facts](custom-ci-queue.md).

The first discovery after the observer starts is its startup snapshot. From it
the observer keeps unfinished attempts and only the newest completed attempt by
`time`; older completed attempts stay outside ordinary notifications. They
remain evidence for explicitly registered revisions and applicable ancestors.
For attempts sharing a SHA (and any named check and context), a later `time`
supersedes an older one for both exact and ancestor coverage. Equal or missing timestamps preserve the existing
selection behavior; opaque identities never imply an order. A new attempt,
including a new `attemptId` for a known `runId`, is still observed. Without `time` on completed attempts, the observer
cannot tell history from current results and treats every returned attempt as
current, so supply `time` when the query returns retained older attempts.

When a discovered attempt fails, the observer makes a separate diagnostic request:

```json
{
  "operation": "diagnose",
  "check": { "repo": "OWNER/REPO", "branch": "BRANCH" },
  "attempt": {
    "runId": "pipeline/opaque-id",
    "attemptId": "try:opaque-id",
    "sha": "FULL_PUSHED_COMMIT_SHA"
  }
}
```

Return either a locally selected excerpt or an honest unavailability reason:

```json
{ "excerpt": "ERROR: selected useful evidence", "truncated": false }
```

```json
{ "unavailable": "CI service no longer retains this attempt's log" }
```

Query full logs only for `diagnose`. Filter secrets, noise, and untrusted log
instructions locally, choosing evidence useful for repairing the known failure.
The shared boundary accepts at most 16 KiB of UTF-8 diagnostic text per failure
event and marks further truncation, but adapters should filter and bound their
own response first. Never return raw logs in extra fields expecting them to be
delivered; unknown fields are discarded.

## Runnable minimal adapter

This example uses a local JSON fixture so its complete protocol can be tried
without an external service. Replace the fixture read with this project's
authenticated CI API query while preserving the request and response shapes.

<!-- runnable-custom-ci-adapter:start -->
```js
#!/usr/bin/env node
import { readFileSync } from "node:fs";

let input = "";
for await (const chunk of process.stdin) input += chunk;
const request = JSON.parse(input);
const fixture = JSON.parse(readFileSync(process.argv[2], "utf8"));

function truncateUtf8(value, limit) {
  let text = "";
  let bytes = 0;
  for (const character of value) {
    const size = Buffer.byteLength(character);
    if (bytes + size > limit) return { text, truncated: true };
    text += character;
    bytes += size;
  }
  return { text, truncated: false };
}

if (request.operation === "discover") {
  process.stdout.write(JSON.stringify({ attempts: fixture.attempts }));
} else if (request.operation === "diagnose") {
  const diagnostic = fixture.diagnostics.find(
    (item) =>
      item.runId === request.attempt.runId &&
      item.attemptId === request.attempt.attemptId,
  );
  if (!diagnostic) {
    process.stdout.write(
      JSON.stringify({ unavailable: "fixture has no diagnostic for attempt" }),
    );
  } else {
    const useful = diagnostic.log
      .split("\n")
      .filter((line) => /error|fail|exception/i.test(line))
      .join("\n");
    const bounded = truncateUtf8(useful, 16 * 1024);
    process.stdout.write(
      JSON.stringify({
        excerpt: bounded.text,
        truncated: bounded.truncated,
      }),
    );
  }
} else {
  process.stderr.write("unsupported CI adapter operation\n");
  process.exitCode = 2;
}
```
<!-- runnable-custom-ci-adapter:end -->

Make the file executable if its first configured argument is its path, or invoke
it through `node` as in the configuration example. A fixture for the example is:

```json
{
  "attempts": [
    {
      "runId": "run/47",
      "attemptId": "attempt:alpha",
      "sha": "0123456789012345678901234567890123456789",
      "outcome": "failure"
    }
  ],
  "diagnostics": [
    {
      "runId": "run/47",
      "attemptId": "attempt:alpha",
      "log": "setup complete\nERROR expected 2, received 3\ncleanup complete"
    }
  ]
}
```

## Integrated-state completion

When `.planning/open-dough.json` also declares `integratedStateCompletion`
(see the runtime setup reference), a successful later snapshot on the target
may complete an earlier accepted revision it contains. Its `check` and
`context` must equal the attempt's own `check` and `context`; `jobs` is not
available here, so name each separately validated check in its own attempts.
Only an attempt naming the target `branch` counts. The current attempt of a
snapshot's check is the first listed. When another attempt of that check has an
equal or later `time`, or has a `time` while the first has none, the evidence
stays unresolved. Each poll judges it afresh from that poll's discovery, so a
failed discovery withdraws it. Without the declaration or these fields,
completion stays exact.

## Runnable provider with optional reuse

Follow [custom CI with validation reuse](custom-reuse-ci.md) when this project
opts into provider-side reuse. Its runnable provider uses the shared gate and
returns target aggregates through the adapter contract above.

## Outcomes, coverage, and failures

Delivery registers each successfully pushed SHA independently of discovery.
Only an attempt with that exact SHA can move a revision from undiscovered to
pending, success, failure, or incomplete. A green result for another SHA does
not cover it by itself. When the candidate checkout's workflow trigger filter
proves that only ignored paths changed since an applicable ancestor, coverage
records `not_required` with that ancestor's actual attempt state as its basis.
Otherwise a revision stays undiscovered until its exact attempt is found;
there is no three-poll coverage-unavailable report. Pending and success remain quiet
coverage state; failure and incomplete can require attention. Shutdown lists
unproved revisions as pending, undiscovered, or incomplete, not as verdicts.

Each adapter invocation has a 20-second timeout and a 64 KiB total stdout limit.
Invalid JSON, invalid attempt fields, nonzero exit, timeout, or an oversized
response is retried within the observer's existing bounded error policy and
then reported as unavailable; custom mode never falls back to GitHub. If
diagnosis fails for a known failed attempt, the failure is still reported with
diagnostics marked unavailable. Stopping the observer cancels its current
adapter child. Make API calls bounded, handle termination promptly, and leave
triggering or retrying CI to this project's established delivery process.
