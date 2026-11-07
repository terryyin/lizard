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
configuration file, or `"ciAdapter": []` selects the GitHub Actions default. For
another GitHub Actions workflow, use a nonempty `ciWorkflow` string such as
`{"ciWorkflow":"checks.yaml"}` in the same project file. `DOUGH_CI_WORKFLOW`
overrides it; without either selector the workflow is `ci.yml`. The observer
reads the file once at startup, so restart it after changing the setting. Keep
endpoint URLs and credentials in this project's normal environment or credential
store, not in this file. Before starting observation, verify that the command can
access the one selected check for the repository and branch receiving pushes.
Custom mode needs Node 20+ and the command's own runtime and credentials; it does
not need `gh`.

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
      "startedAt": "2026-09-15T10:00:00Z",
      "completedAt": "2026-09-15T10:03:00Z",
      "branch": "BRANCH",
      "check": "project-checks",
      "context": "push"
    }
  ]
}
```

`runId` and `attemptId` are opaque strings or numbers. Preserve both as given.
`sha` is the exact checked commit. `outcome` is `pending`, `success`, `failure`
or `incomplete` (finished without a verdict, such as a cancelled check).
`url` and `time` are optional. Optional ISO-8601 pipeline bounds `startedAt`
and `completedAt` together give Intent its wall time. `time` orders attempts.
`branch`, `check` and `context` optionally name the validated branch, stable
check and triggering context as nonempty strings. The observer drops attempts
naming another branch, even for the exact SHA. An omitted `branch` lets an
attempt cover its exact SHA without attesting the target. For a SHA the service
checked once across branches, omit `branch` on attempts it admitted elsewhere.

Discovery may also return [authoritative queue facts](custom-ci-queue.md).

Intent's story comparison reads retained `discover` attempts for the
configured target and any known story branch. It matches Baseline, Branch
or Provisional, and Landed builds by full revision across validated branches.
Completed endpoints ask [`proof`](custom-ci-proof.md) on the listing branch.

The observer's first discovery supplies its startup snapshot. It keeps
unfinished attempts and the newest completed attempt by `time` for ordinary
notifications. Older completed attempts remain evidence for registered
revisions and applicable ancestors. Within a SHA, check and context, a
later `time` supersedes earlier ones for exact and ancestor coverage. Equal
or missing timestamps keep existing selection behavior. IDs imply no order.
The observer sees new attempts, including a new `attemptId` for a known
`runId`. Without `time` it treats every completed attempt as current, so
supply `time` when returning retained older attempts.

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

## CI proof reads

See [Custom CI proof reads](custom-ci-proof.md) for the dashboard `proof` operation,
summary validity, and attemptId ambiguity before newest-per-check retention.

## Runnable minimal adapter

Local JSON fixture for the full protocol. Replace the fixture read with this
project's authenticated CI API query; keep the request and response shapes.

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

When `.planning/open-dough.json` declares `integratedStateCompletion` (see the
runtime setup reference), a successful later target snapshot may complete an
earlier accepted revision it contains. Matching `check` and `context` required;
`jobs` is unavailable, so name each check in its own attempts. Only attempts
naming the target `branch` count; the current attempt is the first listed. Equal
or later `time` on another attempt of that check (or `time` only on the other)
leaves evidence unresolved. Each poll rejudges from discovery; a failed discovery
withdraws it. Without the declaration or these fields, completion stays exact.

## Runnable provider with optional reuse

Follow [custom CI with validation reuse](custom-reuse-ci.md) for provider-side reuse:
shared gate, target aggregates through the adapter contract above.

## Outcomes, coverage, and failures

Delivery registers each successfully pushed SHA independently of discovery.
Only an attempt with that exact SHA can move a revision from undiscovered to
pending, success, failure, or incomplete; another SHA's green result does not.
When the candidate's workflow trigger filter proves only ignored paths changed
since an applicable ancestor, coverage records `not_required` with that
ancestor's attempt state. Otherwise a revision stays undiscovered until its
exact attempt is found; there is no three-poll coverage-unavailable report.
Pending and success stay quiet; failure and incomplete can require attention.
Shutdown lists unproved revisions as pending, undiscovered, or incomplete.
Each adapter call has a 20-second timeout and 64 KiB stdout limit. Invalid JSON
or attempts, nonzero exit, timeout, or oversized stdout is retried under the
observer's bounded error policy, then unavailable; custom mode never falls back
to GitHub. Failed diagnosis for a known failure still reports with diagnostics
unavailable. Stopping the observer cancels its adapter child. Bound API calls,
handle termination promptly, and leave triggering or retrying CI to this
project's established delivery process.
