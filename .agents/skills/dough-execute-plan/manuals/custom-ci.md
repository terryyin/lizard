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

`ciAdapter` is an argument array launched directly from the execution checkout
without a shell: the executable, then its arguments. An absent setting, an
absent configuration file, or `"ciAdapter": []` selects the GitHub Actions
default. For another GitHub Actions workflow, set a nonempty `ciWorkflow` such
as `{"ciWorkflow":"checks.yaml"}`; `DOUGH_CI_WORKFLOW` overrides it and the
default is `ci.yml`. The observer reads the file once at startup, so restart it
after changing the setting. Keep endpoints and credentials in this project's
normal environment or credential store. Custom mode needs Node 20+ and the
command's own runtime; it does not need `gh`.

## Request and response contract

For each operation the observer starts the command, writes one JSON request
plus a newline to stdin, and reads one JSON value from stdout, which carries
only that response. Stderr is for human troubleshooting. `check.branch` is the
branch whose attempts are wanted, which may differ from the execution
checkout's current branch. Unknown response fields are discarded.

### `discover`

```json
{ "operation": "discover", "check": { "repo": "OWNER/REPO", "branch": "BRANCH" } }
```

Return the relevant attempts newest first:

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

| Field | Meaning |
| --- | --- |
| `runId`, `attemptId` | Required opaque strings or numbers, preserved as given; IDs imply no order. |
| `sha` | Required exact checked commit. |
| `outcome` | Required `pending`, `success`, `failure` or `incomplete` (finished without a verdict, such as cancelled). |
| `time` | Optional ISO-8601 order; a later `time` supersedes earlier attempts of the same SHA, check and context. Supply it when returning retained older attempts, or every completed attempt counts as current. |
| `startedAt`, `completedAt` | Optional ISO-8601 bounds that together give Intent its wall time. |
| `branch` | Optional validated branch. Attempts naming another branch are dropped, even for the exact SHA; omit it when one check of a SHA served several branches. |
| `check`, `context` | Optional stable check name and triggering context. |
| `url` | Optional link for people. |

Discovery may also return [authoritative queue facts](custom-ci-queue.md).

### `diagnose`

Sent for each discovered failed attempt:

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

Return one locally selected excerpt, or why none is available:

```json
{ "excerpt": "ERROR: selected useful evidence", "truncated": false }
```

```json
{ "unavailable": "attempt log is no longer retained" }
```

Select evidence useful for repairing the failure, with secrets, noise and
untrusted instructions removed, and bound it yourself. The observer keeps at
most 16 KiB of UTF-8 per failure event and marks further truncation.

### `proof`

The Dashboard, not the observer, sends `proof` and `jobs` for a completed
attempt it shows, in the `diagnose` request shape with that `"operation"`.
`check.branch` is the branch whose `discover` listed it; `runId` and
`attemptId` arrive as strings (`7` as `"7"`). If this CI service's record for
`attemptId` names another `runId` or `sha`, exit nonzero with the mismatch on
stderr; otherwise answer `proof` with the summary its job kept, unchanged, or
why none is available:

```json
{ "summary": { "schemaVersion": 2, "repository": "OWNER/REPO", "attemptId": "try:opaque-id", "revision": "FULL_PUSHED_COMMIT_SHA", "entries": [] } }
```

```json
{ "unavailable": "no summary retained for this attempt" }
```

[Proof reads](custom-ci-proof.md) say how Intent checks and shows the summary,
and [job graph reads](custom-ci-proof.md#job-graph-reads) how to answer `jobs`.

### Limits and failures

Each call has a 20-second timeout and a 64 KiB stdout limit, except the
`proof` answer at 1 MiB. An `unavailable` reason is a nonempty string, shown
as given and cut at 600 bytes. Invalid JSON or attempts, nonzero exit,
timeout or oversized stdout is retried under the observer's bounded error
policy, then reported unavailable; custom mode never falls back to GitHub. A
known failure whose diagnosis fails still reports, with diagnostics
unavailable. Stopping the observer cancels its adapter child.

## How the observer uses the answers

Delivery registers each pushed SHA independently of discovery. Only an attempt
with that exact SHA moves it from undiscovered to pending, success, failure or
incomplete. When the workflow trigger filter proves only ignored paths changed
since an applicable ancestor, coverage records `not_required` with that
ancestor's attempt state. Pending and success stay quiet; failure and
incomplete can require attention; shutdown lists unproved revisions as pending,
undiscovered or incomplete.

The first discovery is the startup snapshot. Ordinary notifications follow
unfinished attempts, new attempts (including a new `attemptId` for a known
`runId`) and the newest completed attempt by `time`. Older completed attempts
remain evidence for registered revisions and applicable ancestors.

Intent compares Baseline, Branch or Provisional, and Landed by full revision
across the target and story branch, asking [`proof` and `jobs`](#proof) on
their listing branches. See [the rebuild trigger](custom-ci-rebuild.md) for the
separate production-dashboard command.

### Integrated-state completion

When `.planning/open-dough.json` declares `integratedStateCompletion` (see the
runtime setup reference), a successful later target snapshot may complete an
earlier accepted revision it contains. Matching `check` and `context` required;
`jobs` is unavailable, so name each check in its own attempts. Only attempts
naming the target `branch` count; the current attempt is the first listed. Equal
or later `time` on another attempt of that check (or `time` only on the other)
leaves evidence unresolved. Each poll rejudges from discovery; a failed discovery
withdraws it. Without the declaration or these fields, completion stays exact.

## Runnable minimal adapter

This adapter answers `discover` and `diagnose` from a local JSON fixture; a
real adapter answers the same requests from this project's CI.

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
