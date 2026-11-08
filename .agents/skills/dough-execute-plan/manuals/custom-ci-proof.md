# Custom CI proof reads

The Open Dough dashboard's Intent **CI status** and story comparison can show
the CI proof summary that a completed attempt's own CI job wrote
([CI proof summary](../../dough-test-optimization/references/ci-proof-summary.md#publish-through-a-ci-adapter)).
To read it, the dashboard, not the observer, starts the configured
[command adapter](custom-ci.md) from this project's folder with the same stdin
and stdout contract and writes:

```json
{
  "operation": "proof",
  "check": { "repo": "OWNER/REPO", "branch": "BRANCH" },
  "attempt": {
    "runId": "pipeline/opaque-id",
    "attemptId": "try:opaque-id",
    "sha": "FULL_PUSHED_COMMIT_SHA"
  }
}
```

CI status reads the completed attempt it shows (for a running result, its
completed ancestor). The story comparison reads each completed endpoint
attempt it finds by revision among the retained `discover` attempts. In
both views, `check.branch` is the branch whose `discover` listed the
attempt. Unlike `diagnose`, `runId` and
`attemptId` always arrive as strings: a discovered `7` is asked as `"7"`.
`sha` is the full revision.

Before answering, check the attempt against this CI service's own attempt
record, as `diagnose` does: the record for `attemptId` must name this `runId`
and `sha`, or the adapter exits nonzero with the mismatch on stderr. Then
answer with the summary this attempt's job kept, unchanged, or with why there
is none:

```json
{ "summary": { "schemaVersion": 1, "repository": "OWNER/REPO", "attemptId": "try:opaque-id", "revision": "FULL_PUSHED_COMMIT_SHA", "entries": [] } }
```

```json
{ "unavailable": "no summary retained for this attempt" }
```

The `summary` value is the summary file's JSON object as the collector wrote
it (the `entries` above are abbreviated). A successful answer carries that
complete summary; never trim entries or breakdowns for size, and never
substitute `unavailable` because the summary is large. Present parts and
their entries may carry optional `durationMs`: the summed test execution
time in milliseconds for that part, and the sum of present parts that
recorded one for the entry. A part written without a duration carries none;
schema versions stay at 1. A summary collected from fragments written with
`--by-file` also carries each entry's file breakdown, which the tab uses
for a selected entry's or folder's figures; keep it unchanged too. Intent
reads a `proof` answer up to 1 MiB (the collector contract's summary read
limit). When the summary is larger than that limit, still answer it whole so
Intent can report the size failure: under Proof it shows a problem line that
the attempt's CI proof is larger than the 1 MiB proof limit (no counts, and
nothing is remembered). The `unavailable` reason is a nonempty string, shown
to the developer as given and cut at 600 bytes; never answer a verdict
there. Any other answer, both fields, a nonzero exit, invalid JSON or the
20-second timeout is shown as the adapter having answered no proof read,
with the adapter's last stderr line for a nonzero exit. An adapter that does
not implement `proof`, such as the
[runnable minimal adapter](custom-ci.md#runnable-minimal-adapter), exits
nonzero for it and shows that line; nothing is retried, and the verdict,
`discover` and `diagnose` are unaffected.

Intent shows the summary's counts only when it is a valid summary whose
`repository` (ignoring case), `attemptId` (as text) and full `revision` equal
the shown attempt's; otherwise it says what is invalid or which differs.
When another attempt the adapter's `discover` answered at that revision
names the same `attemptId` — counting every answered attempt before the
newest per check is kept for the verdict — Intent says the attempt is
ambiguous and asks no adapter, so give each attempt its own `attemptId`.
The comparison keeps the build visible and shows the reason when its
summary is missing, invalid or ambiguous. Valid summaries are remembered
by attempt for the dashboard process. An attempt with no valid summary can
be read again when the view next asks for its proof.

## Job graph reads

Intent's story comparison can show CI time without runner waits for each
selected completed endpoint. The dashboard asks the adapter for that attempt's
job graph. This optional `jobs` operation uses the same request shape as
`proof`, with the listing branch, string IDs and full revision.

```json
{
  "operation": "jobs",
  "check": { "repo": "OWNER/REPO", "branch": "BRANCH" },
  "attempt": {
    "runId": "pipeline/opaque-id",
    "attemptId": "try:opaque-id",
    "sha": "FULL_PUSHED_COMMIT_SHA"
  }
}
```

Answer with the selected attempt's job graph or the reason it is unavailable.

```json
{
  "jobs": [
    { "id": "build", "durationMs": 30000, "needs": [] },
    { "id": "test-a", "durationMs": 40000, "needs": ["build"] },
    { "id": "test-b", "durationMs": 60000, "needs": ["build"] }
  ]
}
```

```json
{ "unavailable": "no job graph retained for this attempt" }
```

Each job has a unique nonempty `id`, a finite nonnegative `durationMs` of
measured execution excluding runner and queue waits, and `needs` containing
the IDs of jobs it waited for. Use an empty `needs` array for independent jobs.
Report each matrix or shard instance as its own job and use `durationMs` 0
for jobs skipped by a failed attempt. Intent measures the longest dependency
chain, which is 90 seconds in the example. The dashboard makes this read only
for story comparisons. The same 20-second and 64 KiB limits apply as for
`proof`. An ambiguous attempt ID at the revision skips the read.

Intent shows the adapter's `unavailable` reason, bounded to 600 UTF-8 bytes.
For command failures, Intent reports a nonzero exit with its last stderr line,
invalid JSON, timeout, or output over the limit, as it does for proof reads.
An unsupported operation can be reported with a nonzero exit and a reason
on stderr. Intent also explains a graph with no jobs, empty or duplicate IDs,
invalid durations, missing dependencies, or a cycle.
The endpoint's wall time and proof remain available independently of timing.
