# Custom CI proof reads

The Open Dough dashboard's Intent **CI status** tab can show, below an
attempt's verdict, the CI proof summary that attempt's own CI job wrote
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

The attempt is the completed one whose result the tab shows (for a running
result, its completed ancestor), as discover returned it for `check.branch`,
the branch whose discover listed it. Unlike `diagnose`, `runId` and
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
it (the `entries` above are abbreviated). Present parts and their entries may
carry optional `durationMs`: the summed test execution time in milliseconds
for that part, and the sum of present parts that recorded one for the entry.
A part written without a duration carries none; schema versions stay at 1.
A summary collected from fragments written with `--by-file` also carries
each entry's file breakdown, which the tab uses for a selected entry's or
folder's figures; keep it unchanged too, and mind the 64 KiB limit below.
The `unavailable` reason is a nonempty string, shown to the developer as
given and cut at 600 bytes; never answer a verdict there. Any other answer,
both fields, a nonzero exit, invalid JSON, the 20-second timeout or more
than 64 KiB of stdout is shown as the adapter having answered no proof read,
with the adapter's last stderr line for a nonzero exit. An adapter that does
not implement `proof`, such as the
[runnable minimal adapter](custom-ci.md#runnable-minimal-adapter), exits
nonzero for it and shows that line; nothing is retried, and the verdict,
`discover` and `diagnose` are unaffected.

The tab shows the summary's counts only when it is a valid summary whose
`repository` (ignoring case), `attemptId` (as text) and full `revision` equal
the shown attempt's; otherwise it says what is invalid or which differs.
When another attempt the adapter's `discover` answered at that revision
names the same `attemptId` — counting every answered attempt before the
newest per check is kept for the verdict — the tab says the attempt is
ambiguous and asks no adapter, so give each attempt its own `attemptId`.
Each showing of the tab, and each **Check again**, asks again; nothing is
kept.
