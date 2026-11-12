# Custom CI rebuild trigger

Use this manual when implementing this project's custom CI trigger for the
production dashboard's automatic landed-CI timing follow-up. First provide the
read-only [command adapter](custom-ci.md) and [proof reads](custom-ci-proof.md).
The execution CI observer never runs a rebuild. Triggering and retrying remain
part of this project's delivery process; `ciRebuild` exposes one bounded request
to the production dashboard. Development dashboards send no requests.

## Declare the trigger

Declare an argument array beside `ciAdapter` in this project's
`.planning/open-dough.json`:

```json
{
  "ciAdapter": ["node", "scripts/project-ci.mjs"],
  "ciRebuild": ["node", "scripts/project-ci.mjs"]
}
```

The commands may share an executable or use separate programs. `ciRebuild`
applies only with a configured `ciAdapter`; absent or empty `ciRebuild` keeps CI
readings without a repeat, including retained GitHub Actions receipts. There is
no follow-up enablement setting: the production dashboard on the machine that
records a qualifying new landing receipt follows it up automatically.

## Handle one request

The command launches from the project checkout without a shell. Read one JSON
request from stdin, emit one JSON value on stdout, and send diagnostics only to
stderr. The bound is 20 seconds and 64 KiB of stdout. Bound the CI API call and
handle termination promptly. Keep credentials in this project's normal
environment or credential store.

```json
{
  "operation": "rebuild",
  "check": { "repo": "OWNER/REPO", "branch": "main" },
  "attempt": {
    "runId": "pipeline/original",
    "attemptId": "attempt:original",
    "sha": "0123456789012345678901234567890123456789"
  }
}
```

The request names the original landed attempt exactly as `discover` listed it.
Preserve its opaque string or number identities. Ask for one full check of this
exact revision as a new attempt, even if the branch has since moved. Resolve the
project's CI admission API and permissions before implementing this operation;
if they cannot distinguish new admission from existing work, leave that boundary
unresolved rather than claiming acceptance.

## Return the admission result

When this request admitted a new attempt, answer with its discovery identity:

```json
{
  "result": "accepted",
  "attempt": {
    "runId": "pipeline/repeat",
    "attemptId": "attempt:repeat",
    "sha": "0123456789012345678901234567890123456789"
  }
}
```

When no new attempt was admitted, answer with a nonempty reason:

```json
{ "result": "refused", "reason": "This revision already has queued work" }
```

The provider owns admission and queueing. Refuse when the revision already has
queued or running work. Do not answer `accepted` with an existing reused attempt.
If admission could not be settled, answer:

```json
{ "result": "uncertain", "reason": "The service did not confirm admission" }
```

A nonzero exit, timeout, oversized stdout, invalid JSON or invalid answer shape
also counts as uncertain. The dashboard spends its one-request allowance before
invoking the command. It never resends after refusal, uncertainty or interruption,
including an interruption before the command ran. Another request could start a
duplicate build whose attribution cannot be established. These outcomes remain
unverified; a later discovered attempt does not establish acceptance.

## Expose the comparable repeat

Make `discover` list the exact accepted `runId`/`attemptId` and `sha`, including
while queued (`outcome: "pending"`). The dashboard follows this identity through
the existing `discover` and `proof` reads. Acceptance alone is not a CI outcome.

For a full comparable repeat, that attempt must:

- have a different `attemptId` in text form from the original (numeric `1` and
  string `"1"` name the same attempt); its `runId` may remain the same;
- keep the original `sha`, `check` and `context`;
- start strictly after the original completed, have valid ordered `startedAt`
  and `completedAt` bounds, and finish with `outcome: "success"`;
- meet the existing [proof completeness rule](custom-ci-proof.md), covering all
  proof declarations of the original. Additional complete proof is allowed.

Return the provider's attempt URL when one is available. The review keeps that
link with each attempt's identity, check/context, revision, bounds, wall time and
proof completeness. Both attempts use the same saved prior-build average and
provisional 5% rule. The repeat never becomes a second reference sample.

## Check with a fixture provider

Before connecting a real trigger, exercise the operation against a fixture:
five exact successful, complete 100-second builds, then a qualifying 105-second
Done landing, should send one request naming that original. List the accepted
attempt pending to see **CI rerun in progress**, then complete it with matching
proof at 106 seconds for **Possible CI slowdown**, or 98 seconds for **CI timing
returned to range**. Review both attempts against the saved 100-second average.
These durations are examples, not extra admission rules.

Also exercise refusal, an uncertain result, interruption and an accepted attempt
with incomplete proof or mismatched identity. Observe the unverified explanation
and no second request after further collection or restart. Keep unreleased
dashboard code on fixture CI; demonstrate real admission through the released
production dashboard only.
