# Custom CI queue facts

Extend the [custom adapter discovery contract](custom-ci.md#request-and-response-contract)
when this project can authoritatively report queued or coalesced work.

Discovery may also return a separate authoritative `queue` array:

```json
{
  "attempts": [],
  "queue": [
    {
      "sha": "FULL_PUSHED_COMMIT_SHA",
      "branch": "BRANCH",
      "state": "queued",
      "check": "project-checks",
      "context": "push"
    }
  ]
}
```

Each queue entry requires a full SHA, a nonempty branch and `state` of `queued`,
`running` or `retrying`; `check` and `context` are optional nonempty strings.
Omitting `queue` means the adapter does not supply queue facts; `queue: []`
authoritatively reports none. Every successful discovery replaces the previous
queue snapshot, and failed discovery withdraws it. The observer stamps local
observation time; pending explanations expire after two minutes without another
successful observation.

A same-branch exact queue entry can explain pending work for its SHA. With a
declared integrated-state policy, its check and context must also match. A queued
descendant can explain an accepted ancestor only under that policy, with matching
branch, check and context and locally proved Git ancestry. The observer never
fetches a non-successful candidate to establish that ancestry. Queue facts do
not create attempts, run or attempt IDs, `checkedBy`, verdicts or diagnostics.
Actual failures remain failures, and a current real pending retry supersedes an
older successful attempt by its provider timestamp.

When queue facts disappear as an automatic retry becomes a real pending attempt,
that current same-target descendant attempt can still explain pending work under
the declared policy and locally proved ancestry. Each successful poll renews this
progress; failed or empty discovery withdraws it. It retains the original
completion handoff and deadline, and supplies no success verdict. Completion
still requires the current successful attempt and every declared required job.
