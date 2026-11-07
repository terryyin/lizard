# Release a lost workspace's assignment

This continues [Publish the preparation assignment](preparation-assignment.md),
whose `<installed>` path, receipts, and `developer-identity-refused` handling
apply here unchanged.

When the workspace that announced a preparation assignment no longer exists,
nothing can abandon it by workspace. Releasing it is then the developer's
decision about one exact assignment: its profile path plus its allocation,
the commit that added that profile. Apply this only when a developer asks to
release such an assignment and publishing to the recorded target is
authorized, as for
[Abandon the preparation](preparation-assignment.md#abandon-the-preparation).
Only that developer's explicit confirmation that this exact assignment is
abandoned releases it. Its age, its silence, no process running for it, a lost
workspace, a full rotation, or your own inference is never that confirmation.
Without it, report the assignment and leave it published.

Read the assignment first. The allocation appears in an `agent-unavailable`
receipt's `occupied` entry, in the `announced` receipt, or in this command's
own refusal. Name the project's integration checkout (its established
checkout for ordinary work, never another preparation workspace) and the
authorized remote target; when either is unknown, ask rather than guess. Run,
without confirmation:

```text
node <installed>/scripts/preparation-assignment.mjs abandon \
  --integration <integration checkout> --profile <profile path> \
  --remote <remote> --target <trunk branch> --push-authorized
```

`<profile path>` is the repository path a receipt reports as `profile`, such
as `.planning/agents/yui-chan.json`. This publishes nothing and stops with
`confirmation-required`, reporting the assignment trunk holds: `agent`,
`identity` (the story), `activity`, and `allocation`. Show those to the
developer and ask whether that exact assignment is abandoned. Only when they
confirm it, rerun with `--allocation <allocation> --confirmed-abandoned`
added, using the allocation from that receipt, never one you assume. Act on
`status`:

- `abandoned`: remote trunk accepted a commit at `publishedSha` that only
  removes that profile. The story stays queued, and `refresh` reports the
  integration checkout as for the announcement; that checkout's files,
  index, and branch are otherwise untouched.
- `already-released`: trunk had already ended that allocation (`endedBy`);
  nothing was published. A `successor` is a later allocation of the same
  name, possibly for the same story, and stays.
- `allocation-mismatch`: trunk holds a different allocation at that profile
  than the one supplied; nothing was published. The receipt reports the
  current one. It is a different assignment, so it needs its own
  confirmation.
- `not-preparation`: the profile records execution, or is not a readable
  preparation assignment; nothing was published. Only completing its Taken
  story releases an execution assignment.
- `unpublished` or `unconfirmed`: as for that abandonment; rerun the same
  confirmed command once the remote is reachable.
- `no-assignment`: trunk holds no assignment at that profile that the
  supplied allocation could have added; nothing was published.

Any draft the lost workspace held is gone with it; this release decides
nothing about content. Preparing the story again announces anew.
