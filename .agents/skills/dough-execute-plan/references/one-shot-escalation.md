# Escalate a one-shot attempt

When the attempt proves larger than one-shot allows, through newly discovered
complexity, a separate outcome, or failure to converge, stop substantive work
and escalate it into tracked work before going further, also when replanning
is disabled (`--no-replan`). If the developer stopped the work, do not
escalate: report the attempt and its evidence and leave its workspace to them.

Escalation carries edits only out of an owned workspace. An attempt in the
default checkout stops instead: report the attempt, its evidence, and its
edits left in the default checkout for the developer.

Keep the attempt's edits uncommitted in the workspace: only uncommitted edits
are carried, so undo a result commit you already made while keeping its
changes. For a queued story, first revert any completion or spent-record
removal you composed for it; the story stays in the backlog. An unlisted
request needs its story: draft it in a suitable seed in the originating
checkout as [Prepare the story](admit-accepted-work.md#prepare-the-story)
describes. A queued story keeps its identity, link and title.

Admission publishes a claim, so it needs the
[publication preconditions](trunk-publication.md#preconditions) and authority
to push it. Without that authority, stop there: report the attempt, its
evidence, and the edits kept uncommitted in its workspace. Otherwise run the
start command with the same workspace and branch, `--identity`,
`--publisher-id`, `--push-authorized --workspace-authorized`, and
`--admit --link <link> --title <title> --carry`. The
command parks your edits under `refs/dough/carried/<branch>`, returns the
workspace to clean fetched trunk, and publishes the ordinary
[admission](admit-accepted-work.md#act-on-the-result) there; a queued story's
existing entry moves to Taken without a readiness assessment. It then restores
your edits, uncommitted, over the claim and reports `carried: {restored:
true}`. A queued story that another owner now holds stops with
`source-refused` or `conflict` and leaves your edits untouched.

Any other stop that names `carried.ref` keeps your edits under that ref:

- an interrupted publication resumes as the start command describes, with the
  same flags; if the start ended without a result, run it again unchanged,
  and if that refuses a workspace commit fetched trunk lacks, resume with
  `--starting-revision` set to your one-shot `startingRevision` and
  `--candidate-sha` set to the workspace's `HEAD`;
- `carry-conflict` means the claim was accepted but your edits conflict with
  it in the listed `paths`: report it and leave the merge to a human.

Once restored, the workspace is the claimed story's checkout. Continue through
[Continue into implementation](admit-accepted-work.md#continue-into-implementation)
within the original instruction's authority, reusing your proof while its
boundary is unchanged. Under `--no-replan`, or without authority to plan, stop
there before planning: report the Taken story, the evidence, and the edits
restored uncommitted in its workspace.
