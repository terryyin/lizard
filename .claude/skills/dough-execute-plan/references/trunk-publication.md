# Publish execution results

Startup uses this rule to publish a claim. Slice delivery uses it to
publish a validated increment or an owned CI repair. Story wrap-up uses the
same Git steps for each owned closure commit, including before-cleanup and
final-closure commits, under
[wrap-up closure publication](wrap-up-closure-publication.md). Do not invent a
second publication sequence.

This rule does not create execution authority or wait for CI. Observation is
armed from the execution checkout against the authorized target branch.

## Publish a claim

For Story Branch and Trunk Mode, [Take or admit work](../SKILL.md#take-or-admit-work),
including [admission](admit-accepted-work.md) of accepted unlisted work, uses the installed startup operation to publish and confirm the claim on remote
trunk before implementation. Retain the published SHA and recovery
coordinates from its compact result and register that SHA after the observer
is armed. Later environment preparation does
not unpublish that SHA. An unavailable destination or failed publication
preserves the reported state and does not authorize starting unclaimed
work. CI coverage for this claim, including a Story Branch claim's unobserved
trunk target, follows
[Own one observer](ci-monitor.md#own-one-observer).

## Publish an execution increment or repair

After wrap-up proof, refactor, format, and commit succeed, publish the owned
unpublished suffix through managed delivery
([Publish an execution increment or repair](#publish-an-execution-increment-or-repair)).
Planned slices, planless and contextual work, bug repair, and a retrospective
correction all use this delivery. An owned CI repair uses it too. Pause,
stash, and restore stay in
[CI observation](ci-monitor.md#handle-a-notification); do not add a second
repair push.

Managed delivery resolves this checkout's CI runtime, establishes or reuses
this coordinator's own live observer of the authorized target, publishes the
candidate, and attaches the accepted SHA. Do not run a separate probe, start, or `register-push`
for ordinary increments or already-authorized repairs. Retain the delivery
receipt's observation directory when present; do not transcribe mailbox handles
by hand. An unavailable host bridge returns `pendingCi: unobserved` (or an
equivalent coverage-gap receipt) while leaving remote acceptance intact.

Trunk Mode builds the candidate from the local execution branch and pushes
that candidate to remote trunk (`refs/heads/<trunk>`).
It does not push the execution branch. Story Branch Mode pushes that candidate
to the recorded remote execution branch (`refs/heads/<execution branch>`) and
does not push it to remote trunk. Keep the same execution worktree. Caller-selected
current-branch work and host-owned execution enter this
sequence only from the recorded checkout, and only when that caller already
supplied publication authority. Without it, do not push; report the commit
as pending publication. A local commit or local merge does not enter this
sequence. Their previously published base is the recorded checkout's `HEAD`
from before the operation's first commit. An `unpublished-base` stop means
the remote does not hold that base, such as when the developer has their own
unpublished commits in the default checkout: report that work for the
developer to resolve, and never choose a different base to get past it.

A `transport-timeout` stop means a fetch, push, or remote-tip read did not
answer within the transport bound and was ended; its `stage` names which. The
stop rewrites nothing, so the committed candidate it names is preserved. Run
the same `deliver` or `resume` again to retry. When `pushIssued` is true, whether the
remote accepted the candidate stays unknown until the retry's fetch settles it.

## Preconditions

Apply [publish the candidate's preconditions](publish-the-candidate.md#preconditions).
For this caller the owned suffix is the Taken commit for a claim, or
consecutive execution commits not yet on the authorized remote target for an
increment or repair. [Publish a claim](#publish-a-claim) names the
claim target. [Publish an execution increment or repair](#publish-an-execution-increment-or-repair)
names the increment or repair target. The owned workspace is the execution
worktree. For a claim, the supplied validation confirms the selected entry is
**Taken** on the candidate and that no empty commit was invented. For an increment or
repair, it reuses accepted proof whose promise, boundary, implementation,
setup, and observations still match.
[Default-checkout preservation](maintain-default-checkout.md#preserve-pending-local-work)
applies only when this publication mutates that checkout. A maintenance stop
follows
[human judgment](execution-decisions.md#stop-for-human-judgment),
[delivery staging](wrap-up.md#deliver-the-change), and
[resume](../SKILL.md#continue-or-recover-at-an-execution-boundary).
It does not erase a remote acceptance the publisher has already recorded.

## Publish the candidate

Apply [Preconditions](#preconditions), then run managed delivery once from the
owned workspace, where `<installed>` is this project's installed skills
directory that holds `dough-execute-plan` (normally `.agents/skills/` or
`.claude/skills/`):

```text
node <installed>/dough-execute-plan/scripts/execution-increment-delivery.mjs deliver \
  --mode <mode from the established start> \
  --workspace <owned workspace> --branch <execution branch> \
  --previously-published-base <previously published base SHA> \
  --target-ref <authorized target ref> --repo <owner/repo> --host <host> \
  --authority <publish|local-only> [--tracking one-shot] \
  [--session-json <json>] [--coordinator <value> --observer-directory <directory>] \
  [--default-checkout <path>] [--one-shot-identity <identity>]
```

Story Branch Mode passes `--mode story-branch` with
`--target-ref refs/heads/<execution branch>`. Trunk Mode passes `--mode trunk`
with `--target-ref refs/heads/<trunk>`. Caller-selected current-branch work
and host-owned execution have no established start; they pass `--mode trunk`
with their caller's authorized target. A one-shot landing follows
[Land the retained result](one-shot.md#land-the-retained-result). That
operation owns runtime resolution, observer establish/reuse, the
[publish the candidate](publish-the-candidate.md#publish-the-candidate) Git
sequence, and exact-revision registration. A claim uses the execution workspace
selected before its commit; other publications retain theirs. Do not invent a
second publication sequence or a manual `register-push` after managed delivery.

Run `deliver` through the coordinator's own Bash or Shell tool so the observer
belongs to the session that will receive CI events. On Claude Code,
`--host claude` takes that coordinator's identity from its
`CLAUDE_CODE_SESSION_ID`; do not probe, start, or build session JSON for it.
On Cursor, `--host cursor` takes that coordinator's identity from its
`CURSOR_CONVERSATION_ID` in the same way.
An explicit `--session-json` stays authoritative when a caller must name a
different owner, and malformed session JSON stops delivery instead of falling
back to another identity. That identity selects the observer: every increment
and repair reuses the live observer this coordinator claimed, from any
worktree of the repository, and a coordinator without one establishes its
own. Observers other coordinators hold for the same repository and target
stay theirs. If no identity is available, the receipt reports an
unobserved coverage gap naming the missing source while publication acceptance
stands; the next `deliver` from the coordinator's own tool, or with its
`--session-json`, attaches observation without a manual observer start. If
this coordinator holds more than one live observer of the target, the receipt
reports an `ambiguous` gap naming their directories: keep the one this
execution retained, [stop](ci-notify-hosts.md#stop-for-cancellation) the
others, and the next `deliver` reuses it.
On Codex, the yielded stream armed at execution start under
[ci-notify-codex.md](ci-notify-codex.md) is the observer of every increment
and repair: pass `--host codex` with the observer note's coordinator as
`--coordinator` and its exact stream directory as `--observer-directory`.
Only that stream receives the registration, from any worktree of the
repository. Without both inputs, or when the directory is not this
coordinator's live stream of the target, the receipt reports an unobserved gap
naming the input to supply while publication acceptance stands; the next
`deliver` with the retained inputs, after arming when no stream is retained,
registers on it.
A pre-rebase unpublished SHA is not the receipt. After confirmation of a
publication whose target is remote trunk, attempt a refresh under
[Refresh eligibility](maintain-default-checkout.md#refresh-eligibility).
A publication whose target is the remote execution branch does not refresh
the default checkout. Report the publication acceptance, observation result,
and any
[maintenance result](maintain-default-checkout.md#independent-maintenance-outcome)
separately. An
unavailable bridge is a coverage gap on the delivery receipt, not a reason to
undo acceptance.

Retain each pre-push candidate with its actual `suffixBase`, as
[candidate step 5](publish-the-candidate.md#publish-the-candidate) requires.
The managed publisher passes both to `beforePush` and returns `suffixBase`
alongside the accepted receipt; reconcile the pair together after a rewrite.

## Recover a rejected push

Before replaying a claim, recheck that identity's membership on the
fetched remote. Resume when retained execution context and the published
candidate's provenance agree this execution owns it, and do not push again.
A competing claim whose provenance names another execution is a recoverable
conflict: preserve this workspace and do not replay. Identical **Taken** text
is not that provenance. Ambiguous ownership keeps the conflict. When the
identity is still absent, apply
[recover a rejected push](publish-the-candidate.md#recover-a-rejected-push).
A second rejection or other persistent failure stops; preserve remaining
state and report it.

## Resolve a publication rebase conflict

Follow [publication rebase conflict](publication-rebase-conflict.md) for backlog
adapter routing of the owned-suffix rebase, ordinary conflict resolution, and
the required stop when identity or product intent remains unresolved. Fallback
domain knowledge applies only when that adapter is unavailable.

## Resume an interrupted publication

After interruption during a claim's publication or an increment or
repair publication, apply
[publish the candidate's resume](publish-the-candidate.md#resume-an-interrupted-publication)
against that publication's authorized remote target.
The owned suffix is a claim or an increment, using whichever execution
resources actually exist for this publication. Continue only the first
unfinished obligation that resume names. Do not duplicate the commit, push
an already-published candidate, or replace the execution worktree. The claim
uses the workspace selected before its commit, as
[Publish the candidate](#publish-the-candidate) already states for that case.
After that publication obligation is accepted, apply the refresh rule in
[Publish the candidate](#publish-the-candidate). The resume classification
itself still only inspects the checkout.

For managed increment recovery, run the installed resume command from the
owned workspace with the candidate and base retained together before push and
the owner input `deliver` takes:

```text
node <installed>/dough-execute-plan/scripts/execution-increment-resume.mjs resume \
  --workspace <owned workspace> --candidate-sha <retained candidate SHA> \
  --suffix-base <retained suffix base SHA> \
  --target-ref <authorized target ref> --repo <owner/repo> --host <host> \
  [--session-json <json>] [--coordinator <value> --observer-directory <directory>] \
  [--default-checkout <path>] [--superseded-sha <pre-rebase SHA>]... \
  [--one-shot-identity <identity>]
```

Use the returned receipt and `suffixBase` for that delivery comparison. A
legacy recovery without retained base context omits `--suffix-base`; it can
recover publication but cannot establish a historical comparison by guessing
its base. Missing coverage still follows the observer recovery contract.

In that shared table, "Missing registration" is this project's CI
registration: a published SHA absent from the existing observer's coverage or
`register-push` receipt — except a Story Branch claim published to trunk
before any observer is armed there is unobserved coverage, not a missing
registration; see [Own one observer](ci-monitor.md#own-one-observer).

Resume verifies remote acceptance, pushes only a candidate the remote lacks, and
registers the accepted SHA once on this coordinator's live observer. Keep
that owner input for the whole execution. On Claude Code and Cursor, run it
through the coordinator's own Bash or Shell tool, or pass the `--session-json`
that names the session whose observer this execution retained; malformed
session JSON stops resume. On Codex, pass the observer note's coordinator and
exact stream directory. Resume starts no observer and registers on no other
coordinator's observer. When this coordinator's observer is absent, ended,
lost, or one of several, or its owner input is missing, the receipt keeps
`publication: "accepted"` beside an unobserved `observation` whose `ownership`
and `reason` name that state and the input or step that recovers it: report
the coverage gap with the publication, follow that step, and rerun the same
resume to register the SHA.

Workspace or environment-preparation failure after a confirmed claim
publication keeps that published SHA and reuses the claim and any verified
workspace: see [Preserve remaining state](#preserve-remaining-state) and
[execution location](execution-location.md)'s setup-failure rule. It does not
allocate a replacement claim or a nested worktree.

## Preserve remaining state

Apply [publish the candidate's preserved state](publish-the-candidate.md#preserve-remaining-state),
which defers local preservation to
[maintain the default checkout](maintain-default-checkout.md#preserve-pending-local-work).
For this caller, that state is not permission to start unclaimed work,
start implementation from an unpublished claim, or substitute a different
destination than the one recorded for this publication.
