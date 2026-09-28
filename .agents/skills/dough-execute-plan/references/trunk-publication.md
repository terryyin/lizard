# Publish execution results

Startup uses this rule to publish a claim. Slice delivery uses it to
publish a validated increment or an owned CI repair. Story wrap-up uses the
same Git steps for each owned closure commit, including before-cleanup and
final-closure commits. Do not invent a second publication sequence.

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

Managed delivery resolves this checkout's CI runtime, establishes or reuses the
matching live observer for the authorized target, publishes the candidate, and
attaches the accepted SHA. Do not run a separate probe, start, or `register-push`
for ordinary increments or already-authorized repairs. Retain the delivery
receipt's observation directory when present; do not transcribe mailbox handles
by hand. An unavailable host bridge returns `pendingCi: unobserved` (or an
equivalent coverage-gap receipt) while leaving remote acceptance intact.

Trunk Mode builds the candidate from the local execution branch and pushes
that candidate to remote trunk. It does not push the execution branch. Story
Branch Mode pushes that candidate to the recorded remote execution branch and
does not push it to remote trunk. Keep the same execution worktree. Caller-selected
current-branch work and host-owned execution enter this
sequence only from the recorded checkout, and only when that caller already
supplied publication authority. Without it, do not push; report the commit
as pending publication. A local commit or local merge does not enter this
sequence.

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
[Default-checkout access and preservation](maintain-default-checkout.md)
apply only when this publication mutates that checkout. A maintenance stop
follows
[human judgment](execution-decisions.md#stop-for-human-judgment),
[delivery staging](wrap-up.md#deliver-the-change), and
[resume](../SKILL.md#continue-or-recover-at-an-execution-boundary).
It does not erase a remote acceptance the publisher has already recorded.

## Publish the candidate

Apply [Preconditions](#preconditions), then run managed delivery from the owned
workspace through the installed
`dough-execute-plan/scripts/execution-increment-delivery.mjs` entry point
(`deliver` with the owned workspace, branch, previously published base,
authorized target ref, repository, host, and publication authority). That
operation owns runtime resolution, observer establish/reuse, the
[publish the candidate](publish-the-candidate.md#publish-the-candidate) Git
sequence, and exact-revision registration. A claim uses the execution workspace
selected before its commit; other publications retain theirs. Do not invent a
second publication sequence or a manual `register-push` after managed delivery.

Run `deliver` through the coordinator's own Bash or Shell tool so the observer
belongs to the session that will receive CI events. On Claude Code,
`--host claude` takes that coordinator's identity from its
`CLAUDE_CODE_SESSION_ID`; do not probe, start, or build session JSON for it.
An explicit `--session-json` stays authoritative when a caller must name a
different owner, and malformed session JSON stops delivery instead of falling
back to another identity. If no identity is available, the receipt reports an
unobserved coverage gap naming the missing source while publication acceptance
stands; the next `deliver` from the coordinator's own tool, or with its
`--session-json`, attaches observation without a manual observer start.
A pre-rebase unpublished SHA is not the receipt. After confirmation of a
publication whose target is remote trunk, attempt a refresh under
[Refresh eligibility](maintain-default-checkout.md#refresh-eligibility).
A publication whose target is the remote execution branch does not refresh
the default checkout. Report the publication acceptance, observation result,
and any maintenance result separately. A deferred or stopped refresh does not
erase the accepted publication and does not authorize another push. An
unavailable bridge is a coverage gap on the delivery receipt, not a reason to
undo acceptance.

## Publish wrap-up closure

Story wrap-up treats each owned wrap-up commit on the execution checkout as a
verified increment whose target remains remote trunk. Publish it immediately
through [the common sequence](#publish-the-candidate) before the next wrap-up
mutation that depends on its recovery from shared trunk. That closure target
is not the Story Branch increment destination above. Do not merge the
execution branch.

Resolve observation ownership before the first wrap-up publication: recover
the matching execution observer when it still exists; if observation already
ended, use the existing setup to arm one observer from the same execution
checkout against the authorized target using [CI observation](ci-monitor.md).
Do not start an observer automatically or retarget another mailbox. Register
each confirmed published SHA with that observer. After the last wrap-up
publication this invocation will perform, invoke
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
once for that final accepted SHA on the matching observer. Handle its combined
CI and shutdown receipt, then report the exact published closure SHAs, that
receipt, and remaining coverage. An unavailable bridge or registration failure
is lost coverage: report it truthfully and continue without inventing
successful observation.

A publication stop leaves the commit recoverable on the execution branch.
Do not delete spent history, remove resources, or claim closure. Invoke
completion only after the final applicable wrap-up publication, never between
intermediate recovery-record publications. After a success or bounded
unresolved receipt with confirmed shutdown, wrap-up retires only this
execution's worktree under Dough Land's
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree), with that
receipt as its gate. Unconfirmed shutdown or retained observation preserves the
worktree and branch.

## Observe Story Branch integration

Story Branch wrap-up changes publication targets. Before integrating its saved,
published final-closure tip, close the observer bound to the remote execution
branch with
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
for that tip when it is the last accepted registered revision. A green
execution-branch receipt covers only that branch and never releases later trunk
observation.

From the retained execution workspace, recover one matching observer already
bound to the authorized trunk target or use the existing setup to start one
there. Establish it before integration publication. Do not retarget the old
mailbox, register a revision against a differently targeted observer, or create
a duplicate observer for the same repository, target, and coordinator. When the
branch observer cannot complete, or the trunk bridge cannot be established,
retain explicit unavailable coverage; do not invent success or silently discard
an observer that may still own the checkout.

Publish the history-preserving integration through the common candidate
sequence. After remote confirmation, register only the accepted integrated SHA
with the trunk observer. A saved branch tip or superseded merge candidate is
not that receipt. Invoke
[the shared completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
once for the accepted integrated SHA on that trunk observer and handle its
combined receipt. With confirmed shutdown as its gate, wrap-up then retires
the execution resources under the same
[Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree);
publication recovery retains the same observer and repeats neither target
setup nor an already accepted push.

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

In that shared table, "Missing registration" is this project's CI
registration: a published SHA absent from the existing observer's coverage or
`register-push` receipt — except a Story Branch claim published to trunk
before any observer is armed there is unobserved coverage, not a missing
registration; see [Own one observer](ci-monitor.md#own-one-observer).

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
