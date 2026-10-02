# One-shot work

One-shot work completes an explicitly requested, genuinely trivial outcome
with no Taken entry or agent profile, and no story or plan left behind. It runs
in an owned isolated workspace, or in the default checkout when that is
selected, and stops with its verified result retained there for review; only
an explicit request to land it, or automatic landing selected with it,
publishes that result to remote trunk. It tracks work without a separate
execution path: planless execution,
verification, refactoring, delivery, and closure stay as they are. It grants
no permission beyond the current instruction: implementing findings,
publishing drafts, and widening scope still need their own authority.

## Decide whether one-shot applies

Use it only when the developer or parent instruction explicitly selects one-shot
(`--one-shot` or a clear equivalent) for an independently invoked outcome, such
as direct contextual work, a bug investigation or repair, test profiling or
optimization, exploratory testing, or a standalone review, or for a story
queued in the **Backlog list**. Never infer it from apparent smallness. Without
that selection, accepted work is [admitted](admit-accepted-work.md) and queued
work Taken as usual.

The outcome must be eligible: one understood, coherent outcome with no known
need for multiple slices, no unresolved domain or architecture decision, and a
credible focused verification path. An ordinary test-and-fix loop or a short
diagnosis may fit. A queued story whose plan has more than one slice is not
eligible. Known larger work is admitted or Taken instead.

These are not one-shot, even when selected:

- preparation-only requests, which keep their existing keep and disposition
  rules; refinement selects one-shot through
  [one-shot refinement](../../dough-story-refinement/references/one-shot-refinement.md);
- work already Taken, which keeps its claim's lifecycle; and
- a supporting step of an active story, which continues under that story.

## Start in an owned workspace

A carried [established one-shot start](established-start.md#continue-an-established-one-shot-start)
continues there instead of running the start command below.

Resolve the same execution context as any Story Branch or Trunk Mode start:
mode, the owned workspace, and the actual remote and trunk branch it starts
from. Starting needs authority to create or use that workspace, not to publish.
Then invoke the start command from [Take or admit work](../SKILL.md#take-or-admit-work)
with `--one-shot` instead of `--admit`: the originating integration checkout
when one exists (otherwise the repository context that section describes),
owned workspace path and branch, mode, actual remote and trunk branch, and
`--workspace-authorized`. An unlisted request needs no `--identity` or
`--publisher-id`; supply `--identity` for a queued story, or when the request
names existing work, so the command can check how fetched trunk holds it.

The command fetches trunk, selects or creates the owned workspace at fetched
trunk, and publishes nothing: no commit, push, profile, or backlog change. Its
one-line result `ok: true, status: "prepared"` carries `startingRevision`,
`created`, and the default checkout's `maintenance`; retain
`startingRevision`. A refusal (`ok: false`) starts no work: `invalid-request`
also names `--one-shot` combined with `--admit`, `authority-required` names
missing workspace authority, and `source-refused` names work fetched trunk
shows as Taken, held by an agent profile (execution or preparation), or queued
with a recorded `not-ready` reason. Then run this project's checkout-bound
setup under [execution location](execution-location.md) before implementation.

## Work in the default checkout

When the developer or parent instruction also selects the default checkout
(`--default-main` or a clear equivalent), work directly in this project's
established default checkout on its trunk branch instead of an owned
workspace. Resolve that checkout's actual path and current branch; a
configured project folder alone does not establish them. Run the start command
with `--one-shot --default-main`, `--workspace` set to the default checkout,
mode, actual remote and trunk branch, and `--workspace-authorized`, plus
`--identity` as above. Omit `--branch` and `--integration`, or name the trunk
branch and that same checkout.

The command fetches trunk and checks a supplied identity there as above, then
takes the checkout exactly as it is: uncommitted changes and local commits
stay, and nothing is reset, refreshed, created, or published. Its result
`ok: true, status: "prepared"` carries `role: "default-checkout"`, the
`workspace` path, its `branch`, its actual HEAD as `startingRevision`, the
`fetched` trunk, and `created: false`. `setup-failed` refuses a checkout on a
branch other than trunk or with an ongoing Git operation; report its `error`
and leave switching branches or finishing the operation to the developer.
`invalid-request` names a `--branch` or checkout path that does not match, and
`--default-main` without `--one-shot`.

Existing changes in the checkout need no clean checkout or confirmation: they
become part of this session's result. Verify and commit there as below, staging
everything (`git add -A`) so all checkout content is committed together; never
discard, stash, or leave out existing content. Report that the result commit
sits on the checkout's local trunk branch, after any earlier local commits.

## Verify and retain the result

Work only in that workspace. Run the focused verification the outcome needs
and the [post-change refactor](../../dough-post-change-refactor/SKILL.md) pass,
then commit the result with plain `git commit`: the start named no agent.

Then stop for review, unless automatic landing was selected: then continue
under [Land automatically when selected](#land-automatically-when-selected).
Report workspace path and branch, retained `startingRevision`, result commit, verification
evidence, and pending issues. Tell the developer to ask this workflow to
[land the retained result](#land-the-retained-result), naming the kept workspace.
For a queued story, also report that remote trunk still lists
it in the **Backlog list** while its closure waits in the result commit.
Leave the workspace and its branch in place: nothing is pushed or retired
until the developer asks to land the result. The default checkout always stays in place.

## Land the retained result

When the developer explicitly asks to land the retained result, in this session
or a later one that names its workspace, deliver it from that workspace through
[increment publication](trunk-publication.md#publish-an-execution-increment-or-repair)
with `previouslyPublishedBase` set to the retained `startingRevision` (otherwise
the merge base of the workspace branch and fetched trunk) and the target set to
remote trunk, even in Story Branch Mode: one-shot work has no execution branch
or claim to deliver to. That request is the authority to publish it. After
acceptance, refresh the default checkout and complete CI observation as for any
trunk publication. A default-checkout result is delivered from that checkout,
with `previouslyPublishedBase` set to the merge base of its HEAD and fetched
trunk, because its earlier local commits are part of the result; it is the
default checkout itself, so supply no separate one to refresh. If the delivery
result is lost or interrupted,
[resume the interrupted publication](trunk-publication.md#resume-an-interrupted-publication)
with the candidate you retained, never by committing or pushing again.

## Land automatically when selected

When the developer or parent instruction also selects automatic landing
(`--auto-land` or a clear equivalent), that selection is the developer's
advance authority to publish this verified result, so it does not wait for
review. It is independent of the workspace choice. A confirmation that
existing default-checkout changes may join the result lets them be committed
with it; it never selects automatic landing. Add `--auto-land
--push-authorized` to the start command: its prepared receipt then also
carries `landing: "auto-land"`, and `authority-required` names missing
publication authority before any workspace is selected. Tracked work refuses
`--auto-land` with `invalid-request`.

Land only once the focused verification passes, the post-change refactor pass
is done, and no product, scope, or architecture decision remains open. Then
deliver the committed result as [Land the retained result](#land-the-retained-result)
describes, without waiting for a landing request: a queued story's closure
lands in the same commit, with `--one-shot-identity`. In the default checkout,
all checkout content is committed together and delivered from that checkout.
Report the accepted SHA and target, and the default checkout's refresh and CI
observation as their own results: remote acceptance alone completes neither.

Stop instead, keeping the committed result and its workspace as they are, and
report what stopped it, when verification fails, a decision remains open (it
goes to the developer), or delivery stops: `ownership-changed`, a
reconciliation `conflict`, a failed recheck of a reconciled candidate, or a
second rejection. Push nothing more after such a stop. A lost or interrupted
delivery result resumes the retained candidate as described above. After the
landing, [retire the workspace](#retire-the-workspace); the default checkout
stays.

## Complete a queued story in the same commit

For a queued story, the result commit also closes the story, so remote trunk
never shows it Taken. Before committing, and after moving any lasting product
knowledge into maintained code, tests, or documentation as
[story wrap-up](../../dough-story-wrap-up/SKILL.md#assimilate-lasting-knowledge)
requires, apply its
[spent-history deletion](../../dough-story-wrap-up/SKILL.md#delete-spent-history-including-shared-records)
to this story: remove its entry with the product-backlog `complete` command,
its story section (its seed only when every remaining section is spent), and
its plan. Sibling stories and other entries stay as they are.

When landing it, add `--one-shot-identity <identity>` to `deliver`, and to
`resume` when resuming, so each fetched remote trunk is checked for the story
before anything is rebased or pushed. The stop
`ok: false, status: "ownership-changed"` means another owner now holds the
story (a Taken entry or agent profile) or its entry has left the
**Backlog list**. Nothing was pushed and your commit stays unchanged in the
workspace. Report the `ownership` and `error`, and leave the story to that
owner and the developer. A reconciliation `conflict` on the story's backlog
entry usually means the same competing change: report it instead of resolving
it by removing the other side's entry.

## Finish with no change

A supported no-change conclusion, such as behavior that already matches its
intent, has no product result. Report the evidence. For a queued story, commit
its closure alone, as in
[completing a queued story](#complete-a-queued-story-in-the-same-commit), and
stop for review as for any result; it lands only on request or under
selected automatic landing. An unlisted
request has nothing to retain: retire an owned workspace.

## Retire the workspace

After a landed result's delivery and CI completion gates pass, or after an
unlisted no-change conclusion, retire the clean workspace and its branch under
Dough Land's [Retire the worktree](../../dough-land/SKILL.md#retire-the-worktree).
The default checkout is never retired. Nothing remains to wrap up: the
delivered commit already closed any queued story.

## Escalate when the work grows

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
