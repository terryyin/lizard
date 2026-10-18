---
name: dough-land
description: >-
  Dough Land lands everything in one reviewed checkout on the authorized remote
  trunk: commits all of its changes, reconciles and publishes them without
  force, and refreshes a supplied default checkout when safe. An owned worktree
  is retired with its branch after completion gates hold and trunk contains its
  work; the default checkout stays in place. Ordinary landing refuses unfinished
  execution before commit; `--keep-worktree` selects an explicit retained increment.
  Use only on explicit invocation, such as "/dough-land", "use Dough Land",
  "land this worktree", or "land" for changes made on the default checkout, or
  when another skill's validated keep instruction links here. A casual
  "keep", "looks good", or approval does not invoke it.
  Hosted merges and pull requests are out of scope. With
  `--process-retrospective`, it also reviews the landing's own process and
  records supported findings in `DearDough.md`.
---

# Dough Land

Land one reviewed checkout, an owned worktree or the default checkout on the
target branch: commit everything in it, publish that onto the authorized remote
trunk, refresh a supplied default checkout when safe, and retire an owned worktree
only after completion and retirement gates hold. Apply [completion attention](#completion-attention) to the final
response. Publication, refresh, and cleanup are separate results.
A later step that stops or is deferred never undoes an earlier one.
With `--process-retrospective`, also
[review this landing's process](#review-this-landings-process).

Run only when the developer explicitly invokes Dough Land, or when a calling
skill's own validated keep instruction links here. Reviewing, approving,
pausing, or saying "keep" in passing does not start a landing. Do not open a
pull request, request a hosted merge, or discard anything; when the developer
wants one of those, stop and say that Dough Land does not do it.

## Resolve the checkout and target

Resolve both before any commit:

- **Landing checkout.** The checkout named by the invocation or its context:
  normally the current one, or the owned workspace a calling skill recorded.
  When the context says the change sits on the default checkout, that checkout
  is the one to land. Record its path and branch, whether this work created it
  or reused one another workflow owns, and its starting revision when known.
  Ownership comes from
  that context or the worktree's
  [creation record](../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it);
  do not infer it from a clean directory. In an
  [established session](../dough-execute-plan/references/established-session.md),
  the session's block names this checkout, its role, starting revision, and target.
- **Default checkout.** The project's established checkout for ordinary work,
  when the context supplies one; the refresh step inspects it. Landing needs no
  default checkout. When the landing checkout is a worktree, the default
  checkout is a different one; when the change sits on the default checkout,
  it is the landing checkout and needs no separate refresh source.
- **Target.** The authorized remote target as `refs/heads/<branch>` on a named
  remote: the target selection already recorded for this worktree, or the one
  the project or developer authorizes. Default the branch to `main` only when
  neither the caller nor the project supplies one.

Stop before committing, and name the gap, when:

- no checkout is in context, or more than one candidate fits;
- the named default checkout is on another branch than the target; name both
  branches;
- the target is missing, contradictory, or not a branch on an authorized
  remote; or
- the checkout has an `index.lock` or an unfinished merge, rebase,
  cherry-pick, or revert. Name it; the developer resolves it, then reruns.

## Establish completion before landing

Before staging, committing or publication, resolve whether the checkout serves
execution, preparation, or ordinary reviewed work from its retained context and
this project's selected work. Taken membership alone establishes neither
unfinished implementation nor completion. A Preparing assignment is preparation,
not execution; completed preparation keeps its existing release and landing
journey. Do not label ordinary reviewed work unfinished merely because it has no
story or plan.

For execution, apply wrap-up's
[execution-completion judgment](../dough-story-wrap-up/SKILL.md#establish-execution-completion)
to the selected source and available results. For a resolved executable plan,
check its recorded slices and obligations before any Git change:

```sh
node <installed>/dough-execute-plan/scripts/story-obligations.mjs check --plan <PLAN.md> --planned-completion
```

Use this project's selected plan, not a plan found by guessing from this skill's
location. The command reuses the existing plan and obligation readers: it names
pending slices, open obligations, and unreadable or missing slice evidence. A
clear result checks those records only; judge the promised outcome and accepted
proof too. For planless execution, judge its instruction, changes and execution
results; absence of a plan is not completion evidence.

On an ordinary invocation, unfinished execution or missing or ambiguous completion
evidence stops here, before commit or publication. Keep the checkout, branches,
index, plan and assignment unchanged; name the remaining work or exact missing
evidence and explain `--keep-worktree` as the explicit retained-landing choice.
Never infer that choice from a request to land. A completed execution still Taken
is not refused merely for that membership: preserve its owning completion and
retirement gates. Landing does not perform wrap-up, release its assignment, or
manufacture completion.

On every rerun, establish completion again before commit or publication. A tip
already accepted by trunk does not establish completion or authorize retirement.

An explicit `--keep-worktree` selects a retained increment, including when
execution is unfinished or its completion evidence is unavailable. Carry the
remaining work or evidence gap forward; it is not a completion result. Continue
through commit, publication and optional refresh, then skip retirement entirely.
Keep the same worktree, local branch and existing remote execution branches, Taken entry, agent
profile, plan and session context. Do not release an execution assignment or
change its mode, branch destination for later execution, or observer target.
Resume its next unfinished work in that same workspace through its existing
execution workflow; eventual closure still belongs to ordinary story wrap-up
and its completion, CI and ownership gates.

## Commit everything in the checkout

Everything in the checkout is the reviewed change, including unrelated files
and local commits the fetched target lacks. Stage all tracked, untracked, and
deleted paths (`git -C <checkout> add -A`) and commit them there with a
message describing the reviewed change, as an
[agent commit](../dough-execute-plan/references/agent-commits.md) when that
reference applies to the checkout, otherwise with plain `git commit`; its
refusal stops the landing before publication. When there is nothing to commit, create no commit; that is the
normal rerun case.

The owned unpublished suffix is every commit on the checkout's branch that
the fetched target does not contain. Its previously published base is the last
SHA this landing recorded as accepted, otherwise the checkout's recorded
starting revision, otherwise the merge base of the branch and the fetched
target (for the default checkout, its local HEAD before the commit when that
is contained in the fetched target).

A calling skill that must land only its own record checks, before linking
here, that the worktree holds nothing else. Dough Land does not pick paths out
of a worktree. A preparation keep stages its assignment's release in the
worktree before linking here, so it lands with the result; Dough Land itself
never adds or removes an assignment profile.

## Publish

With the original launch's supplied dashboard landing context, use the
[launch-bound handoff](references/dashboard-completion.md#retain-the-launch-landing)
for publication and resume. Record its accepted comparison before refresh or
retirement; reporting failure leaves Git acceptance accepted and retries only evidence.

Apply [reviewed-checkout publication](references/publish-reviewed-checkout.md):
use [publish the candidate](../dough-execute-plan/references/publish-the-candidate.md)
for a wholly unpublished suffix, or history-preserving integration for a published
prefix and the complete reviewed tip above it. Keep the queued-closure check
before each reconciliation and retry, and recheck affected proof before pushing.
Preserve the state on a conflict, refusal, failed recheck or second rejection;
report publication, refresh and cleanup as unfinished. Do not loop.

## Visit consumers of completed selected work

After accepted publication and before retirement, apply the shared
[supplier dependency procedure](../dough-product-backlog/references/supplier-dependencies.md)
when retained context establishes a selected completed supplier. Preserve its
recoverable outcome before cleanup, publish directly justified consumer updates
through this authorized landing workflow, and report unresolved work. A generic
landing or an unfinished increment establishes no supplier completion.

## Review this landing's process

Only with `--process-retrospective`, after accepted publication and any
consumer visit, apply the shared
[process review of a run](../dough-execution-retrospective/references/process-review-of-a-run.md)
once per landing, with the landing checkout as the write location. Recorded
findings are uncommitted changes: continue through
[Commit everything](#commit-everything-in-the-checkout) and [Publish](#publish)
before refresh and retirement.

## Refresh the default checkout

After acceptance, when the landing checkout is the default checkout, it is
already at the accepted SHA: record the refresh as already current, or as a
fast-forward to the published trunk when the publication moved it. Otherwise
attempt
[Refresh eligibility](../dough-execute-plan/references/maintain-default-checkout.md#refresh-eligibility)
on the supplied default checkout for the landed remote and branch.
Record its
[maintenance result](../dough-execute-plan/references/maintain-default-checkout.md#independent-maintenance-outcome)
and reason separately from publication; apply [completion attention](#completion-attention)
when responding. No refresh result is a failed landing
or blocks retirement.

Another skill may apply this section on its own after its own accepted
publication.

## Retire the worktree

Apply [owned worktree retirement](references/worktree-retirement.md) after
publication, the [completion judgment](#establish-completion-before-landing), and
the calling workflow's retirement gates. It checks recorded
ownership and target containment, preserves recoverable state on a hold, and
keeps the default checkout in place. Retain supplied dashboard reporting context
outside the checkout before removal; reporting remains the final operation.
With `--keep-worktree`, do not call retirement, even when trunk already contains
the tip. Verify the retained resources and record their disposition separately
from publication and refresh.

## Stop, rerun, and report

A stop keeps every resource recoverable (worktree, branch, commits, index, any
default checkout, and the remote as it is) and names the exact unfinished
step. A rerun reads real Git and remote state and continues from the first
unfinished step, applying [completion judgment](#establish-completion-before-landing)
before any commit, publication or retirement:

| State found | Continue with |
| --- | --- |
| Unfinished Git operation in the checkout | Stop and name it |
| Uncommitted changes | [Commit everything](#commit-everything-in-the-checkout) |
| Branch tip not contained in the fetched target | [Publish](#publish), through the publisher's [resume](../dough-execute-plan/references/publish-the-candidate.md#resume-an-interrupted-publication) |
| Tip already contained | Record it as accepted; push nothing. Apply optional refresh, then retain with `--keep-worktree` or apply completion and retirement gates |
| Worktree or branch already absent | Record it as already retired |

Never commit the same change twice, push an already accepted candidate again,
or create a replacement worktree.

Keep publication acceptance, refresh outcome, and verified resource disposition
as separate operational facts in the command results and available execution
context. Preserve the accepted SHA and target, refresh result and reason, and
cleanup paths and verified disposition for recovery; no new recap or record is
required solely to repeat them.

## Completion attention

After operations settle, read and apply the shared
[completion attention rule](references/completion-attention.md), including its
[final dashboard operation](references/dashboard-completion.md) with supplied reporting context.
For retained unfinished execution, state that the reviewed changes landed,
the story remains unfinished, and its workspace and branches remain usable;
name the next unfinished work or missing evidence. Report `unfinished` with that
exact attention message through the supplied dashboard command. Never send quiet
`completed`, mark the session Done, or imply story completion from publication.
Manual session acknowledgment does not complete slices or release the assignment.
