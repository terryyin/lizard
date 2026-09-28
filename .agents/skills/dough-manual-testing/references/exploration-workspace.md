# Own a temporary exploration workspace

This is the shared checkout ownership lifecycle. It owns safe selection, the
retained local checkout role, and separate target selection. Manual testing,
bug fixing, preparation, and execution use it for that ownership. Each caller
supplies its own continuation: exploration use, preparation disposition, or
execution mode, project-command readiness, and execution resume. This
lifecycle does not choose when selection runs, whether a claim is published
first, or what happens to a written record.

## Select the checkout

The caller supplies the verified base revision and chooses when this step
runs. When the caller does not supply a different verified base, verify the
current Git revision and use that.

First determine whether a story, active plan slice, explicit caller selection,
or another workflow already owns a checkout for the work. Use that checkout and
do not create a nested temporary workspace. Its owning workflow retains delivery
and cleanup responsibility.

Otherwise create one temporary branch and paired worktree from the verified
base revision
(for example, `git worktree add <path> -b <branch> <verified-revision>`).
Record the result under
[Record local checkout role and target selection](#record-local-checkout-role-and-target-selection).
If creation only partly succeeds, retain and report the observed state
instead of guessing that it is safe to retry or remove.

A directory `git worktree list` shows is not automatically this work's
checkout. Verify that a candidate is the checkout the current story, plan,
session, host, or caller already owns before using it. Treat an unverifiable
or ambiguous match the same as a missing checkout.

Resolve the location from this project's conventions and ordinary host Git
facilities. Use no parallel registry, configuration format, or worktree
manager. Missing conventions or an unsafe location stops selection.

## Record local checkout role and target selection

Record these as separate facts. Use the actual paths established for this
selection. Do not substitute a role name for a path, or a remote ref for a
local checkout.

**Local checkout role.** Record each local path and the role it has for this work:

- owned workspace: its worktree path, branch, and starting revision, and
  whether this session created it or reused a checkout another workflow
  already owns;
- originating checkout: the path this work was invoked from, when that path
  is not the owned workspace;
- integration checkout: the project's established local checkout for ordinary
  work, when the caller has one. This path is a local role. It is not the
  publication target.

**Target selection.** When this work publishes, separately record the
authorized remote target. Resolve the remote and branch from the caller or
the project. Default the branch to `main` only when neither caller nor
project supplies one. The target is a remote ref. Recording it does not
check out or advance the integration checkout. Callers that do not publish
omit target selection and still record the local checkout role.

## Use and resume it

Use the same workspace for all checkout-bound preparation, exploration,
evidence handling, and reporting in that session. Do not create a second
workspace alongside it. Leave the originating checkout unchanged throughout
the session unless the calling skill explicitly begins its own post-exploration
integration sequence.

After an interruption, verify the recorded worktree path, branch, and starting
revision against actual Git state before reusing the workspace. When the record
includes whether this session created the workspace, verify that too. If the
identity cannot be verified, stop and report it rather than continuing in a
possibly different checkout.

The workspace isolates only the Git checkout. Shared accounts, services,
databases, and other external state remain governed by their existing rules.

## Close or retain it

Before cleanup, remove session-owned temporary artifacts and resolve any
caller-authorized durable evidence through the caller's own preservation and
integration rules. Remove only a clean, unambiguous workspace this session
created, then delete its branch (`git worktree remove`, followed by safe
branch deletion). A reused or host-owned workspace stays with its owning
workflow; retain and report it instead of removing it.

If cleanup would be unsafe because of dirty state, unresolved evidence,
ambiguous ownership, or an identity mismatch, do not force, reset, or delete
anything. Retain and report the exact branch, worktree path, starting revision,
and evidence state so the session can be resumed or deliberately disposed of.
