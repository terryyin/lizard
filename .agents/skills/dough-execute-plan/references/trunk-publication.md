# Publish execution results

Use the [shared candidate procedure](publish-the-candidate.md) for execution
publication. Follow [wrap-up closure publication](wrap-up-closure-publication.md)
for before-cleanup and final-closure commits.

## Publish a claim

[Take or admit work](../SKILL.md#take-or-admit-work) publishes and confirms the
claim on remote trunk before implementation. Retain its accepted SHA and
recovery coordinates. Apply [claim coverage](ci-monitor.md#own-one-observer)
when arming observation, including the unobserved trunk claim in Story Branch
Mode. If workspace setup fails after acceptance, keep that claim and any
verified workspace under [execution location](execution-location.md).

## Publish an execution increment or repair

After accepting proof and the refactor disposition, use
[guarded fresh preparation and delivery](fresh-increment.md) to prepare,
commit and deliver new work. A failed preparation or commit withholds
publication and observation. Use [managed delivery](#publish-the-candidate)
for an existing committed candidate. Use
[resume](#resume-an-interrupted-publication) after an unknown push result.
For CI repairs, apply the [CI pause and restore procedure](ci-monitor.md#handle-a-notification)
and use this delivery entry point.

Retain the recorded workspace and publish only with the caller's authority.
Story Branch Mode targets its remote execution branch. Trunk Mode targets
remote trunk. Caller-selected current-branch or host-owned execution uses its
own authorized target. Without authority, keep the result local and report
publication pending.

## Preconditions

Apply [shared publication preconditions](publish-the-candidate.md#preconditions).
The owned workspace is the execution checkout. The owned suffix is the
Taken commit for a claim, or the consecutive unpublished execution commits
for an increment or repair. A claim's proof confirms the selected entry is
**Taken** in a nonempty commit. An increment or repair uses accepted proof
whose promise, boundary, implementation, setup and observations still apply.

Supply the recorded previously published base as a full or unique abbreviated
commit ID. For caller-selected local work use the recorded checkout's HEAD
before its first commit. Resolve any default-checkout preservation under
[maintain the default checkout](maintain-default-checkout.md#preserve-pending-local-work).

## Publish the candidate

After [preconditions](#preconditions), run the installed command once for an
existing committed candidate. Resolve `<installed>` to this project's
installed skills directory, usually `.agents/skills/` or `.claude/skills/`.

```text
node <installed>/dough-execute-plan/scripts/execution-increment-delivery.mjs deliver \
  --mode <mode from the established start> \
  --workspace <owned workspace> --branch <execution branch> \
  --previously-published-base <previously published base SHA> \
  --target-ref <authorized target ref> --repo <owner/repo> --host <host> \
  --authority <publish|local-only> [--tracking one-shot] [--validated-candidate <accepted proof SHA>] \
  [--session-json <json>] [--coordinator <value> --observer-directory <directory>] \
  [--default-checkout <path>] [--one-shot-identity <identity>] [--remote <name>] \
  [--landing-context <supplied absolute file>]
```

Use `--mode story-branch` with `refs/heads/<execution branch>` or
`--mode trunk` with `refs/heads/<trunk>`. Caller-selected current-branch
and host-owned work pass `--mode trunk` with their authorized ref. For a
one-shot landing follow [Land the retained result](one-shot.md#land-the-retained-result).
Supply any first-trunk [landing context](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).

Use the [shared exact-candidate sequence](publish-the-candidate.md#publish-the-candidate)
through this command. Retain `candidate` and `suffixBase` from `beforePush`
together, including rewrites, and use the accepted result's receipt and base.

### Bind observation

Managed delivery resolves the checkout's CI runtime, establishes or reuses
this coordinator's observer of the authorized target and attaches the accepted
SHA. Use these owner inputs with `deliver`, `resume` and `finish` where applicable.

- On Claude Code or Cursor, run through the coordinator's own Bash or Shell
  tool. It supplies `CLAUDE_CODE_SESSION_ID` or `CURSOR_CONVERSATION_ID`.
  For a call lacking that identity, pass `--session-json` with the same
  coordinator's `session_id` or `conversation_id`. A Claude observer claimed
  with `agent_id` requires both `session_id` and `agent_id` in that JSON.
  Explicit JSON is authoritative. Malformed JSON stops the command.
- On Codex, pass `--host codex`, `--coordinator` and the exact
  `--observer-directory` from the [armed stream](ci-notify-codex.md).

Keep the returned observation directory and `observation.notifies` when
present. Reuse only this coordinator's observer for the target, including
from another worktree of the repository. For missing owner input, use the
receipt's named input on the next `deliver`. For an `ambiguous` gap, keep the
retained directory and [stop](ci-notify-hosts.md#stop-for-cancellation) this
coordinator's other observers of that target. A replacement coordinator
stops the previous recorded observer and attaches its own on `deliver`.
Use managed delivery to establish and register observation. On Codex, arm
the named stream first under its adapter. Keep other coordinators' observers
in place.

After acceptance on remote trunk, apply
[refresh eligibility](maintain-default-checkout.md#refresh-eligibility).
Execution-branch delivery leaves the default checkout alone. Report remote
acceptance, coverage and
[maintenance](maintain-default-checkout.md#independent-maintenance-outcome)
as separate results. A coverage gap or maintenance stop leaves acceptance
intact. For a maintenance hold apply
[human judgment](execution-decisions.md#stop-for-human-judgment),
[delivery staging](wrap-up.md#deliver-the-change) and
[execution resume](../SKILL.md#continue-or-recover-at-an-execution-boundary).

### Act on a stopped result

| Result | Action |
| --- | --- |
| `unresolvable-base` | Read `problem` (`not-an-object-id`, `unknown`, `ambiguous` or `not-a-commit`). Use the full commit ID from the delivery receipt. |
| `unpublished-base` | Report the local base the remote lacks for the developer to resolve. Keep the recorded base. |
| Proof stop | Retain the rewritten candidate and base. Renew affected proof under [fresh recovery](fresh-increment.md#publish). |
| Candidate mismatch | Preserve the fresh commit receipt and follow [fresh recovery](fresh-increment.md#publish). |
| `transport-timeout` | Read the stalled `stage` and preserve the named candidate. Retry the same delivery or resume with its retained inputs. With `pushIssued` true, acceptance is unknown until a new fetch settles it. |

## Recover a rejected push

Before retrying a claim, startup checks membership, provenance and the
selected source on the fetched target. Recognize this execution's owned
claim without a second push. A rival or ambiguous claim preserves the
workspace for recovery. Establish ownership through provenance beyond the
Taken entry's text. An absent claim with an unchanged source uses the
[shared retry bound](publish-the-candidate.md#recover-a-rejected-push).

## Resolve a publication rebase conflict

Follow [publication rebase conflict](publication-rebase-conflict.md) for
adapter routing, resolution and the stop for unresolved identity or intent.

## Resume an interrupted publication

Use the original execution resources and target. Resume a claim from its
retained startup coordinates. For an increment, start from
[the retained pair](publish-the-candidate.md#resume-an-interrupted-publication).
After guarded fresh, use `recovery.candidateSha` and `recovery.suffixBase`
with the recorded owner context. Follow [fresh recovery](fresh-increment.md#publish)
for proof stops and mismatches. An interrupted push needs no new commit.

```text
node <installed>/dough-execute-plan/scripts/execution-increment-resume.mjs resume \
  --workspace <owned workspace> --candidate-sha <retained candidate SHA> \
  --suffix-base <retained suffix base SHA> \
  --target-ref <authorized target ref> --repo <owner/repo> --host <host> \
  [--session-json <json>] [--coordinator <value> --observer-directory <directory>] \
  [--default-checkout <path>] [--superseded-sha <pre-rebase SHA>]... \
  [--one-shot-identity <identity>] [--remote <name>] \
  [--landing-context <supplied absolute file>]
```

Reuse the original launch context for a pending
[first trunk landing](../../dough-land/references/dashboard-completion.md#retain-the-launch-landing).
Use the returned receipt and `suffixBase` as the delivery comparison, then
continue the caller's remaining duties. For a published revision missing
from its observer's coverage, resume registers it once on the recovered
[live owner](#bind-observation). Resume starts no observer. The Story Branch
claim's unobserved trunk coverage follows [claim coverage](ci-monitor.md#own-one-observer).

With `publication: "accepted"` and unobserved coverage, report the gap and
use `observation.ownership` and `reason` to recover the owner. If a
replacement is needed, stop the recorded observer and establish the new
coordinator's observer by [managed delivery](#bind-observation). Rerun the
same resume once the owner input or observer is recovered.

## Preserve remaining state

Apply [shared preservation](publish-the-candidate.md#preserve-remaining-state).
Keep the recorded target and stop implementation when claim acceptance or
its required workspace setup remains unresolved.
