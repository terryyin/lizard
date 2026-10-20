# Preparation journey

One story refinement invocation prepares its story through to a landed plan:
story refinement, then slice planning, then slice-plan refinement when slice
planning's own rule calls for it, then the landing. The result is one
preparation draft in one owned workspace, the refined story and its plan with a
recorded readiness assessment, published on the remote target as one commit.

## Continue into slice planning

Once the seed records the story's goal, scope, and key examples, `refined` is
recorded, and no question to the coordinator remains, invoke
[dough-slice-planning](../../dough-slice-planning/SKILL.md) for that story in
the same session. The seed and its recorded facts carry the ready outcome
under [judge the refinement outcome](../SKILL.md#judge-the-refinement-outcome)
into planning.

Both ready outcomes continue: Ready for slice planning and Flawless. A Flawless
story gets its one-slice plan and recorded assessment, and executing it with an
explicit skip-planning instruction stays the coordinator's choice when
execution starts.

When several stories are refined together, continue for the story the
invocation names, which is the established identity when the instruction
carries one. Each sibling refined for its boundaries ends at its own
refinement result. A story that already has an associated plan continues with
that plan instead of a new one.

## Carry the same preparation

Slice planning runs in the same workspace, branch, and session as the
refinement, on the same Preparing assignment. Its `start` for this story in
this workspace returns `continued`, so the journey has no second Established
preparation block, announcement, workspace, or branch. Keep the recorded
identity, workspace, target, and integration checkout for the later `release`
or `abandon` commands and the landing.

## Stop before planning

Two conditions end the invocation at the refinement result:

- **An open coordinator question.** A Needs human engagement outcome lists
  each expected response, and no plan is written. When the coordinator
  answers in the same session and no question remains, finish refinement and
  continue into slice planning as above.
- **An explicit refine-only instruction.** The invocation carries
  `--refine-only` from [refinement options](refinement-options.json), or an
  ordinary-language instruction to leave planning for later. It composes with
  every option and focus: apply the others within the refinement, then end
  with where the refined story waits and slice planning in that workspace as
  the next step.

[One-shot refinement](one-shot-refinement.md) is a separate journey that ends
at its review or landing. Missing context and failures stop each skill as that
skill describes.

## Land at the end of preparation

Preparation of a queued story with an announced Preparing assignment ends when
slice planning records its readiness assessment, with any slice-plan
refinement it invoked, whether planning followed refinement in this session or
began it, or when a slice-plan refinement invoked directly on that story's
plan records its reassessment. A plan that needs no refinement adds nothing
before landing: no approval step or report of its own.

At that end, with no open coordinator question and no opt-out, land the result.
The instruction that started this preparation is the advance keep instruction
for its completed result. Enter
[Validate a keep instruction before acting](preparation-disposition.md#validate-a-keep-instruction-before-acting),
which confirms the workspace holds only this preparation's result, with every
scratch observation edit reverted, then
[Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result):
stage `release` and land through Dough Land. Seed, plan, recorded facts, and
the assignment's end publish in one snapshot. The announcement's publication
authority (`--push-authorized`) covers landing to the same remote target.

The recorded assessment does not gate landing. A `ready` or `not-ready` plan
whose reasons, early probe slices, or pre-Take decisions the plan already
names is a complete result, and it lands so the coordinator sees it on the
remote target. Reasons bounded by the plan's authorized early probe slices
need no response now: they stay in the plan and the story's record. The story
stays queued with the recorder's facts: landing neither Takes it, starts
execution, nor completes it, and grants no execution authority.

An open coordinator question is a response the preparation needs before its
result is complete:

- a Needs human engagement refinement outcome;
- missing required context;
- a disputed constraint or an Escalate finding;
- a story-resplit recommendation; or
- a stopped write or recording.

Any of these stops landing: report the expected response, keep the draft and
its Preparing assignment in the workspace, and say what continues once it is
given. When the coordinator answers in the same session and no question
remains, finish preparation and land.

The opt-out is `--retain` from [refinement options](refinement-options.json),
or an ordinary-language instruction to leave landing for later: finish
preparation and record the assessment, then keep the result with its Preparing
assignment for an explicit keep. An explicit instruction not to publish, which
already prevents the announcement, also disables landing.

A landing stop keeps the handling of
[Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result):
`story-left-queue` or `release-conflict` from the release, other content in
the workspace, a publication conflict, a second rejection, or an unclear push
ends with the draft retained and nothing more pushed. Report the receipt with
the decision or rerun that continues.

Other written results, such as a decomposition seed, a session that ends at
the refinement result, or a record with no announced assignment, follow
[Decide what happens to the written result](preparation-disposition.md#decide-what-happens-to-the-written-result).

Whatever the disposition, the session then ends under
[finish with only what needs a response](#finish-with-only-what-needs-a-response).

## Hand off to execution in the same session

An execution instruction for the prepared story, with its mode, may reach the
session that holds that story's Preparing assignment while its workspace holds
unlanded preparation: a result the opt-out retained, a draft an answered
question left, or preparation that has not yet reached its landing. The
handoff lands that preparation and starts execution in the same workspace and
branch, with no second worktree, branch, landing, or Take. With nothing
unlanded, or with the worktree already retired, the ordinary start under
[Take or admit work](../../dough-execute-plan/SKILL.md#take-or-admit-work)
applies and this section adds nothing.

Resolve the start before landing, because the start reads the landed result
from the remote target and refuses one it cannot execute:

- the execution source and authority under
  [Establish execution context](../../dough-execute-plan/SKILL.md#establish-execution-context);
- the mode, the
  [publication preconditions](../../dough-execute-plan/references/trunk-publication.md#preconditions),
  and every input the start needs: workspace path and branch, identity,
  publisher ID, remote, target, and host; and
- a recorded `ready` assessment on the result: planning's own, or planless
  only under an explicit skip-planning instruction, through the recorder's
  [planless authority](../../dough-product-backlog/references/record-preparation.md#planless-authority).

Any stop here retains the draft and its Preparing assignment and lands
nothing. A refinement result with neither a plan nor that instruction has no
execution source and stops, with slice planning as the next step. An
[open coordinator question](#land-at-the-end-of-preparation) is reported with
its expected response; nothing is landed or claimed.

The execution instruction is then the keep instruction for this preparation's
result. Enter
[Validate a keep instruction before acting](preparation-disposition.md#validate-a-keep-instruction-before-acting),
which stops on an unreverted scratch observation edit as on any other content,
then
[Keep and publish the retained result](preparation-disposition.md#keep-and-publish-the-retained-result)
with `release` staged, as the landing above does. Two things differ:

- **No retirement.** The workspace continues as the execution workspace, so
  Dough Land's [retire step](../../dough-land/SKILL.md#retire-the-worktree)
  does not run and the worktree stays at the landed commit.
- **No completion report.** The session continues, so this landing gives no
  final report and no dashboard completion report of its own.

The announcement's publication authority covers this landing; the execution
instruction's authority covers the claim.

Once the keep is confirmed, run the ordinary installed
`execution-start.mjs start` under
[Take or admit work](../../dough-execute-plan/SKILL.md#take-or-admit-work)
with this preparation's workspace path and branch, the selected mode, and the
recorded integration checkout, remote, and target. It reuses the workspace at
fetched trunk and publishes the Take. In Story Branch Mode the branch becomes
the story branch, published at the Take; in Trunk Mode it stays the temporary
execution branch. Retain the receipt as your execution identity, then continue
at checkout-bound setup and the first slice. The story identity, plan path,
mode, integration checkout, dashboard reporting context, and the execution
instruction carry over.

Recovery repeats neither an accepted landing nor a claim. A stop in the keep
sequence, such as `story-left-queue`, keeps that sequence's handling: the
draft is retained, nothing more is pushed, execution has not started, and the
report carries the receipt with the decision or rerun that continues. After a
landing stop or a refused or interrupted start, rerun `release` and the same
landing, which push nothing already accepted, then the same start with the
same workspace path, branch, and publisher ID, which answers `published`,
`existing`, or `resumed`. A start that names this branch with another
workspace path is refused `setup-failed`, because the branch already exists.

Retirement belongs to
[story wrap-up](../../dough-story-wrap-up/SKILL.md#remove-execution-resources-safely),
which retires the worktree and branch through the creation record this
preparation's start wrote; nothing rewrites that record. Reporting belongs to
execution's [finish or stop](../../dough-execute-plan/references/finish-or-stop.md).

## Finish with only what needs a response

One attention judgment covers the whole preparation, whichever of the three
skills began it: refinement, slice planning, any slice-plan refinement, and the
landing are one journey. Each settled step hands on to the next without a
message of its own. After the landing or other disposition settles, apply
[completion attention](../../dough-land/references/completion-attention.md) to
the preparation as a whole, including what Dough Land's landing settled.

A preparation that landed with nothing requiring attention ends there. Its
refinement outcome, plan, concerns, readiness assessment, and landing live in
the seed, the plan, the story's record, and the remote target; the response
adds no summary of them or the story's next step. Where the host requires a
response, give its minimal native one.

These need a response, each with its expected response, its consequence, what
continues once it is given, and who can give it when known, without recapping
settled work:

- an [open coordinator question](#land-at-the-end-of-preparation) or a landing
  stop, with the draft and its Preparing assignment kept as described above;
- a result held at a selected boundary: the refine-only result, the opt-out's
  retained result, an unpublished result, or a one-shot result kept for review.
  Give the draft's workspace and branch, whether others still see the story as
  Preparing, and the concrete next action, such as slice planning in that
  workspace or an explicit keep;
- a landed plan that names a decision a person must make before the story is
  Taken: name the decision and its owner, and that execution waits for it; and
- a concern, reminder, or unfinished step that completion attention reports for
  the landing, such as a stopped refresh or held retirement.

When the developer explicitly asks for details, such as the plan, its slices,
or architecture findings, or selected an option or focus whose instruction
reports its findings, give them. A parent agent or invoking workflow that
delegated a step of this preparation receives what it needs to continue, such
as the plan path, the recorded assessment and its evidence, the result's
location, or the decision it must make, as that step's return.

With supplied dashboard reporting context, the completion report under
[dashboard completion](../../dough-land/references/dashboard-completion.md) is
the session's final operation, after the landing or other disposition settles.
A landed preparation with nothing requiring attention reports `completed` with
no message and waits for its receipt. A landed preparation that needs a
response reports `completed` with that exact response. A retained or stopped
result reports `unfinished` with the exact response above. Without that
context, make no dashboard contact.
