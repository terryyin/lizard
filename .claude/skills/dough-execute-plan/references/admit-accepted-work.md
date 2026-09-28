# Admit accepted work that no backlog list holds

When the current instruction accepts a mission for tracked work and no backlog
list holds it, admit it through the startup command in
[Take or admit work](../SKILL.md#take-or-admit-work) before its substantive work.
Work already queued starts as queued work; work already Taken continues under
its existing claim. A mission explicitly selected as
[one-shot work](one-shot.md) is not admitted unless it
[grows](one-shot.md#escalate-when-the-work-grows): `--carry` then admits it
with its edits, a queued story included.

## Decide whether a mission was accepted

A mission is an independently requested outcome you are undertaking: a bug
diagnosis or repair, test profiling or optimization, exploratory or manual
testing, a standalone review, a direct maintenance or contextual
instruction, or a retrospective's follow-up correction accepted for execution.
Admit it once the developer or parent instruction accepts it, before
diagnosis, profiling, exploration, review, or edits. The label of the
request does not decide this; the acceptance does.

These are not new missions and publish no admission:

- clarifying a request to identify its outcome, a reporting-only or
  refinement-only request, and recommendations nobody accepted for work;
- a supporting step of an active story, such as its tests, refactoring, CI
  repair, retrospective, or a nested repair: it continues under that story's
  owner. A separately authorized outcome still needs its own admission;
- preparing an already queued story, which keeps its Preparing assignment; and
- caller-selected current-branch work, which has no remote claim to publish.

Another agent's claim on the same work stops this path; report it rather than
admitting a duplicate.

## Prepare the story

First reuse or draft its story in a suitable seed in the originating checkout:
`**Identity:**`, `**Goal:**`, bounded scope, and known expectations. A new
retrospective correction already has its minimal story linked to its plan;
reuse that story and never list the plan separately. Record its
actual preparation with the product backlog
[record-state](../../dough-product-backlog/references/record-preparation.md)
operation: approach `unselected` while the approach is undecided, `planless`
only under explicit planless authority, or `planned` with its plan. Record no
assessment you have not made. Then add
`--admit --link <seed path>#<anchor> --title <entry title>` to the start
command's flags, with the link relative to the backlog directory.

## Act on the result

The command carries only that story's section (a new seed whole) and its
declared plan into the isolated claim; other local edits stay local and
unpublished. It publishes them, the Taken entry, and your agent profile in one
remote-trunk commit, and lists the carried paths as `admitted`. Admission
requires no ready assessment and grants no execution authority; later
implementation still needs its normal source, approach, and authority. Because
the draft remains in the originating checkout, a deferred local refresh is
expected and leaves the accepted admission intact. `status: "existing"` means
you already hold this claim; continue under it. A refusal starts no dependent
work: `source-refused` names missing identity, preparation, or Goal, already
queued work, or a home listed under another identity; `source-conflict` names
the `path` that fetched trunk changed differently, with both versions kept for
a human decision; `conflict` means another claim holds the work. If publication
is interrupted, resume it as the startup command describes, keeping the same
admission flags: the resumed start publishes or confirms the preserved
candidate, reconciled onto current trunk, never later edits to your drafts. An
accepted admission continues with the same checkout-bound setup as any
accepted start. Its owned workspace is the story's checkout: investigation,
observation, and any later implementation use it rather than a nested one.

## Continue into implementation

Permission to investigate, observe, or review is not permission to implement,
and Taken membership never implies readiness. An admission recorded `planless`
under the instruction's explicit planless authority proceeds under that
instruction. Otherwise, when implementation is authorized, attach it to the
same story: plan it with ordinary slice planning (or record `planless` only
under explicit planless authority), assess readiness through
[record-state](../../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion),
and publish that preparation through its ordinary keep. Recording the planned
approach also links your Taken entry to its plan; keep and publish that
backlog change with the plan and story. Then run the start command without
`--admit`. It continues your claim as `status: "existing"` with the resolved
`plan`, writing nothing: no second claim, profile, story, or backlog link. It
refuses, starting nothing, while the published approach is unselected, the
published preparation is not ready, the published Taken entry lacks the plan
link its preparation declares (record the planned approach again with
`record-state` and publish it), the originating checkout holds an unpublished
edit of the story or plan (the draft your admission published is not one), or
another agent holds the claim.

## Finish the mission

A supported no-change conclusion, such as behavior that already matches its
intent, a profile with no worthwhile change, or observation with no findings,
completes the mission; close it through ordinary
[story wrap-up](../../dough-story-wrap-up/SKILL.md). Uncertainty alone is not
completion: unfinished or inconclusive work stays Taken until it completes or
an explicit disposition returns or abandons it.
