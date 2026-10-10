# Record preparation facts

Apply this after an authorized preparation write that leaves a work item with a
recorded identity in its canonical home. Call this project's installed
product-backlog recorder. Do not invent a second status grammar, free-form
readiness prose, or a parallel state file.

[dough-story-decomposition](../../dough-story-decomposition/SKILL.md),
[dough-story-refinement](../../dough-story-refinement/SKILL.md),
[dough-slice-planning](../../dough-slice-planning/SKILL.md),
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md),
[dough-execute-plan](../../dough-execute-plan/SKILL.md), and
[dough-story-wrap-up](../../dough-story-wrap-up/SKILL.md) link here from their
write or claim steps. Keep competing recording rules out of those callers.

## Resolve the recorder

Resolve this project's installed skill directory in the checkout that holds the
canonical home, the same way other installed backlog tooling is resolved:
normally `.agents/skills/dough-product-backlog` for Codex/Cursor or
`.claude/skills/dough-product-backlog` for Claude Code. Run
`scripts/product-backlog.mjs` from that directory. Supply `--file` when the
backlog is not this project's default path. Resolve backlog and home links from
this project, not from this skill's location.

If the installed script, backlog path, work-item identity, or canonical home
required for the write cannot be identified, name the gap and stop that
recording step. Preserve the caller's missing-context stop for the prose write
itself. Do not guess an identity, home, or status spelling.

## When to record preparation

Record inside the same owned preparation workspace and uncommitted disposition
as the prose write. The structured block is part of that retained draft;
[preparation disposition](../../dough-story-refinement/references/preparation-disposition.md)
still owns keep, leave-unpublished, and discard. Do not Take, move the queue,
start execution, or publish merely because recording finished.

| Authorized write | Facts to record |
| --- | --- |
| Decomposition leaves a candidate with a recorded identity | `--refinement not-refined` and `--approach unselected` |
| Refinement establishes goal, scope, and key examples | `--refinement refined`; keep an existing planned or planless approach, otherwise `--approach unselected` |
| Slice planning writes the active plan | `--refinement refined` and `--approach planned` with `--plan` relative to the canonical home |

When the work is already in **Taken**, as admitted work is, the planned
recording also links that entry to its plan in place. Keep that backlog
change with the plan and story in the same disposition, so they publish
together. A Taken entry that already links a different plan refuses the
recording with nothing written; report it, since repointing the link is a
separate decision. Queued work gains its link when it is taken.

Omit `--assessment` on these writes. A plan that still has a remaining concern
is recorded as planned, not ready. Readiness assessment is the separate step
below; do not grant execution authority here.

## Assess readiness at preparation completion

When the preparing agent finishes reviewing the current story and, for planned
work, its plan — including after planning-only or plan-refinement requests that
stop without execution — assess that content and record ready or not-ready.
[dough-slice-planning](../../dough-slice-planning/SKILL.md) and
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md) both
use these criteria. Do not invent a second readiness rule in either caller.

The recorder stores the agent's judgment and checks mechanical consistency. It
does not judge prose quality, grant Take, start execution, or substitute for
the triggering instruction's execution authority.

### Criteria

Review the canonical home (and the associated plan when planned) as they stand
now. Then choose:

| Assessment | Requires |
| --- | --- |
| `ready` | Refinement `refined`; approach is a selected `planned` path with bounded slices, mapped proof, and decisive premises settled by the consuming operation's recorded result in the named checkout or bounded by a permitted early probe slice, or an explicitly authorized `planless` path; no blocking concern remains |
| `not-ready` | At least one blocking reason naming what still blocks readiness |

A remaining slice-specific concern, unresolved goal/scope/examples, missing or
unmapped proof, an unselected approach, or a cheaply observable decisive
premise left unobserved blocks `ready`. Judge decisive premises, result-bearing
observations, and permitted early probe slices under
[slice planning](../../dough-slice-planning/SKILL.md#write-the-plan).

Presence results or observations outside the checkout the plan names block
`ready` for an unsettled premise not bounded by a permitted probe. An
observation or replay recording part of the promised journey as not covered
also blocks `ready`. Record `not-ready` with one `--reason` per blocking premise
naming it and the unreached operation; name other blocking concerns too. Clear
a premise-based reason only with a fresh observation of that premise; citing
earlier evidence again does not clear it. Once no blocking concern remains,
re-read the current basis and record `ready` without reasons.

### Planless authority

`--approach planless` is allowed only when the current human or parent-agent
instruction explicitly authorizes skipping planning (the existing skip-planning
/ planless selection). Absence of a plan file, an empty plan, or the agent's
preference alone is not that authority.

The recorder cannot see conversation authority. If skip-planning authority is
missing or unclear, refuse the planless recording path here: do not run
`record-state` with `--approach planless`, do not hand-edit a planless block,
and leave the canonical home unchanged for that attempt. Name the missing
authority and stop.

### Assessment commands

1. Confirm current preparation facts and digests (no write):

```text
node <installed>/scripts/product-backlog.mjs read-state --link <href>
```

Use the returned `basis.document` and, when planned with a distinct plan file,
`basis.plan` as the digests you actually reviewed. The basis covers the story's
own section, the seed's shared context outside other stories' sections, and
the distinct plan, so review those; other stories in the same seed do not
affect it.

2. Record the assessment on the same refinement and approach the review still
supports:

```text
node <installed>/scripts/product-backlog.mjs record-state \
  --identity <id> --link <href> \
  --refinement refined \
  --approach planned|planless \
  [--plan <path-relative-to-home>] \
  --assessment ready|not-ready \
  --expect-document <sha256-from-read-state> \
  [--expect-plan <sha256-from-read-state>] \
  [--reason <blocking-text>...]
```

Supply `--plan` and `--expect-plan` for planned work whose plan is a distinct
file. Omit both for planless. Supply one or more `--reason` values for
`not-ready`; omit `--reason` for `ready`.

Report the recorder's result and evidence. A refusal leaves the home unchanged;
report it and do not hand-edit a substitute assessment. Completing this step
still does not Take the item, move the queue, or start execution.

## Execution and resume

Authorized execution and resume consume a recorded assessment as evidence of
preparation review. They do not treat it as current authorization, substitute
it for the triggering instruction, add a mandatory readiness gate on Take, or
auto-start work from a ready badge. Membership in **Taken** or **Backlog list**
never implies or renews ready.

A digest mismatch retains the recorded Ready or Not ready judgment and reasons.
The shared reader derives `changedSinceReview`; the dashboard and established
start show "Changed since readiness review". This indication informs execution
and does not independently block an authorized start with recorded Ready.
Genuine Not ready, absent or invalid preparation and startup safeguards keep
their existing handling.

[dough-execute-plan](../../dough-execute-plan/SKILL.md) and
[dough-story-wrap-up](../../dough-story-wrap-up/SKILL.md) follow this section.
Do not invent a second readiness rule, status grammar, or parallel state file
in those callers.

### Take and resume write nothing about readiness

Take and resume change only the backlog claim under
[take queued work](../SKILL.md#take-queued-work-for-execution). They must not
run `record-state`, hand-edit a story-state block, write `--assessment ready`,
or infer ready from queue membership, a plan link, or resume alone. Reading a
story never triggers a write.

### Plan evidence during delivery

When delivery updates an active plan — slice `Status: done`, accepted proof,
learnings, or revised remaining slices — publish that plan evidence through the
existing delivery path. Do not call `record-state` with `--assessment ready` (or
otherwise renew readiness) merely because a slice finished. The shared reader's
digest basis treats a changed plan as mismatched: `read-state` retains the
judgment and reports `changedSinceReview: true` until an agent actually reviews
the current content and records a new assessment through
[assess readiness at preparation completion](#assess-readiness-at-preparation-completion).

Preserve recorded done status and accepted proof in the plan. They are
completion evidence, not a readiness renewal.

### Scope-changing execution writes

When authorized execution or replanning changes story or plan scope (including
in-place plan refinement of remaining work), leave the prior ready assessment
visible alongside its change indication until a real new assessment is recorded.
The scope author still owns story/plan alignment and readiness review; existing
replanning paths own the prose rewrite. After reviewing changed content, record
ready or not-ready with the current digests — never by copying the old basis or
auto-renewing on the write that caused the mismatch.

### Accepted work and context-only quick execution

An independently accepted mission that no backlog list holds, including a
contextual instruction, gets a minimal story whose actual facts are recorded
here before [admission](../../dough-execute-plan/references/admit-accepted-work.md):
`unselected` while undecided, `planless` only under explicit planless
authority, with no assessment not actually made. Implementation later
authorized for it attaches its plan and assessment to that same story through
the procedures above; admission and Taken never renew or imply ready.

[One-shot execution](../../dough-execute-plan/references/one-shot.md), which
starts on fetched remote trunk or in the selected default checkout without a
claim, and a supporting step of an active story create no canonical home,
plan file, story-state block, or queue entry. Keep their scope, decisions,
progress and proof in the conversation or active story. Do not fabricate a seed, plan, or `record-state` write.

### Wrap-up cleanup

Closure under [dough-story-wrap-up](../../dough-story-wrap-up/SKILL.md) deletes
spent source and plan history. Removing a canonical home removes its story-state
block; there is no catalog tombstone. Preserve source/plan cleanup and Git
history recovery. The done record `complete` writes outlives that history.

## Canonical homes

- **Anchored feature story:** `--link` is the seed path plus the story anchor
  (for example `seeds/SEED-021-example.md#first-story`). `--identity` must match
  the identity that home already records.
- **Plan-homed correction:** when an existing correction's plan is its
  canonical home, `--link` is that plan path (for example
  `slice-plans/075-correction/PLAN.md`). For `--approach planned`, `--plan` is
  relative to that file; use the plan's own basename (for example `PLAN.md`)
  so the association stays on the same document. A new correction's story is
  an anchored story like any other.

Create or update the plan file before recording a planned approach; the
recorder refuses a missing plan path.

## Preparation-only commands

After the matching prose write, before assessment:

```text
node <installed>/scripts/product-backlog.mjs record-state \
  --identity <id> --link <href> \
  --refinement not-refined|refined \
  --approach unselected|planned|planless \
  [--plan <path-relative-to-home>]
```

Use the `read-state` command above to check without writing.

`read-state` reports `not-recorded`, recorded refinement/approach, and any
assessment view. Use it to confirm the facts just recorded. A refusal leaves
the home unchanged; report it and do not hand-edit a substitute block.
