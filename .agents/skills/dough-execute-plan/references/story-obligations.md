# Keep story gaps owned

In planned execution, record every gap, loss, limitation, or interim behavior
named in an implementation return or independently discovered during
[acceptance](wrap-up.md#accept-proof) under `## Story obligations` in the active
plan. Read it against the selected story's original Goal, Scope, key examples and exclusions;
quote the clause it touches. The plan's narrower scope cannot exclude it.
The [script](../scripts/story-obligations.mjs) checks the record's structure,
slice ownership, and quotes in the selected story section, not whether your
judgment satisfies the goal. Quotes match word for word after whitespace and
Markdown emphasis normalization. Read the actual story before choosing a
disposition; script success alone does not accept the behavior. For an independently
discovered gap, retain its discovery observation in the existing `Reported` text
or title; do not invent an implementation report or a second record format.

## Check the paths before closing a gap

Before choosing `no user cost` or `proved`, connect the reported state to the
selected story's actual promise. Inspect the relevant entry, continuation,
failure/recovery, and handoff paths in this project's guidance and
implementation: what can produce that state, what consumes it, and what result
the developer receives. The return's stated cause and the current slice's
command do not establish the only way to reach it. Follow a directed action
through its continuation to the promised result; a passing initial guard does
not establish what committing, rerunning, or handing work to another owner does.
Inspect only paths relevant to this gap, without a routine path inventory.

Reuse sufficient inspection and observations under
[executable proof](../../dough-story-refinement/references/executable-proof.md).
Check what the accepted evidence actually observes at the relevant boundary;
do not extend a helper's passing result to an unobserved continuation. When
reachability or the promised result remains uncertain, obtain the smallest safe
missing observation within authority, or keep that dependent promise open and
name its missing proof. Retain independently accepted evidence. A reachable
contradiction returns to the current slice, unless a valid receiving or interim
owner already owns its correction and proof under the dispositions below.
Preserve the gap's id, original text and reporting provenance. A genuine story
exclusion or developer-authorized changed promise retains its authority; path
inspection does not authorize changing scope.

Record the inspected path and result in the existing disposition reason or
proof location, with ordinary acceptance evidence; add no field or entry per
path. Close a harmless limitation as `no user cost` when its concrete reason
holds through the relevant paths, and use `proved` when sufficient current
evidence covers the promised result. Continue without another approval,
unrelated obligations, or a blanket rerun of accepted observations. Reassess
that reason or proof when a changed path invalidates it. Create no obligation
when neither the return nor acceptance identifies a gap.

## Record format

The plan's `**Source:**` link must name its selected story section. Use one
`###` entry with a stable id per obligation and exactly these three fields,
plus the optional `Commitment:` field described under
[the source review](#record-the-source-review):

```markdown
## Story obligations

### G1. Results panel omits the source
Reported: slice 4 — "The results panel renders results without their source."
Story clause: "Each result names its source"
Disposition: return
```

Keep the reported quotation and the story clause when ownership moves. A
`return` is owned by the slice number in `Reported`; it has no separate
recipient. When a later slice returns an interim or receiving obligation,
update that number to the current slice and retain the original reporting
slice in the entry title, for example `G1. Record during Stop (first reported
in slice 1)`. Until such a return, a receiving or interim entry retains its
reporting origin. Earlier slices' independently accepted proof stays accepted.

Choose exactly one disposition:

- `return` — implementation corrects and proves it in the current slice.
  Use this for a gap contradicting the goal or a key example, even when the
  story never enumerated that gap. A test pinning the loss is not proof.
- `receiving slice 8` — that named later planned slice implements and proves
  it; its delegation carries the obligation and required observation.
- `interim until slices 2, 3` — provisional behavior stays open through the
  named dependent slices. At each dependent slice's acceptance, re-read it
  against the story examples. If that slice makes it produce a wrong result,
  change it to a current-slice `return`; otherwise keep it open until final
  behavior is proved. The last named slice cannot finish while it is open.
- `excluded "<story quote>"` — the story itself explicitly excludes or defers
  it. A genuine exclusion requires no rework; an unresolved decision about a
  deferral stops only that path. A plan or return's scope remark is no quote.
- `owner changed "<story quote>"` — the developer has authorized a changed
  promise and recorded it in the story; cite that text, never silently weaken
  the promise to accept an omission.
- `no user cost "<goal quote>": <reason>` — it lies outside the goal and
  costs the user nothing; record the goal clause and the concrete reason.
- `proved by slice 4: <proof location>` — name the slice and inspected,
  sufficient current proof accepting it. Once supplied, continue without
  another approval or blanket rerun.

A learning or an out-of-scope remark is no disposition. Neither is a plan
Current decision, slice text, proof row, implementer statement or owner-decision
label: quoting one fails `quote-not-in-story`. Keep learnings only for
discoveries changing remaining work. Single-slice quick execution creates
no plan: return goal-contradicting gaps to the same slice, obtain required
observations before accepting dependent promises, and retain independently
valid proof. If it continues as planned work, carry every reported or discovered gap into
the new plan's entries.

## Record the source review

In planned execution, keep the source-only reviewer's numbered return beside
the plan as `source-review.md`. When you retain it, link it from the plan's
header with one line under `**Source:**` that starts with `**Source review:**`
followed by a Markdown link to that file. Keep the launch and input
description above one `## Commitments` table with one row per returned id: the
id, the source text as returned, the allocated slice, and the observation or
the obligation id that disposes it.
Escape a `|` inside a cell as `\|`.

```markdown
## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 4a.1 | Listed with its filter: outside the Area | slice 4 | observed: decision.test.py excluded list |
| 4a.5 | Listed with its filter: already held | slice 4 | G2 |
| 7 | Each decision has an identity | slice 7 | |
```

- **Allocate** every id when you retain the return or replace it after a story
  amendment, before the next `list` or `check`: each check refuses an
  unallocated row. Give it the slice whose output owes the commitment, or an
  obligation id when an obligation disposes it instead (receiving a later
  slice, excluded with a story quote, or owner changed). A whole-story
  commitment, such as the goal, belongs to the slice that completes it,
  usually the last. Correct a wrong allocation at any later acceptance.
- **Observe** at each acceptance: for every id allocated to this slice, write
  `observed: <product or proof location>` once inspected, or the id of the
  obligation that owns the gap. The location must show the commitment's own
  expected result, as the source text states it: a location showing a
  different or narrower result, such as an item kept somewhere other than
  where the commitment lists it, is a mismatch to record as an obligation,
  not an observation. An observation recorded at an earlier slice
  stays valid while its boundary is unchanged; do not rerun it for process.
  An id allocated to a slice already done when you allocate it cites that
  slice's earlier recorded proof as `observed:` when that proof shows the
  result; otherwise record its obligation.
- **Link** an obligation back, optionally, with `Commitment: 4a.5, 4a.6` after
  its `Disposition:`. When present, it must name exactly the rows whose last
  cell is that obligation's id, or the check refuses `mismatched-commitment`;
  refusals name those rows either way.

A record that is no disposition is no observation or allocation reason either;
resolve the mismatch under [accept proof](wrap-up.md#accept-proof). Every
check refuses `unlinked-source-review` for a `source-review.md` beside the plan
without the exact `**Source review:**` link, or a `Source review` label in any
other form. A plan with neither keeps obligation-only checks.

## Use the record at execution boundaries

Resolve `<installed-execute-plan>` to the installed execution skill directory,
`<PLAN.md>` to the active plan, and `N` to the current slice:

```sh
node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N
node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N
node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion
```

The listing returns full entries for the slice's returns, receiving
obligations, and open interims naming it, and, when the plan links a source
review, `commitments`: the rows allocated to the slice. Carry them as promises
with the required observations before delegation. A refusal names the entry and
reason in one JSON result and exits non-zero: correct the record or required
behavior before the blocked delegation, slice commit/done transition, or
completion record. `check --slice N` treats that slice as about to finish even while its
status is `planned`. `check --completion` refuses every open return, receiving,
or interim entry. With a linked source review, every check also refuses
`unallocated-commitment` (a row with neither a slice nor an obligation, or
naming a missing slice even with an obligation), `unobserved-commitment` (a
row allocated to that slice, or to a done one, without an observation or an
existing obligation, and at completion any such row), `unknown-commitment` (a
`Commitment:` id the return lacks), and the
[record's](#record-the-source-review) link refusals. Plans without this section have zero obligations; do
not migrate them merely to use the script.

For a caller that already resolved a planned execution, `check --planned-completion`
also requires a readable, non-empty ordered slice list with every slice done,
then applies the same completion obligation check. It names pending or ambiguous
slices and missing evidence; it does not discover execution identity or establish
accepted proof. An absent plan or a planless instruction still requires the
caller's completion judgment. This option takes neither `--slice` nor `--completion`.

When replanning removes or renumbers slices, move their receiving and interim
obligations to the remaining slices that own their behavior and proof; update
other slice references consistently. Never drop an obligation through a plan
rewrite. Run the check before delivery; dangling references are refused.
