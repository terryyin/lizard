# Keep reported gaps owned

In planned execution, record every gap, loss, limitation, or interim behavior
named in an implementation return under `## Story obligations` in the active
plan. Read it against the selected story's goal, key examples, and exclusions;
quote the clause it touches. The plan's narrower scope cannot exclude it.
The [script](../scripts/story-obligations.mjs) checks the record's structure,
slice ownership, and quotes in the selected story section, not whether your
judgment satisfies the goal. Quotes match word for word after whitespace and
Markdown emphasis normalization. Read the actual story before choosing a
disposition; script success alone does not accept the behavior.

## Record format

The plan's `**Source:**` link must name its selected story section. Use one
`###` entry with a stable id per obligation and exactly these three fields:

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

A learning or an out-of-scope remark is no disposition. Keep learnings only
for discoveries changing remaining work. Single-slice quick execution creates
no plan: return goal-contradicting gaps to the same slice, obtain required
observations before accepting dependent promises, and retain independently
valid proof. If it continues as planned work, carry every reported gap into
the new plan's entries.

## Use the record at execution boundaries

Resolve `<installed-execute-plan>` to the installed execution skill directory,
`<PLAN.md>` to the active plan, and `N` to the current slice:

```sh
node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N
node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N
node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion
```

The listing returns full entries for the slice's returns, receiving
obligations, and open interims naming it. Carry them as promises with the
required observations before delegation. A refusal names the entry and reason
in one JSON result and exits non-zero: correct the record or required behavior
before the blocked delegation, slice commit/done transition, or completion
record. `check --slice N` treats that slice as about to finish even while its
status is `planned`. `check --completion` refuses every open return, receiving,
or interim entry. Plans without this section have zero obligations; do not
migrate them merely to use the script.

When replanning removes or renumbers slices, move their receiving and interim
obligations to the remaining slices that own their behavior and proof; update
other slice references consistently. Never drop an obligation through a plan
rewrite. Run the check before delivery; dangling references are refused.
