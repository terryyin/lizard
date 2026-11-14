# Keep the source fresh on a story branch

In Story Branch Mode, the execution branch carries its own copy of the selected
source from the Take, while the developer may amend it on remote trunk. Before
each slice's done mark, the execution-complete record, and wrap-up's deletion,
compare that copy with trunk so no work is accepted against a stale source.
Trunk Mode does not use this check.

## Run the check

Resolve `<installed-execute-plan>` to the installed execution skill directory,
`<execution checkout>` to the owned workspace, `<identity>` to the Taken work's
identity, and the authorized remote and trunk from the established publication
context. Run from the execution checkout:

```sh
node '<installed-execute-plan>/scripts/source-freshness.mjs' check --workspace '<execution checkout>' --identity '<identity>' --remote '<authorized remote>' --target-ref 'refs/heads/<trunk>'
```

It fetches the remote, takes the branch's fork from fetched trunk as the Take,
and reads the Taken entry on fetched trunk for the records: with a plan link,
the story section and the linked plan; for a plan-homed correction, its plan;
otherwise the story section alone, as for a quick story or a plan linked only
on the branch. It compares each record on trunk with the working tree (an
uncommitted plan edit counts) from its `from` base: the newest trunk revision
since the Take whose text the branch copy contains, or the Take. It prints one
JSON line with the `result`, `trunkRevision`, and each record's `state`; the
result is the most severe state:

- `unchanged` (exit 0) — the branch copy contains trunk's change to each
  record. Continue the boundary.
- `amended` (exit 1) — trunk's change applies cleanly over the branch's
  edits; the record carries its `change` and `application`. Follow
  [Absorb an amendment](#absorb-an-amendment).
- `conflict` (exit 2) — trunk's change overlaps the branch's edits, or would
  change or remove a slice the branch marked `done`; the record carries its
  `reason`, `branchText` and `trunkText`, and for a done slice its `slices`.
  Follow [Resolve a conflict](#resolve-a-conflict).

A refusal exits 3 with a `reason` and message, such as a failed fetch, an
identity not Taken on trunk or at the Take, or an absent record. It accepts no
boundary: correct the input or publication context and rerun, or stop under
[human judgment](execution-decisions.md#stop-for-human-judgment). Neither
command commits, pushes, or writes to trunk.

## Absorb an amendment

1. Read each amended record's `change` against your working copy.
2. Run the command with `absorb` in place of `check`. It writes trunk's change
   into the amended records in the working tree, word for word, and exits 0
   with `written`; it writes nothing on `conflict`.
3. Add the written records to the current increment's owned paths, so its
   commit and publication carry them; wrap-up's deletion gate absorbs in a new
   increment.
4. Apply the amended source under [Accept proof](wrap-up.md#accept-proof) and
   [story obligations](story-obligations.md#record-format): a new or changed
   promise is a `return` for the current slice or `receiving slice N` for a
   named remaining slice; an amended exclusion settles its obligation with
   `excluded "<quote>"`; an added slice is remaining work under
   [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
   When that reopens work after the `## Execution complete` record, remove the
   record and rewrite it under
   [Record execution completion](finish-or-stop.md#record-execution-completion)
   once that work is done.
5. Rerun `check` until it reports `unchanged`, then continue the boundary.

## Resolve a conflict

Compare `branchText` with `trunkText` from `trunkRevision`. Reconcile in the
branch copy when you can carry trunk's amendment and preserve the agreed
requirements and their accepted proof. For example, move a branch proof note
away from the trunk edit so both can stand. For a changed or removed `done`
slice, verify that its proof still covers the agreed behavior or reopen it
under plan refinement when already authorized.

When scope, expected behavior, or what counts as done still needs a developer
decision, record `awaiting story review` or `awaiting correction review` under
[delivery step 5](deliver-the-change.md#deliver-the-change). Name the record,
both texts, the trunk revision, and the decision needed. Hold dependent work
and deliver independent safe work under
[human judgment](execution-decisions.md#stop-for-human-judgment). If the
decision keeps trunk's text, apply it to the branch copy. If it keeps the
branch's text, the developer changes trunk.

Carry reconciled records and settle their amendments under
[Absorb an amendment](#absorb-an-amendment) steps 3–4. Rerun `check` at the
blocked boundary on the reconciled working copy with its proof in place.
Continue only on `unchanged`.

## At wrap-up's deletion

On `amended` or `conflict`, wrap-up deletes nothing. Return to execution in
the same session to absorb the amendment or handle the conflict, then
re-enter wrap-up, which deletes only on `unchanged`.
