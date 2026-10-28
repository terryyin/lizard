# Guard a fresh execution increment or owned repair

Use this after accepting semantic proof and completing the independent refactor
for a fresh increment or owned repair. Keep the recorded execution checkout,
branch, ownership, and project hook contract. This operation performs mechanical
preparation, commit, and authorized managed delivery in dependency order;
successful commit does not establish semantic proof.

Resolve the applicable requirements from this project's execution context. Put
their inputs in a temporary JSON file outside the commit's owned paths, and put
the intended commit message in a separate file. Invoke the installed command
directly from the execution checkout:

```sh
node '<installed-execute-plan>/scripts/execution-increment-delivery.mjs' fresh \
  --mode story-branch --workspace '<execution checkout>' --branch '<recorded branch>' \
  --authority publish --preparation-file '<requirements.json>' \
  --message-file '<commit-message.txt>' \
  --previously-published-base '<retained published base SHA>' \
  --target-ref 'refs/heads/<recorded branch>' --remote '<authorized remote>' \
  --repo '<owner/repo>' --host '<host>' \
  [--session-json '<json>'] [--coordinator '<value>' --observer-directory '<directory>']
```

Use `trunk` or `current-branch` when that is the established mode. The command
does not create a checkout or switch branches. It checks the recorded branch
before preparation. Resolve publication authority and all applicable project
requirements before invocation. A publish request requires the existing managed
publication context before any preparation or commit: retained published base,
authorized target, repository, checkout and branch. Story Branch Mode targets
its execution branch; Trunk Mode targets its authorized trunk. Caller-selected
current-branch and supported host-owned work retain their checkout and target.
A one-shot landing declares its existing `--tracking one-shot` contract; the
retained-result one-shot operation still owns that separate journey.

Without publication authority, use `--authority local-only` and omit publication
inputs. That path never fetches, pushes, starts an observer, or registers a
revision. It reports the local commit pending publication. A current-branch
base is its recorded checkout's HEAD before the operation's first commit; do
not replace an unpublished-base refusal by choosing another base.

The preparation JSON supplies these fields; missing required context refuses
the attempt before project commands or staging:

| Field | Required input |
| --- | --- |
| `kind` | `planned` or `quick`, from the execution source. |
| `commit` | `agent` when [agent commits](agent-commits.md) applies; otherwise `git` for the ordinary Git contract, including caller-selected current-branch and one-shot work. |
| `generation` | Triggered generator operation, or an explicit `notRequired` reason when its trigger does not apply or compatible generation is already accepted. |
| `formatting` | This project's selective formatter operation, or an explicit `notRequired` reason when the project requires no formatting. Do not substitute hook-owned lint. |
| `summary` | Required project-summary mutation operation, or an explicit `notRequired` reason. Quick work must not invent an execution record. |
| `plan` | Required only for planned work: workspace-relative `path` and positive integer `slice`, nothing else. Delivery marks that slice `done` itself. Put the plan's other evidence (accepted proof, learnings, revised remaining slices) in the plan before invocation, as your owned edit staged with it. Quick work omits this field. |
| `progress` | Optional. `true` when planned hosted/external observation still needs the published revision: same path without done; motivating open receiving obligations stay open/owned and do not refuse. Omit for ordinary `done`. Not quick work or Git plumbing. |
| `staging` | `ownedPaths`: exact workspace-relative files whose entire working contents you own and want staged; `stagedPaths`: exact files whose already-staged changes you own and want committed without restaging their unstaged bytes. Supply both arrays, even when one is empty. |

An operation is an object such as
`{"command":["executable","argument"],"timeoutMs":120000}`. Resolve the actual
executable and each argument for the project. `timeoutMs` is an optional positive
bound for that process. An inapplicable operation is
`{"notRequired":"the concrete applicability reason"}`; it cannot also carry a
command. File existence or an unconditional caller success flag cannot stand
in for a required operation's execution.

Each command runs directly with its argument array in the selected checkout,
without an implicit shell. If a compound project script is necessary, its own
terminal result must represent all required work. Do not make an output filter
the final status of a failed prerequisite. The maintained entry builds no such
pipeline and inspects each terminal result before proceeding.

Resolve any ambiguous staged ownership with its owner before invoking the
command. It refuses unrelated staged paths both before preparation and before
its own staging; it never unstages another writer's work. For separable changes
in a shared file, stage only your reviewed changes first and name that file in
`stagedPaths`. Do not name it in `ownedPaths`, which stages its whole working
content. Review the intended staged changes and project commands under the
existing delivery contract; the path lists are ownership declarations, not
semantic review or a sandbox for arbitrary scripts.

The command runs generation and formatting, checks planned obligations before
the plan/status mutation, runs that mutation and the applicable summary, and
checks the changed obligation record again. The mutation changes only the
selected slice's real `Status:` line, leaving an already `done` slice as it is,
so a repaired attempt repeats the same request. It refuses, naming the slice
and changing nothing, when that slice is missing, duplicated or unreadable, or
when another slice is `done` in the working plan but not at the checkout's
`HEAD`. With `progress: true`, skip that mutation and recheck after summary without
treating the unfinished selected slice as committing; failed prerequisites
still publish nothing; after accepted observation omit `progress` so ordinary
fresh marks `done` and still refuses while missing. It then stages owned whole
files, runs real `git diff --cached --check`, and commits through
`agentCommit` or ordinary Git as resolved above.
Project hooks remain enabled. Refused agent identity never falls back to
ordinary Git.

After successful preparation and commit, publish authority passes the actual
`commit.sha` to managed delivery as `validatedCandidate` before that delivery
establishes or reuses observation. The publisher preserves candidate-mismatch,
reconciliation, renewed proof, authorized suffix and host ownership rules in
[trunk publication](trunk-publication.md). A later branch tip cannot replace
the receipt. Supply the same observer owner inputs that reference describes:
Codex uses its already-armed coordinator and exact stream directory; Cursor
and Claude Code use their own host tool identity or explicit session JSON.
Missing observation authority reports the existing coverage gap beside accepted
publication; it does not fabricate coverage or authorize a different observer.

A published success has `publication: accepted`, the original `commit.sha`,
and `receipt.sha`, the exact remote-accepted revision after any proof-governed
rewrite. Its observation is attached, reused, or an explicit coverage gap. A
local-only success has `report: committed-pending-publication`, `commit.sha`,
`publication: pending`, and `observation: null`, with no remote acceptance.

Once a fresh commit succeeds, retain its `commit` and returned `recovery`
(`candidateSha`, `suffixBase`, checkout/remote/target/host context) even if
delivery stops or throws. Each pre-push rewrite updates that pair. A
`delivery-interrupted` result keeps it and reports publication as unknown:
inspect remote acceptance through existing resume. A candidate-mismatch
retains the fresh receipt, never a later branch tip. A proof stop retains the
publisher's rewritten candidate/base; renew proof, then use existing-candidate
managed delivery with `validatedCandidate` (or interrupted-publication). Do
not rerun successful preparation or invent another commit to retry.

A thrown renewed-proof operation returns `validation-failed` with publication
stopped and its retained rewritten candidate/base. Renew proof before
existing-candidate delivery; that failure is not a push-only resume.

A failure exits nonzero and names its `operation` or commit `status`, error,
and available terminal exit/signal details. It withholds dependent work and
leaves preparation edits and staged content for repair. Repair the named
operation, renew only invalidated proof, then retry fresh preparation. Do not
undo owned edits or another writer's bytes merely to clear it. An
already-committed or accepted candidate uses
[interrupted-publication recovery](trunk-publication.md#resume-an-interrupted-publication)
instead of another fresh commit.

For example, a nonzero formatter reports `operation: formatting`, preserves
edits, and withholds commit and delivery. Repair and repeat with still-valid
proof; a slice already marked `done` stays so. On success the commit receipt
is the exact publication candidate registered on the authorized observer. A
refused hook stops the same way; an already-accepted candidate resumes without
another commit or push. A zero-exit output consumer cannot enable withheld
effects.
