# Resolve consumers of a completed supplier

Use this shared procedure during a selected supplier's landing or wrap-up.
A generic checkout landing, an increment, a queue `complete`, disappearance from
Taken, or missing history does not establish completion. Without a selected
completed supplier, continue the ordinary caller without satisfying dependencies.
Refinement and planning remain possible while consumer execution is blocked.

## Preserve context before cleanup

Require the selected stable identity, canonical home, goal/outcome and proof,
its plan and status vocabulary when planned, a recoverable before-cleanup Git
revision, and authorized remote/integration target. All planned slices must be
done; planless work needs an evidenced outcome judgment from its instruction,
changes and results, including an evidenced no-change result. Never invent a
completion record because a supplier home is missing. Git history recovers
selected supplier evidence; it never invents live consumers.

Before deleting source or plan, commit the completed outcome and proof under the
calling workflow and retain its full revision and repository-relative paths.
Preserve that revision in accepted integration history. Recover with
`git show <revision>:<path>`; keep its anchor when the home is a story section.
The accepted integration SHA may differ from this revision and may no longer
contain those files. Preserve both facts honestly. If the evidence cannot be
preserved or recovered, leave the affected context intact and report the gap.

## Discover and judge each current consumer

Run from the established project using its installed backlog command and path:

```sh
node <installed>/dough-product-backlog/scripts/product-backlog.mjs discover-consumers --file <backlog> --supplier-identity <supplier identity>
```

Discovery reads current canonical homes, including queued, Taken, prepared
unqueued stories, sibling sections and canonical corrections. Its `consumers`
carry each current agreement and basis; `problems` identifies unreadable or
ambiguous homes. Inspect those problems and retain affected work; never claim
all consumers were handled when discovery is incomplete. No reverse store or
supplier card annotation is maintained.

For each consumer, read its canonical goal, scope, examples, recorded condition,
preparation and current plan. If required context is missing, retain the block
and report what must be recovered. Keep the observed dependency basis and the
story/plan content used for judgment. Condition satisfaction is agent judgment
with evidence, not text matching. Handle independently justified consumers
separately; one unresolved choice does not prevent another resolution.

Choose and verify the per-consumer outcome:

- **Directly satisfied:** the supplier outcome proves the condition without
  changing consumer assumptions. Cite the specific behavior and proof.
- **Bounded reconciliation:** the consumer's existing goal, scope and examples
  already determine the adjustment. Edit only affected story/plan assumptions;
  preserve their goals, slice status and preparation judgments. Verify the
  adjusted assumption against the delivered contract and existing intent,
  and record the changed paths, evidence and verification result. A moderate
  difference is one whose answer is settled by that intent, not a numeric score.
  Satisfy only when this verification proves the whole recorded condition.
- **Developer decision:** an unspecified behavior or architectural choice has
  multiple viable answers. Do not choose one. Keep `decision-needed` and put the
  actual question in `decision`; retain the competing behaviors, relevant goal/
  plan paths, supplier evidence and next developer action in the evidence summary.
  Cite any Accepted ADR conflict and stop that conflicting path for a human.
- **Still waiting:** failed verification, unavailable context, or a condition
  requiring consumer implementation stays blocked. Record the remaining gap
  and next action in the evidence summary. Implementation of an unstarted
  consumer requires its own execution authorization; request that separate
  authorization as the next action and retain the block during this visit.
  Assumption edits alone never prove implemented behavior.

For example, a completed endpoint returns retry intervals in seconds and rejects
legacy requests. A consumer already promising seconds can reconcile a stale
milliseconds plan assumption and verify the documented contract. A consumer
whose compatibility promise is unspecified needs “Which endpoint compatibility
behavior does the consumer promise?” A condition requiring the consumer's retry
UI to be implemented still waits for that consumer's authorized execution.

Before writing, reread the consumer's goal, scope and plan and compare with the
context used above. Changed intent or failed verification requires a new
judgment; do not apply an old satisfaction. The command separately refuses a
stale dependency basis or changed agreement without writing. Do not repeatedly
retry that refusal with a refreshed basis and the same unreviewed judgment.

<a id="apply-and-publish-a-direct-resolution"></a>

## Record and publish the evidenced outcome

After accepted delivery of the completed supplier on the authorized integration
target, copy the exact observed agreement into an input JSON object. Set
`state` to the judged `satisfied`, `decision-needed` or `waiting` outcome; remove
an obsolete `decision` or include the actual unresolved question. Add `resolution`:
`revision` is the full recoverable before-cleanup supplier SHA; `path` is its
repository-relative canonical home including the selected anchor; `summary`
explains the verification and fulfillment, or why the condition remains unresolved
and its next action. Evidence on a waiting/decision record never means satisfied.

```sh
node <installed>/dough-product-backlog/scripts/product-backlog.mjs resolve-dependency --file <backlog> --identity <consumer identity> --link <consumer home> --dependency-file <json path> --expect-dependencies <observed basis> --remote <authorized remote> --target <integration branch> --accepted-revision <accepted supplier SHA>
```

The command fetches the target, checks both revisions' ancestry, reads the
supplier's historical canonical identity and planned completion, and rejects a
stale agreement before writing. Supply `--plan <backlog-relative plan>` when the
supplier's established plan is not recorded in its preparation. A plan-homed
correction can use that home as its plan. For evidenced planless completion,
supply `--planless-complete --completion-file <repository-relative proof path>`;
the proof must be readable at the retained revision. These arguments carry the
agent's outcome judgment; a readable file alone does not prove the promise.
The same guarded command records unresolved outcomes after supplier cleanup,
using historical identity and accepted evidence instead of requiring a live
supplier home. It cannot invent a new prerequisite: the current consumer must
already hold the unchanged agreement. Ordinary `update-dependency` retains its
live endpoint checks for authoring new relationships.

Each consumer retains the readable evidence locator, exact condition summary,
and separate accepted integration SHA/remote/target. Other suppliers and
preparation judgments are preserved. Edits produce the existing changed-review
indication; never rerecord Ready automatically or erase unrelated Not ready
reasons. A stale refusal requires rereading and rejudging. A repeat visit
preserves the first satisfied record and its evidence; reuse unchanged unresolved
questions/evidence rather than rewriting them as fulfilled or duplicating receipts.

Publish owned consumer changes using the caller's authorized publication
workflow after accepted supplier integration. Until that publication, published
consumers remain blocked. Wrap-up must finish this visit and publication, or
explicitly retain unresolved dependency work with its consumer agreements,
evidence and next action in the existing active context before retiring resources
or deleting needed material. Do not create a reverse registry, a substitute
completion artifact, or claim unresolved relationships were satisfied. Report
per-consumer outcomes, discovery gaps and retained context separately from
supplier publication and cleanup.
