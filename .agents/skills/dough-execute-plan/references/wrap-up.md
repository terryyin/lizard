# Slice wrap-up

The coordinator runs this sequence after implementation, including owned CI repair, using [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair) for validated work.

## Accept proof

For planned Story Branch work, first [settle freshness](deliver-the-change.md#settle-freshness-before-dependent-work).
Apply [proof ownership](../../dough-story-refinement/references/planning.md#own-executable-proof).
Read the complete selected story, bounded correction or quick instruction,
including developer amendments.

Use the short verdict to locate the full report and [run records](proof-evidence.md#read-the-run-record).
A promise absent from the verdict, or a referenced report or run record missing
from disk, is incomplete proof: return the slice naming the missing promise,
report or record. Read from the report and logs only the parts the comparison
below needs, under [targeted retrieval](disposable-research.md).

Compare the candidate with the outcome the source promises under the stated
conditions. Inspect proof that would distinguish that outcome on this candidate.
If the plan or return omitted a required outcome, return the gap to the slice
that owes it. Preserve valid later ownership and approved scope changes. Resolve
disputed scope through [human judgment](execution-decisions.md#stop-for-human-judgment).

Accept once the applicable promises, affected consumers and required gates have
sufficient observations and the story obligations are settled for this slice.
Further inspection needs a specific mismatch or evidence gap. For
developer-operated proof, follow [developer-operated observation](../../dough-story-refinement/references/executable-proof.md#plan-and-reuse-developer-operated-observation).

If a delegated call was interrupted, or its return arrived only after interruption, follow
[interrupted-delegation recovery](delegation.md#recover-an-interrupted-delegated-result) before accepting anything from it.
Use the return as a location index, never the promise list or accepted evidence. Inspect product, setup and observing
assertions/signals at each applicable boundary. Setup supplies preconditions; product and observation establish
the outcome. Passing commands, test names, `proof:` summaries, coverage claims or setup supplying the outcome
do not prove it. Untested or out-of-scope claims remain incomplete; they neither verify nor invent a source promise.

For filtered proof, compare actually selected tests/observations with the claimed promises.
A zero-exit command that selected nothing, or that selected fewer observations
than the claimed promises require, leaves those promises incomplete — obtain
the missing observations (broader or corrected selection, retargeted titles, or
another sufficient check) before accepting them, or return them as uncovered.
Reuse a trustworthy recorded selection and result when they still match the
claimed command, filter, and candidate; do not rerun solely for process.

When the change alters an operation or contract, a message or value, a
default, rendered output, or shared text, return a slice without its
[consumer-trace record](../../dough-story-refinement/references/executable-proof.md#record-the-consumer-trace)
for the trace. When the change has consumers, as
[own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof)
defines them, compare the record's terms with the diff's removals, renames, or
relocations of symbols or modules, changes to callable contracts, and the changed
behavior, including retired behavior, altered preconditions, and a too-broad
derived term naming the changed contract that was not resupplied. Do not accept
a record with a listed consumer lacking a disposition, or with an exclusion the
effect reaches. Inspect discovered consumers, including
unchanged tests and fixtures, then follow the changed effect through the reported
surface or entry points and compare the reached consumers with the return's
selection and proof. Apply the common record rule to each affected purpose's
boundary, proof type and actual observation or gap; a `run` label alone does not
settle it. Name the consumer and missing proof type when obtaining or returning
an observation before dependent acceptance.
A return that stops at file, selector, or helper matches leaves that trace
incomplete: inspect the enclosing observations or caller scenarios and their
setup before accepting it. Confirm sufficient current observations for the
affected purposes, including newly reached consumers. Do not accept on a prior
unaffected-suite or unused-consumer exclusion when the change reaches that
boundary. Do not accept or publish until proof for the record's unexcluded
consumers and any other known affected consumer ran or was reused, including
one left unrun for CI: obtain the missing observation or
return the slice for it. Preserve sufficient reused or equivalent-purpose proof
and accepted evidence for unrelated consumers and unchanged boundaries. Require
the suites that
[executable proof](../../dough-story-refinement/references/executable-proof.md)
selects, not every suite. A discovered failure keeps the
[failure policy](failure-policy.md) and the consumer's legitimate promise;
passing by supplying the outcome in setup or weakening the assertion is not
sufficient proof.

For a required behavior or observation missing from the product or return, obtain
it within authority or return it for correction before dependent acceptance. If
proof is unavailable, name the promise incomplete and stop only its dependent
delivery; preserve independently valid accepted evidence. For planned execution,
record every reported or independently discovered gap, loss, limitation or interim
behavior as a structured [story obligation](story-obligations.md), with discovery
provenance in the existing reported text or title, the story clause and exactly
one disposition before declaring dependent acceptance; a short outcome file does
not replace these obligations. The script checks structure and quotes, not judgment. Apply
[the relevant-path check](story-obligations.md#check-the-paths-before-closing-a-gap)
before closing a gap as `no user cost` or `proved`. Re-read every open
interim the current slice depends on; a newly wrong result becomes a return
owned by this slice, preserving the original text and provenance. For quick
execution, apply that reference's same-slice rule without creating a plan.

Read fixture or setup changes that turn a failing scenario green against the
story's examples. Check a reshaped example in its real shape, file or input;
green on a different fixture does not accept it. A neighbouring case's guidance
does not authorize a different promised consequence.

Retain the promise, accepted boundary, consumer trace, inspected setup/assertion
locations, results and literal commands for refactoring. When a named mismatch
or evidence gap remains, inspect the smallest relevant callers or
implementation for that gap. Return incomplete or contradictory evidence with the
promise, inspected locations and gap; refactoring cannot supply missing behavior.

Judge substance under [the return contract](return-report.md).
Equivalent layouts carrying literal commands, results, owned changes, promise
coverage, boundaries, setup and observation locations need no report-only resend
or formatting retry. Missing terminal results or contradictions remain incomplete.
Explicit completion markers such as `## REFACTOR COMPLETE` stay verbatim.

Compare each run record's `revision` and `uncommittedChanges` with the candidate
checkout. Proof from before a later checkout edit covers the earlier state;
obtain only the observation that edit affects, preserving unaffected accepted
evidence.

Reuse accepted inspection only while its promise, boundary, implementation, setup
and observations match. Recover literal commands from the original handoff; rerun
only missing, ambiguous, changed-boundary or omitted required integration proof.
Preserve failures under [execution decisions](execution-decisions.md#diagnose-failed-proof).
For expensive proof, inspect the handoff's retained [pre-launch decision](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch).
Reconsider changed host/workload conditions before launch; deferred proof stays incomplete with its owner and next condition.

Require CI-safe work: no deliberate failing tests or unfinished end-to-end proof
represented as complete. Mark unfinished proof by this project's convention;
run full CI before commit only when explicitly required.

## Deliver the change

Follow [Deliver the change](deliver-the-change.md#deliver-the-change) in the
selected [execution location](execution-location.md). After the commit, publish
through [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).
