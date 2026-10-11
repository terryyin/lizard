// What each operation the tool offers is for, and what it deliberately does
// not decide, in the words a caller reads when they ask for help or name an
// operation the tool does not have. Every refusal that hands back the usage
// hands back this one description.

import { directionHeading } from "./product-backlog-direction.mjs";
import { queueHeading, takenHeading } from "./product-backlog-document.mjs";
import { doneCatalogPath } from "./product-backlog-done-catalog.mjs";
import {
  doneRecordDirectory,
  doneRecordWindowDays,
} from "./product-backlog-done-record.mjs";
import { defaultBacklogPath } from "./product-backlog-store.mjs";

export const usage = `Usage: product-backlog.mjs add --identity <id> --title <title> --link <href>
                             (--after <id> | --before <id> | --position first|last)
                             [--file <path>]
       product-backlog.mjs place --identity <id>
                             (--after <id> | --before <id> | --position first|last)
                             [--return] [--file <path>]
       product-backlog.mjs take --identity <id> (--plan <path> | --no-plan)
                             [--file <path>]
       product-backlog.mjs complete --identity <id> [--dropped] [--file <path>]
       product-backlog.mjs catalog-done [--file <path>]
       product-backlog.mjs refresh --identity <id>
                             [--title <title>] [--link <href>] [--plan <path>]
                             [--file <path>]
       product-backlog.mjs direction (--text <text> | --clear)
                             (--expect <text> | --expect-none) [--file <path>]
       product-backlog.mjs adopt --all [--file <path>]
       product-backlog.mjs merge --ancestor <path>
                             --branch <path> --branch <path> [--file <path>]
       product-backlog.mjs record-state --identity <id> --link <href>
                             --refinement not-refined|refined
                             --approach unselected|planned|planless
                             [--plan <path>]
                             [--assessment ready|not-ready
                              --expect-document <sha256>
                              [--expect-plan <sha256>]
                              [--reason <text>...]]
                             [--file <path>]
       product-backlog.mjs read-state --link <href> [--file <path>]
       product-backlog.mjs read-dependencies --link <href> [--file <path>]
       product-backlog.mjs update-dependency --identity <id> --link <href>
                             --dependency-file <json-path>
                             --expect-dependencies <sha256> [--file <path>]

add adds one already identified entry to "## ${queueHeading}" at the requested
relative position. Identities are supplied, never allocated there.

place moves one listed entry to a requested position in "## ${queueHeading}",
using the same relative destinations as add, and carries its line across
unchanged. It applies a priority the caller has decided and ranks nothing
itself. Returning work from "## ${takenHeading}" is stated explicitly with
--return; a destination alone never returns taken work.

take moves one identified entry to the end of "## ${takenHeading}", keeping its
identity and adding the selected plan link; an entry already there is resumed in
place. The plan decision is always stated: --plan names the active plan, and
--no-plan takes a quick story, or a correction whose canonical home is already
its plan. Taking work does not decide or grant execution authority.

complete removes one identified entry from whichever active list holds it,
applying a completion the caller has already decided. It never decides whether
work is complete, and it never deletes a story or plan file: closing those
canonical homes stays with the caller's wrap-up. Removal happens only on this
explicit request naming the identity. It also removes the execution agent
profile under agents/ beside the backlog that names the same identity,
releasing that agent name, and writes the work's done record under ${doneRecordDirectory}/
beside the backlog: its identity, title, completion time, the developer Git is
configured with, and the released profile's agent, host, and model. Completing
the same identity again replaces its record. --dropped removes work that was
dropped rather than finished: the entry and profile go as above and the done
record is left out. Either way it removes done records completed
more than ${doneRecordWindowDays} days before, then rebuilds ${doneCatalogPath} from the done
records that remain. Include those files in the same commit as the backlog
change. A preparation profile is left for its own release.

catalog-done rebuilds ${doneCatalogPath} beside the backlog from the done records
already there: each readable record's file name, identity, completion time, and
Git blob hash, newest first, and each unreadable record file by name and hash
alone. It changes no record and no backlog entry, prunes nothing, and writes no
completion. complete and the Git-aware merge, rebase, and cherry-pick adapters
keep the catalog current. Run catalog-done to publish a catalog for records
written before catalogs existed, and after records arrive or change outside
them, such as by raw Git, a hand edit, or an older installed copy of these
scripts; the catalog is derived from the records and never merged by hand.

refresh updates what one listed entry says about itself — its title, the
canonical document it links, or the active plan it links — after that document
has already been renamed or moved. The entry keeps its identity, its list, and
its position; the moved document's own recorded identity is what establishes
that it is the same work. It renames nothing, moves no file, and repairs no
link anywhere else.

direction records the near-future direction the caller has already chosen,
exactly as supplied: it writes "## ${directionHeading}" when the backlog carries
none, replaces what that section says, or clears it away with --clear. It never
writes, summarises, or reflows strategy text of its own, and it changes no entry
in either list. The direction the request was written against is always stated:
--expect names the text you read, and --expect-none says you read none. A
request whose expectation no longer holds is refused with nothing written, so a
direction someone else has since changed is never overwritten unknowingly.

adopt records one identity for every active entry in the canonical homes its
links name, reusing the ID each home already carries. It changes no membership,
order, or direction, and never runs implicitly: --all is required.

merge reconciles three supplied versions of one backlog — the ancestor both
branches started from, and each branch's version of it — and writes the whole
result to --file, which is read only as the destination. A value a branch left
as the ancestor wrote it accepts the other branch's change, a change both made
the same way is applied once, and two different changes to one meaning are
reported with nothing written, for a human to decide. It prefers neither
branch, unions no lines, and is not Git-aware: establishing which files hold
the three versions stays with the caller.

record-state writes one versioned story-state block into the canonical home
--link names: refinement, approach, and optional readiness assessment. A
planned approach stores --plan relative to that home file and requires that
plan to exist; planless needs no plan file. An assessment takes
--assessment ready|not-ready, the caller's --expect-document (and
--expect-plan for a distinct planned file), and for not-ready at least one
--reason. Ready requires refined plus planned or planless and no reasons.
The recorder rereads current content digests, refuses a stale expected basis
without edits, and never grants execution authority. It replaces only the
selected story's block and serializes cooperating writers per canonical file.
A planned approach for work already in "## ${takenHeading}" also links that
entry to its plan, keeping its place; an entry linking another plan refuses
with nothing written. Other entries and other approaches leave the backlog
unchanged.

read-state prints the shared reader's normalized preparation facts,
assessment view, and current content basis for the home --link names as
JSON. The basis covers the story's own section, the seed's shared context
outside other stories' sections, and a distinct plan. Legacy absence is
"not-recorded"; an unsupported schema version is "unsupported-version"; a
stored assessment retains ready/not-ready and its reasons. changedSinceReview
is true when its reviewed basis no longer matches; it informs, never writes,
and does not independently block authorized startup.

read-dependencies prints the consumer's separate, versioned dependency record,
blocking entries, and dependency agreement basis. Absence means no blockers;
a malformed present record is refused. Dependencies stay in the review basis.

update-dependency upserts one supplier agreement from --dependency-file, using
the basis returned by read-dependencies. The JSON object names supplier
{identity, href}, implementation, rationale explaining why normal shared design
and reconciliation are insufficient, condition, and state (waiting, satisfied,
or decision-needed). Satisfied requires resolution {revision, path, summary};
decision-needed requires decision text. Both canonical identities must resolve.
A stale basis or ambiguous endpoint writes nothing. Other dependencies, sibling
stories, and preparation judgments remain intact. This applies an explicitly
decided necessary prerequisite; it never infers one from shared code or order.

discover-consumers --supplier-identity <identity> reads current canonical homes
and reports reverse agreements plus discovery problems without writing.
resolve-dependency records an evidenced consumer judgment with --dependency-file
and --expect-dependencies, including waiting or decision-needed after cleanup;
--accepted-revision <sha> --remote <remote> --target <branch> establish accepted
supplier integration. Recoverable supplier outcome must show all planned slices
done (optional --plan), or --planless-complete --completion-file <proof path>.
The consumer agreement must remain unchanged. Repeats preserve existing evidence.
Condition satisfaction is the caller's evidenced judgment, never text matching.

Paths are resolved against the current directory; --file defaults to
${defaultBacklogPath}. Canonical home links and planned paths are resolved
from the backlog file's directory and from the home file, respectively.`;
