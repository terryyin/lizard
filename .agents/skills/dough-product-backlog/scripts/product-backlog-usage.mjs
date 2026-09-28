// What each operation the tool offers is for, and what it deliberately does
// not decide, in the words a caller reads when they ask for help or name an
// operation the tool does not have. Every refusal that hands back the usage
// hands back this one description.

import { directionHeading } from "./product-backlog-direction.mjs";
import { queueHeading, takenHeading } from "./product-backlog-document.mjs";
import { defaultBacklogPath } from "./product-backlog-store.mjs";

export const usage = `Usage: product-backlog.mjs add --identity <id> --title <title> --link <href>
                             (--after <id> | --before <id> | --position first|last)
                             [--file <path>]
       product-backlog.mjs place --identity <id>
                             (--after <id> | --before <id> | --position first|last)
                             [--return] [--file <path>]
       product-backlog.mjs take --identity <id> (--plan <path> | --no-plan)
                             [--file <path>]
       product-backlog.mjs complete --identity <id> [--file <path>]
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
releasing that agent name; include that removal in the same commit as the
backlog change. A preparation profile is left for its own release.

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
stored assessment whose basis no longer matches is "needs-reassessment". It
never writes.

Paths are resolved against the current directory; --file defaults to
${defaultBacklogPath}. Canonical home links and planned paths are resolved
from the backlog file's directory and from the home file, respectively.`;
