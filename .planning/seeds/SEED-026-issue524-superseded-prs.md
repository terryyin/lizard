# Review open PRs after issue #524 resolution

<a id="close-superseded-issue524-prs"></a>

### Close superseded issue #524 pull requests

**Identity:** SEED-026#close-superseded-issue524-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"unselected"}
```

**Goal:** Contributors and maintainers receive an accurate disposition for open
PRs related to the merged #524 fixes, with fully superseded PRs closed and
thanked using specific covering fix links.

**Source:** The human requests: "Please check for any remaining open PRs related
to #524 and close those superseded by the merged fixes, with a thank-you
message explaining which fixes cover them."

**Scope:** Screen the frozen 15 open PRs: #306, #349, #359, #369, #390, #396,
#401, #440, #446, #448, #490, #506, #515, #519 and #530. Substantively inspect
related candidates, their complete changes and discussion against current
master and the merged fixes. Close only when the contribution's whole scope
is covered. Preserve unrelated and partially covered work. No product code
correction, feature development or new merge is requested.

**Known expectations:** #524 is closed/completed after #525 and #548 and the
related arrow, line, ternary, parameter and template fixes. #519 proposes seven
TypeScript/TSX boundary corrections, broader than the original #524 crash;
the full proposal and discriminating cases determine its disposition. #390's
title/body do not identify its area, so check its changed-file metadata.
Refresh head/state before an external action. Check existing acknowledgements
and retain one comment URL per closed PR. Account for every frozen member.

**Authority:** Review, reproduce, publish evidence and ordinary lifecycle
records, and close fully superseded PRs with a thank-you explanation. Leave any
distinct unresolved behavior open and report it. No additional implementation
authority is inferred from this maintenance request.
