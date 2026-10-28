---
name: lizard-process-pr
description: >-
  Process a Lizard pull request from product-backlog intake through evidence-based
  review, correction, performance qualification and an authorized disposition.
  Use for evaluating, accepting, correcting or processing incoming PRs, including
  batches. Preserve the requested review-only or delivery scope.
---

# Process a pull request

Make the PR backlog-owned first, then complete the requested scope with reusable,
revision-bound evidence. This is project guidance for Codex, Cursor and Claude Code.
It composes the installed Open Dough lifecycle rather than replacing it.

## 1. Put every PR in the product backlog first

Before substantive review, profiling, correction or merge:

1. Resolve the repository and PR URL/number; read `AGENTS.md`, `.agents/agent-map.md`
   and `.planning/PRODUCT-BACKLOG.md`. Minimal metadata lookup is intake work.
2. Search active stories and done records for that PR. Reuse its canonical owner
   when the requested work is already covered, including a Taken batch that names
   each PR. Do not create another item for each retry or review phase. A completed
   owner is history; genuinely new work needs a new linked identity.
3. For an uncovered PR, use [dough-product-backlog](../dough-product-backlog/SKILL.md):
   run its identity-evidence operation before allocating, write a canonical story
   with beneficiary, source PR, evaluable outcome, scope and key examples, then
   `add` it through the installed backlog script. Keep direction and unrelated order.
   The outcome may be a review decision; it need not promise to merge.
4. Confirm that the entry links to the canonical owner and that the owner explicitly
   names this PR. Only then continue. Missing backlog access blocks deeper processing.

Intake does not Take work or authorize external actions. Use the existing refinement,
planning and execution skills for the selected work. A review-only instruction
authorizes the review; record backlog ownership without turning it into correction
or delivery. Preserve existing authorization across resumes.

For an explicitly offline review that excludes commits/publication, keep the owner
queued, record the review decision in its canonical home and perform the authorized
analysis directly. Skip published execution admission, delivery and wrap-up. Report
the review result separately from pending backlog closure; the restriction does
not prevent the required local intake.

## 2. Load a compact evidence packet

Read the complete diff, discussion, inline threads, reviews, checks and dependency
claims once, in bounded sections. Keep raw data on disk and return paths, counts,
revisions and actionable details to the coordinator. Avoid printing tool catalogs,
whole transcripts or nested check-run metadata. Fetch individual patches as needed.

Prefer connected GitHub tools when available. On CLI hosts, the read-only helper
collects paginated REST evidence and review-thread states with `gh`:

```sh
nix develop -c python .agents/skills/lizard-process-pr/scripts/pr_packet.py capture \
  --repo terryyin/lizard --pr 123 --out /absolute/evidence-directory
```

Use `inspect <packet-directory>` to verify and summarize a retained packet. `pack
--repo owner/name --input <json> --out <directory>` accepts an existing export;
see `--help` for its fields. The packet is a read snapshot, never merge clearance.
Refresh remote state/checks immediately before a dependent action. Head/base changes
invalidate affected conclusions even if a cached packet is intact.

Retain: owner identity, requested scope and existing publication authority; exact
head/base and frozen starting baseline; expected/actual behavior; shared consumers;
correction route and estimate; proof paths and applicability; next action/disposition.
Recover this small context on resume before searching raw history again.

## 3. Decide what the contribution needs

Reproduce on the current target through `analyze_file` or the CLI. Distinguish a real
defect from an obsolete example, already-fixed behavior, invalid fixture or a change
whose value does not justify its cost. A green contributor suite is evidence of its
assertions only. Check the reported example and a few discriminating nearby controls
against both target and proposal, including subsequent function boundaries/metrics.

Inspect the whole affected concept and its consumers, including other languages,
extensions, public names/counts, CLI output and the website when reached. Apply
[lizard-language-support](../lizard-language-support/SKILL.md) for reader work,
[dough-pfe](../dough-pfe/SKILL.md) for reuse decisions and
[dough-adr-awareness](../dough-adr-awareness/SKILL.md) for architectural constraints.
Prefer the existing semantic owner over parallel counters, per-token wrappers,
whole-stream buffering or PR-specific switches.

Before corrections, establish whether they can reach the contributor branch while
preserving history. Fork `push: false` alone does not disprove maintainer access;
combine `maintainer_can_modify` with a divergent, non-mutating dry-run when needed.
An up-to-date/no-op dry-run proves no write path. Do not force-push.

Estimate the complete correction and cleanup before editing. Preserve a current
story's time policy: the Fabio cohort defers correction above 15 minutes (review and
measurement excluded). That threshold is historical scope, not a universal timeout.
When work exceeds the selected bound, create/reuse a canonical correction with the
reproducer, expected result, remaining work and PR link; leave inadequate code
unmerged. Do not split a batch into review-only and deferred QA phases. Each owner
must reach a complete disposition for its named PRs.

## 4. Correct and qualify only the authorized scope

Use the selected execution workflow for corrections and independent post-change
refactoring. Review the aggregate contributor change plus corrections, including
untouched representations of the same concept. Preserve discriminating red/green
proof, valid controls and relevant shared consumers. Do not weaken an expectation
to disguise an implementation failure.

For runtime/per-token changes, read [performance](references/performance.md).
Freeze workloads before timing; measure affected API/consumers and broad CLI,
incremental and cumulative cost where applicable. Correctness comes before timing.
Keep heavy work serial. Nix shell entry mutates `.venv`: serialize **all** Nix
entries in one checkout, including reads and observer actions. Batch reads inside
one invocation; `PIP_QUIET=3` reduces noise without changing the required Nix wrapper.

Before accepting a corrected candidate, run full `nix develop -c python -m pytest`
and `nix develop -c make`, plus applicable hooks and current hosted CI. Discover
workflow/matrix/conditional-job applicability at that boundary. Reuse proof only
when its promise, production bytes, inputs, interpreter/options and consuming
boundary still match. Evidence-only edits do not automatically require remeasurement.

## 5. Finish with an evidenced disposition

For authorized delivery, publish corrections without losing contributor history;
use the installed backlog Git adapters when combining backlog/done records. Verify
the final contributor head, base and tree against accepted proof, refresh required
checks and guard the hosted merge with the exact head SHA. Stop if the head moves.
Local green checks never replace required hosted results. Do not merge while an
affected workload, refactor or required check remains unresolved.

Post comments/thanks only when authorized. After a confirmed merge, acknowledge
material corrections and retain one comment URL; check for an existing acknowledgement
before retrying. Close the owner through the normal wrap-up only when its scope is
complete. Keep evidence still consumed by another active story.

Report one of: review completed within requested scope; merged (with acknowledgement
state); unnecessary/already fixed with evidence; real defect deferred to a linked
correction; or blocked/pending with the precise unmet gate. A review, deferral or
closed PR is never labelled merged. A batch's final report accounts for every frozen
member; later arrivals require their own intake.

The [experience summary](references/experience.md) explains the historical basis.
Read it when changing this skill or diagnosing a recurrence, not on every PR.
