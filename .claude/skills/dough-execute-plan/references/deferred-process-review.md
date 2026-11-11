# Deferred process review into wrap-up

When process is enabled under
[Select reviews](../../dough-execution-retrospective/SKILL.md#select-reviews),
obtain automatic-check selection from wrap-up's
[Assess intent coverage](../../dough-story-wrap-up/SKILL.md#assess-intent-coverage)
with no parallel selector. If that owner will request the check, keep process
selected and defer its analysis, suggestions and log access until wrap-up
supplies the report and dispositions — neither `--skip-process` nor an
include-process override. Otherwise run process in the retrospective. Product and
proof complete before continuation; carry selected deferred process as pending
closure state, not a context error blocking wrap-up. Wrap-up
[completes it](../../dough-story-wrap-up/SKILL.md#complete-a-deferred-process-review)
once after report, dispositions and follow-up decision, supplying those with
available run history, elapsed time and setup/waiting problems, writing findings
in the execution checkout before deleting needed sources. Findings enter the
before-cleanup commit. Retain partial deferred review state on interruption;
resume without repeating product or proof review or settled dispositions.
Explicit skips, unresolved selection, missing history, coverage off, ineligible
work and direct wrap-up without a pending deferred review invent no process
review.
