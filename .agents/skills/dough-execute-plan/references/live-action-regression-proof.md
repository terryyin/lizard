# Require current regression proof before a live action

Given an active plan naming a regression prerequisite, when the agent reaches
an authorized live action within the slice — one that changes a live
installation, for example a restart, deployed configuration change, upgrade,
migration, or updater enrollment — confirm an accepted current observation of
that prerequisite applies to the actual candidate and conditions before
performing the action, or leave the action unperformed and report the exact
obligation and gap. Read-only observation is not a live action and is not
gated by this decision.

When no accepted observation exists, run the named regression command and
treat its result under [Diagnose failed proof](execution-decisions.md#diagnose-failed-proof); do
not invent a second proof or failure contract for this decision. A passing
result authorizes the dependent live action; retain the literal command,
candidate identity, and result as the observation that authorized it.

A retained pass authorizes reuse only while it still applies to the actual
candidate and conditions now being acted on. Confirm that correspondence
using [own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof)'s
matching promise, implementation/candidate, setup, relevant conditions, and
observation — not a recent timestamp or an identical whole-repository SHA
alone. A relevant change to the actual candidate's code, tests, dependencies,
configuration, or environment breaks that correspondence and requires
reassessment; an irrelevant change, such as documentation-only, leaves it
intact. When correspondence holds, reuse the retained pass and cite it as the
authorizing observation instead of rerunning the command. A pass observed for
a different candidate or checkout does not qualify the actual one without
this demonstrated correspondence, regardless of an unrelated passing CI
result or an unaffected CI-visible subset.

An independently passing operational or health check — for example a curl
probe or container health check — never substitutes for the missing
regression prerequisite, even while it keeps passing throughout. Operational
checks retain their own observations and timing: do not require a
post-transition observation before the transition it can only observe, and do
not treat it as satisfying this prerequisite.

If the named command fails, is absent, or is unavailable — including when a
retained pass no longer corresponds to the actual candidate — leave the live
action unperformed; do not proceed on an explanation or on unrelated green
CI. Report the exact command/obligation and gap in the existing plan or
conversation, and continue other unrelated authorized work in the same
slice. Once the command is rerun and passes for the actual candidate, perform
the live action. Missing correspondence is diagnosed and resolved through
this same obtain-or-stop path; it does not need a separate decision.

This decision does not itself grant deployment permission and adds no new
post-action observation requirement: existing deployment authority and
post-action observations are unchanged.
