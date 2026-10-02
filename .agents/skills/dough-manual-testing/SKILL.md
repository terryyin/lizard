---
name: dough-manual-testing
description: >-
  Plans bounded manual or exploratory observation of requested product
  behavior when the developer explicitly requests manual testing, exploratory
  testing, or an active plan slice requires it. Covers web, CLI, API, desktop,
  or combined surfaces. Does not run proactively or replace required automated
  tests.
---

# Observe requested behavior

Use only on explicit developer request or plan-slice manual testing.
No diagnosis, product repair, or unrequested tooling.

## Mission

Resolve scope and time budget from the request: a feature, story set, recent
deliveries, or change range. Recover current promises, examples, and
constraints from this project's records or Git history, including deleted
stories. Apply later decisions before older expectations; implementation,
narration, and existing tests are not the oracle.

Identify in-scope externally observable surfaces, including non-web. Resolve
URLs, accounts, startup, secrets, and tools from this project's guidance when
needed; do not guess credentials.

If oracle, budget, or needed environment or access is missing, name it and
stop; do not invent them.

A standalone session the developer accepts is a mission:
[admit it](../dough-execute-plan/references/admit-accepted-work.md) before
setup or exploration, approach `unselected`, unless explicitly selected as
[one-shot work](../dough-execute-plan/references/one-shot.md). Observation
required by an active plan slice or story continues under that story.
Admission changes none of the testing-only limits here.

## Plan

Before acting, list coverage areas and journeys, risks or questions, and
proportional split of preparation, breadth, selective depth, and
surprise/confirmation reserve. Weight by importance and risk. Include each
promised surface. Completing this plan is not acceptance.

## Prepare

For a standalone session that needs a project checkout, first read and follow
the shared [exploration workspace lifecycle](references/exploration-workspace.md).
Enter that lifecycle before checkout-bound setup and use its selected checkout
throughout the session.

Choose the cheapest reliable route: existing setup, whole or partial automated
journey, or temporary harness or test, including a setup-only feature
scenario using existing steps. Skip setup when current state serves. Confirm
state, session, and required services remain available for external
observation; a finished batch run may not. Preserve isolation, cleanup,
compatible-state reuse, and removal of owned temporary artifacts. Missing
reuse is a possible improvement, not authority for permanent test/runner
changes. If no supported route leaves a usable starting state, name it and
stop.

## Explore

Cover planned areas in breadth first with this project's tools. Spend depth on
surprises and high-risk questions; reallocate remaining time, keeping
confirmation reserve. Reuse sufficient automated evidence; do not replay
deterministic checks or proven setup. If a needed tool or environment is
unavailable, name it and stop.

## Report

When planned coverage completes with no actionable findings or material
uncertainty, report exactly `Good.` Otherwise report only discrepancies
(expected versus actual plus evidence), unresolved expectations (not false
fails), improvements (out-of-scope ideas, not failed acceptance), and
material coverage gaps. Never report `Good.` when blocked or incomplete.
Omit narration, speculation, and completion markers. Do not start repair,
root-cause, or permanent test changes.

On normal completion, close or retain the workspace through the shared
[exploration workspace lifecycle](references/exploration-workspace.md#close-or-retain-it).
It owns which workspace is removed, safe cleanup, and exact retention when
cleanup is unsafe. An admitted session, including one reporting `Good.`, then
follows
[finish the mission](../dough-execute-plan/references/admit-accepted-work.md#finish-the-mission).
