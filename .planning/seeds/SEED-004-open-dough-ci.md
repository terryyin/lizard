# SEED-004: Observe CI for Open Dough execution branches

<a id="open-dough-ci"></a>

### Open Dough CI

**Identity:** SEED-004#open-dough-ci
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Future Open Dough executions observe this repository's CI without a
per-run environment override, and pushes to supported story branches run CI.

**Scope:** Persist the `python-app.yml` workflow selector in the project's Nix
development environment. Verify story branch push coverage; the existing
workflow's `**` branch pattern already covers all story branch prefixes.

**Expected:** A fresh observer launched through the required Nix wrapper selects
`python-app.yml` from project settings;
pushes to `claude/*` and `codex/*` story branches receive the same checks as
existing supported branches.

**Reported actual:** The observer defaults to `ci.yml`, while this repository
uses `python-app.yml`. CI did not run for two `claude/*` story branch pushes.
`DOUGH_CI_WORKFLOW=python-app.yml` enabled observation on `master` for one run.

**Evidence and uncertainty:** The report supplies the workflow mismatch and
missing push coverage. Verify the installed adapter protocol and current branch
filters before the bounded repair. Use the bug-fixing skill's planless repair
authority, a ten-minute implementation limit, and no replanning.
