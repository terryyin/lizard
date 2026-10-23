# SEED-005: Observe CI independently of the development shell

<a id="project-ci-adapter"></a>

### Configure CI observation through project settings

**Identity:** SEED-005#project-ci-adapter
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** An Open Dough CI observer started outside Nix observes the repository's
`python-app.yml` workflow through `.planning/open-dough.json`.

**Scope:** Configure the supported project CI command adapter, reuse existing
GitHub acquisition behavior where practical, and remove the Nix-only workflow
selection once project settings own it.

**Expected:** With `DOUGH_CI_WORKFLOW` unset, the observer discovers push attempts
for its requested repository and branch, preserves attempt identities and
outcomes, and can provide failure diagnostics for `python-app.yml`.

**Actual and evidence:** The previous repair sets the selector only in
`shell.nix`; outside Nix the installed GitHub observer still defaults to `ci.yml`.
The user explicitly requests `.planning/open-dough.json` so observation does not
depend on entering the development shell.

**Uncertainty:** Verify and exercise the installed command-adapter protocol and
existing GitHub client before choosing the smallest bridge. The bug-fixing
skill authorizes one planless ten-minute repair with no replanning.
