# CI runtime setup

Read this before launching observation. Use Node 20+ (standard library only)
and Git. The GitHub Actions default also requires authenticated `gh` with read
access to the selected repository. A configured project command instead requires
that command and its endpoint credentials to work from the execution checkout;
the observer does not acquire or manage credentials. Detached host adapters also
require a POSIX host with `ps` and signals. Apply this project's tooling wrapper
to launches if needed; hook commands must find Node directly without entering a
build environment.

For checkout-bound work, resolve the installed skill directory inside the
selected execution checkout. Prefer the current host's alias
(`.agents/skills/dough-execute-plan` for Codex/Cursor or
`.claude/skills/dough-execute-plan` for Claude Code). When that alias is
absent, use any other usable same-checkout installation rather than copying
another checkout or declaring coverage unavailable solely from the missing
path. Do not reuse the installed directory that supplied the initially loaded
skill when it belongs to another checkout. The runtime derives checkout
identity four levels above its `scripts/` module; preserve that layout.

Managed delivery resolves that runtime itself for ordinary increments and
repairs. Explicit observation callers below still canonicalize the selected
execution checkout and the checkout identity implied by the resolved runtime
path before launch. Require them to be equal and require the selected runtime
entry point to exist. A missing runtime or a path that identifies another
checkout stops observation setup before an observer is armed. Report the
selected checkout and candidate runtime path or identity for recovery; do not
search other checkouts or the environment for a replacement.
Use the selected execution checkout as the launch working directory. Quote
paths containing spaces and replace example placeholders before execution.

## Select CI

Resolve repository and branch from the authorized push destination. The
`BRANCH` launch argument is that target branch; do not infer it from the
execution checkout's current branch. Read
`.planning/open-dough.json` to select the source before checking provider
requirements. The observer reads that configuration once when it starts; do not
change it during the observer's lifetime.

- An absent `ciAdapter`, an absent file, or `ciAdapter: []` selects GitHub
  Actions. Verify the branch has a push-triggered workflow and verify the
  workflow selector below. This mode requires `gh`.
- A nonempty `ciAdapter` argument array selects the project's command. Verify
  the command can run directly from the execution checkout and that its selected
  check observes pushes to the resolved repository and branch. Do not require
  `gh` or GitHub workflow identity for this mode. Adapter authentication and
  endpoint access remain project-owned.

This observer handles one selected CI check per execution. It does not support
pull-request-only CI, aggregate multiple checks, or observe deployment. If this
project needs an unsupported mode, report missing monitoring coverage and
continue execution without claiming CI observation.

Set the applicable environment variables on the observer launch (and preserve
them in any project tooling wrapper):

| Variable | Meaning |
| --- | --- |
| `DOUGH_CI_WORKFLOW` | GitHub default only: workflow filename or ID accepted by `gh run list --workflow`; it alone selects the observed workflow, whatever its display name; default `ci.yml` must be verified |
| `DOUGH_CI_WORKFLOW_NAME` | GitHub default only, optional, no default: the selected workflow's exact display name, as a consistency check; a selected run reporting another name ends observation with `CI_MONITOR_UNAVAILABLE` |
| `DOUGH_CI_MAILBOX_ROOT` | Optional private shared mailbox directory; default `/tmp/dough-ci-$UID` |

The branch is a required positional argument for the authorized target, never
an inferred `main` or the execution branch name.
Verify the GitHub selector with
`gh run list --repo OWNER/REPO --workflow SELECTOR --branch BRANCH --event push --limit 1 --json workflowName`:
an unknown selector fails, and a returned run shows the display name to use if
you set `DOUGH_CI_WORKFLOW_NAME`. Record the selected source and, for GitHub,
the verified workflow selector (and display name, when set) with the
observer identity in the active plan for planned execution or in the
conversation for quick execution.
Do not create a plan or separate record for that quick context. A new
coordinator must recover and close the old observer before replacing it.
When overriding the mailbox directory, provide the same absolute value to both
launcher and hooks; a per-process `TMPDIR` does not establish shared identity.

Example after resolving the values and applying this project's tooling wrapper if needed:

```sh
DOUGH_CI_WORKFLOW=checks.yml \
  node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' \
  start --execution OWNER/REPO BRANCH
```

For a configured command, omit the GitHub workflow variables:

```sh
node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs' \
  start --execution OWNER/REPO BRANCH
```

The optional final budget argument is milliseconds; default is eight hours.
Requests time out after 20 seconds, and three consecutive observation errors
end coverage. Registering a pushed revision makes the observer check at once;
otherwise polls are 30 seconds apart. Discovery is bounded to 100 startup
runs and 20 per later poll, retaining already seen unfinished runs. Report this
limit when a busy repository requires coverage beyond those bounds.

## Verify host-bridge readiness

Ordinary install and update register the managed Cursor and Claude Code hook
entries from [cursor-hooks.json](../assets/cursor-hooks.json) and
[claude-hooks.json](../assets/claude-hooks.json) into `.cursor/hooks.json` and
`.claude/settings.json`. The fragment paths assume the platform's installed
skill root above; registration adapts only that path when this project uses a
different supported delivery location.

During observation setup, verify readiness through the
[host adapter](ci-notify-hosts.md). Do not merge fragments, edit host settings,
or otherwise rewrite `.cursor/hooks.json` or `.claude/settings.json` from
execute-plan. Installed files alone do not prove notification delivery. Codex
uses its [yielded-cell adapter](ci-notify-codex.md), not these hook fragments.
