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

Optional `integratedStateCompletion` declares that a successful validation of
a later snapshot on the same target may complete an earlier accepted revision
it contains. Without it, completion stays exact. Declare it only when the
declared check validates the whole snapshot, so a later success covers every
change it incorporates:

```json
{ "integratedStateCompletion": { "check": "ci.yml", "context": "push", "jobs": ["test"] } }
```

`check` names the observed check: for GitHub Actions, the selected workflow
selector (the `DOUGH_CI_WORKFLOW` value, default `ci.yml`). `context` names the
triggering event (`push`). Optional `jobs` lists jobs that must each appear
once and succeed in that attempt. A GitHub run's `success` already requires
every job and matrix shard it ran, so list only jobs that a run could skip;
each entry must equal the displayed job name, such as `Unit (1/2)` for a
matrix shard. A malformed declaration ends observation
with `CI_MONITOR_UNAVAILABLE`. For a configured command, `check` and `context`
must equal the optional `check` and `context` strings its discovered attempts
name; `jobs` does not apply. Only an attempt naming the target in its own
optional `branch` counts, and discovery lists attempts newest first. Another
attempt of that snapshot's check with an equal or later `time`, or with a
`time` the first listed lacks, leaves it unresolved. An attempt without `branch` keeps exact coverage but never
completes another revision. A poll that cannot discover runs withdraws the
evidence until a later poll judges it again. The declaration never changes the
provider's scheduling or exact-commit reuse.

This observer handles one selected CI check per execution. It does not support
pull-request-only CI, aggregate multiple checks, or observe deployment. If this
project needs an unsupported mode, report missing monitoring coverage and
continue execution without claiming CI observation.

Set the applicable environment variables on the observer launch (and preserve
them in any project tooling wrapper):

| Variable | Meaning |
| --- | --- |
| `DOUGH_CI_WORKFLOW` | GitHub default only: workflow filename or numeric ID; it alone selects the observed workflow, whatever its display name; default `ci.yml` must be verified |
| `DOUGH_CI_WORKFLOW_NAME` | GitHub default only, optional, no default: the selected workflow's exact display name, as a consistency check; a selected run reporting another name ends observation with `CI_MONITOR_UNAVAILABLE` |
| `DOUGH_CI_MAILBOX_ROOT` | Optional private shared mailbox directory; default `/tmp/dough-ci-$UID` |
| `DOUGH_GITHUB_REQUEST_OWNER_ROOT` | GitHub default only, optional: private directory, writable by the observer, through which this user's observers and dashboards asking with one `gh` host and login share one GitHub request owner and its rate-limit wait; default `/tmp/dough-github-requests-$UID`. Where it cannot be written, each request fails as `gh failed` without asking GitHub |

The branch is a required positional argument for the authorized target, never
an inferred `main` or the execution branch name.
Verify the GitHub selector with
`DOUGH_CI_WORKFLOW=SELECTOR node '/ABSOLUTE/RESOLVED/SKILL/scripts/ci-github-diagnostic.mjs' workflow OWNER/REPO`:
an unknown selector fails, and the printed `name` is the display name to use if
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
end coverage. A wait GitHub directs for a rate limit, met by this observer or
by another process sharing its request owner, is not an observation error: the observer emits one `CI_MONITOR_THROTTLED` record, asks GitHub
nothing until the directed time, and resumes within the same budget. Registering a pushed revision makes the observer check at once;
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
