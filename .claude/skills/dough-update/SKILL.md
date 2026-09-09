---
name: dough-update
description: Apply the latest released Open Dough guidance from this project's recorded source, or from a supplied repository URL on first install. Use when the user asks to update Open Dough or invokes dough-update.
---

# Update Open Dough

Install and update the latest numeric Open Dough release for Codex, Cursor, and
Claude Code together. Codex and Cursor share `.agents/skills/`; Claude Code
uses `.claude/skills/`. The running tool is only the entry context used to find
the recorded source for a no-URL update.
Pin that release with Git before any repository script runs, then call the
inspected snapshot's helper. Do not reimplement the helper's comparison or
installation decisions.

Local-guidance replacement is not supported by dough-update.
Do not inspect, assess, prepare, repair callers for, or remove the target project's
local guidance. Do not fetch a source or run an installer for such a request. A
one-time adoption is project-specific work outside this reusable updater.
Ordinary Open Dough release updates remain available from the recorded source.

1. Capture the target project's absolute path before fetching anything. Use this
   project unless the user supplied another target. Keep this target distinct
   from the Open Dough source fetched below.
2. If the user asked to install or update a specific version, tag, or branch,
   stop. Say `Open Dough installs and updates the latest numeric release only.
   Requested-version updates are not supported.` Do not fetch or write.
3. Identify the running tool from the current host. Do not infer it from which
   skill directories exist. Pass it as the installer platform hint, then
   install the same released payload into every native root:

   | Running tool | `--platform` | Installed files |
   | --- | --- | --- |
   | Codex | `codex` (omitting `--platform` is equivalent) | `.agents/skills/` |
   | Cursor | `cursor` | `.agents/skills/` (shared with Codex) |
   | Claude Code | `claude` | `.claude/skills/` |

   The release payload includes skill entrypoints and supporting files declared by the
   pinned release's installer and baseline-comparison helper. Treat that
   release as authoritative: payload skills may be added between releases, so
   a path absent from the installed release is not by itself a reason to stop.
   The numeric `VERSION` record and the
   recorded `SOURCE` live beside each `dough-update/SKILL.md`. Installation
   writes the same `SOURCE` from the supplied repository URL or local path,
   then `VERSION`, in both physical roots. Ordinary no-URL updates reuse the
   invoking root's recorded `SOURCE`.

4. Resolve the Open Dough source from the invoking root. If that updater
   destination already exists and has a usable recorded
   `dough-update/SOURCE`, use that source. For an ordinary update, if that
   record is missing or unusable, stop and report that ordinary update cannot
   establish the recorded baseline. Do not ask for a URL, do not treat the
   destination as a first install, and do not substitute the target project's
   remote or local working-tree content. If the destination does not exist,
   use the repository URL supplied by the user; if none was supplied, ask for
   it before proceeding. First installation takes a supplied source. Explicit
   force uses the recorded SOURCE when present; otherwise it takes a supplied
   `--url`.
5. Make a fresh temporary directory. Using only Git, pin the highest numeric
   release before any repository script runs. Do not clone the default
   branch, and do not execute `install.sh` or `open-dough-release.sh` from
   the working tree or from an unpinned clone.

   a. Run `git ls-remote --tags -- <source-url>`. Keep `vMAJOR.MINOR.PATCH`
      tags. When both a tag object and a peeled `^{}` line exist, use the
      peeled commit. Select the highest version by comparing each component
      as a decimal integer string; do not use shell arithmetic.
   b. `git init` the work directory, `git fetch --depth 1 <source-url>
      <commit>`, and check out that commit detached. Confirm
      `git rev-parse HEAD` equals the peeled commit.
   c. Inspect that snapshot's `src/install/open-dough-release.sh`,
      `src/install/open-dough-release-apply.sh`, `install.sh`, the helpers they
      source (including `src/install/open-dough-register-hooks.sh`,
      `src/install/open-dough-register-hooks.mjs`, and
      `src/install/open-dough-register-hooks-merge.mjs` when present), and
      every release payload source they declare under `src/skills/`.
   d. Run the inspected helper, quoting paths. Codex may omit `--platform`.
      For an ordinary update of a recorded installation, run
      `bash <snapshot>/src/install/open-dough-release.sh apply --target
      <captured-project> --platform <tool> --checkout <snapshot>` and do not
      pass `--url`; the helper reads `SOURCE`. For a first installation, also
      pass `--url <source-url>`. Pass `--force` only when the user explicitly
      authorized a forced reinstall. When the invoking root has a usable
      SOURCE, omit `--url`; otherwise include `--url <source-url>`.
      If apply reports that HEAD is not the pinned latest, stop. Do not fetch
      or check out replacement files after inspection. Proceed only if the
      inspected files write solely to the release-declared payload paths
      under both native skill roots, each updater's `SOURCE` and `VERSION`
      records, and the managed host-hook settings they register
      (`.cursor/hooks.json` and `.claude/settings.json`) in the captured
      target project, preserving distributable source, unrelated project
      files, unrelated settings entries, and home guidance. Registration is an
      install/update concern; after apply, observation readiness and observer
      start/stop belong to execute-plan and must not rewrite those settings.
6. Trust the helper's comparison. An ordinary update without a supplied URL
   fetches the recorded VERSION tag as data and compares the installation with
   the payload declared by that recorded release before any skip or
   replacement. Payload paths newly added by the release must be absent before the helper
   may add them; a pre-existing collision refuses without writes. Equal
   recorded versions that still match that baseline must not invoke
   `install.sh` or write the selected files;
   untagged source changes alone do not require writes. Changed or missing
   managed files, or an unavailable recorded tag, refuse without writing even
   when the recorded version equals latest. An older recorded installation is
   replaced with latest only when those files are unchanged. If ordinary
   update cannot establish that baseline — missing or unusable SOURCE or
   VERSION, an unavailable recorded tag or source, baseline metadata mismatch,
   or changed or missing managed files — refuse without writing, forcing, or
   treating the destination as a first install. Explicit `--force` skips that
   comparison and replaces the selected installation with latest, including
   edited, incomplete, equal, or newer files, writing the payload then SOURCE
   then VERSION. A supplied-URL missing selected record advances directly to
   latest. A verified newer selected record is preserved with no downgrade; an
   unverifiable newer record is unsupported without writes. A malformed
   selected `VERSION` is an error, not unknown. Fetch, tag, and invalid-highest
   release failures must not write the target or fall back to a lower release
   or branch. If replacement starts and then fails, report that installed files
   may be incomplete, that the last successful record was left unchanged, and
   that explicit `--force` reinstall is the recovery path.
7. Report the helper's source URL, release tag and commit, running tool and
   native skill root, all installed payload paths, previous version or
   unknown, and actual outcome. After a replacement,
   tell the user to start a fresh session in the same tool, then invoke
   `/dough-update` in Cursor or Claude Code, or `$dough-update` in Codex, to
   use the updated guidance. Do not claim an update succeeded if fetching,
   validation, comparison, or installation failed. Manual reading of the tagged
   `CHANGELOG.md` is sufficient; do not print changelog text here.
