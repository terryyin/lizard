# Running and releasing lizard.ws

The website is a Flask adapter around `lizard.analyze_file.analyze_source_code`.
`index:app` is the application; `website.application` remains a compatibility
import. The page itself is a React app in `website/frontend/src/` (one entry,
`main.jsx`, and one stylesheet, `site.css`). Vite builds it into the ignored
`website/static/dist/`, served at `/website/static/dist/`, with a
`manifest.json`. Flask serves `/` as a small shell (`templates/index.html`):
metadata, the favicon, the manifest-selected script and stylesheet, a
no-JavaScript notice linking the CLI documentation, and the server-owned
language choices and source limit as escaped JSON. React renders everything
else. Node is a build/test dependency only; the deployed service is Python.

Keep these responsibilities in one place each. Flask owns HTTP, the API,
response headers and the runtime configuration (language choices from
Lizard's readers, source limits); `lizard.analyze_file` owns the metrics;
React owns all page content and interaction and reads that configuration
instead of copying it. A missing build is a diagnostic, never a fallback to
an older frontend. Python tests own API/HTTP compatibility and the manifest
contract, using a temporary manifest rather than a real build, so they need
neither npm nor build output. Browser scenarios own rendered behaviour
against the real analyzer and never fake metric results.

## Local development

Build the frontend before serving; without a build `/` answers 503 with the
build commands, while `/analyze` keeps working:

```sh
nix develop -c npm --prefix website/frontend ci
nix develop -c npm --prefix website/frontend run build
nix develop -c python -m flask --app index run --port 8080
```

While editing the frontend, `run watch` rebuilds on change in another
terminal; reload the Flask-served page. There is no separate dev server, so
the production Content Security Policy applies locally too. Python tests need
neither npm nor a build:

```sh
nix develop -c python -m pytest test/test_website.py
```

Development dependencies include a Flask release compatible with the current
Python. This does not add Flask to the distributed analyzer's dependencies.
Production uses Python 3.13 and `requirements.txt`. To check that environment
separately, use Python 3.13 to create an isolated environment, install
`requirements.txt` and pytest, then run `python -m pytest test/test_website.py`
from the checkout. The Nix environment remains the repository tooling wrapper.
When invoking that separate Python from Nix, use `python -I -m pip` and
`python -I -m pytest` so Nix's Python-specific import path does not leak into
the production environment.

The JSON API is `POST /analyze` with `Content-Type: application/json`:

```json
{"lang":".py","code":"def answer():\n    return 42\n"}
```

`filename` can replace `lang`. Supported suffixes come from Lizard's existing
language readers; omitting both uses `.c` for existing callers. The response
contains file `filename`, `nloc`, `token_count` and `function_list`. Each
function includes its name, long name, start/end lines, NLOC, cyclomatic
complexity, token count and parameter count. `/analyse` also accepts JSON or
the older form fields `content` and optional `lang`, through the same adapter.

The app limits source to 64 KiB of UTF-8 and request bodies to 512 KiB.
Gunicorn uses one synchronous worker per instance with a 15-second timeout;
the browser stops waiting after 20 seconds and offers recovery. App Engine
is configured with `max_instances: 2`. These controls bound work but are not a billing
cap or a distributed rate limiter. Source is processed in memory and is not
saved or logged by the app. Do not enable debug mode or request-body logging
on the public service. Use the local CLI for private source or larger files.

## Browser tests

Browser scenarios live in `website/frontend/features/` and run with Cucumber
and Playwright (Chromium). The Nix shell supplies Node 24 from a separately
pinned nixpkgs input; npm and the packages come from
`website/frontend/package-lock.json`. Install once per clean checkout, then
run the suite:

```sh
nix develop -c npm --prefix website/frontend ci
nix develop -c npm --prefix website/frontend exec -- playwright install chromium
nix develop -c npm --prefix website/frontend run build
nix develop -c npm --prefix website/frontend run test:e2e
```

The suite tests the current build output; rebuild after frontend changes.
Without a build it stops at startup and prints Flask's build diagnostic.

On Linux CI, also install the browser's system libraries with
`playwright install --with-deps chromium`. `run test:e2e:dry` lists the
scenarios without starting anything.

The suite starts the real Flask app (`python -m flask --app index run`) on a
free local port, opens a fresh browser context per scenario, and stops the
server when the run passes, fails or is interrupted. A scenario fails if the
page throws or reports a Content Security Policy violation. Scenarios use the actual
analyzer; recovery scenarios stall or stop the owned server rather than faking
responses. A failure saves a screenshot under `website/frontend/reports/` and
prints the server log tail. To check that the suite catches a broken analysis
path, set `E2E_BLOCK_ANALYSIS=1`; the browser then cannot reach `/analyze` and
metric scenarios fail.

### Continuous integration

`.github/workflows/python-app.yml` runs on every branch push and on pull
requests to `master` and `feature/**`; there is no workflow path filter. Its
cheap `changes` job runs `scripts/web_changes.py`, and the `web` job runs only
when that job outputs `web=true`. The Python matrix (`build`) does not depend
on either job, so it always runs and never waits for a browser build.

`scripts/web_changes.py` is the only home of the web change boundary
(`WEB_PATHS`): `website/`, `templates/`, the Flask/App Engine entry files, the
Python requirement files, the Nix files, `.gitignore`, the website and
selector tests, the selector itself and this workflow. Change the list there;
the workflow consumes only its Boolean output. Shared analyzer code
(`lizard_core/`, `lizard_languages/`, `lizard_ext/`) and documentation are
outside the boundary on purpose: a docs-only or analyzer-only change runs the
detector and the Python checks and skips the web job without installing npm
packages or browsers.
The root `requirements.lock` and `requirements-dev.lock` are not website build
inputs; add them to `WEB_PATHS` if the website or web job starts using them.

The selector lists changed paths with `git diff --name-only --no-renames -z`,
so additions, edits, deletions and both sides of a rename count, without
truncation. It compares:

- a push to an existing branch: the event's `before` to `after`;
- a pull request: the merge base of base and head to head, so web changes
  that exist only on the base branch do not count;
- a new branch (`before` is all zeros): the merge base of the head with
  `origin/<default branch>` to the head, or the empty tree when there is no
  such base (an initial repository).

If the comparison cannot be resolved (a missing or unknown commit, for
example after a force push that removed `before`), the `changes` job fails
with `web_changes: cannot compare: ...` instead of reporting no web changes;
rerun or push a new commit once the history is available.

Run the selector locally from a checkout with full history:

```sh
python scripts/web_changes.py push BEFORE AFTER
python scripts/web_changes.py pull_request BASE HEAD
python scripts/web_changes.py push 0000000000000000000000000000000000000000 HEAD --default-branch master
```

It prints the compared range, `web=true` or `web=false` and the matching
paths; under Actions it also writes `web=...` to `$GITHUB_OUTPUT`.
`test/test_web_changes.py` drives the same script against temporary Git
histories.

The `web` job uses Node from `website/frontend/package.json` `engines`, the
npm version from its `packageManager`, Python 3.13 with `requirements.txt`,
then `npm ci`, `playwright install --with-deps chromium`, `run build` and
`run test:e2e`, the same commands as above. When it fails, the
`browser-reports` artifact holds `website/frontend/reports/` (the Cucumber
HTML report and failure screenshots; they contain only test fixtures). The
server log tail is in the job log.

## Releasing

See the [release guide](website-release.md) for preview deployment, public
traffic and recovery, and the [deployment identity guide](website-deployment-identity.md)
for initial Actions authentication and permissions.
