---
id: SEED-012
status: active
planted: 2026-10-27
planted_during: Owner request for website backlog items
trigger_when: Modernize and automate releases of lizard.ws
scope: bounded
---

# SEED-012: Website evolution

These are the owner's queued website stories in requested order. Adding them
does not start implementation or make them ready for execution.

<a id="modernize-website-react"></a>

### Modernize the website with React and conditional web tests

**Identity:** SEED-012#modernize-website-react
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/005-modernize-website-react/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"fe85d0aea28dadf96d3ccf8c7123bd3fb5fa1992a9e0830aab830009256fb76b","plan":"930c12b70e7dce45f849e3d79d4f19160e57b0981cb0d3d808255bdac0beba31"}}
```

**Beneficiaries:** Website visitors trying Lizard and contributors maintaining
its homepage and analysis demo.

**Goal:** Visitors retain a working homepage and analysis journey, while
contributors can develop, build and verify a complete React frontend
reproducibly, with browser CI limited to changes in the web surface.

**Scope:**

- Replace the homepage and demo frontend with the latest stable matching
  `react` / `react-dom` release available when implementation starts. Resolve
  the release then and lock it; do not use a canary or leave an unbounded
  install. React owns the page and interaction. Remove the superseded DOM
  script, page markup, stylesheet copies and obsolete frontend artifacts.
  Reuse the current presentation and content without a visual renovation.
- Preserve installation/documentation links, language selection, existing
  examples, actual function metrics and file totals. Preserve keyboard access,
  visible focus, narrow-screen usability, status announcements, pending
  feedback, empty results and recovery without losing the entered source.
  An unavailable example does not prevent analysis of pasted source.
- Keep `POST /analyze`, `/analyse` JSON/form compatibility and
  `website.application` usable. Keep the real analyzer, server-owned language
  choices and limits, 64 KiB UTF-8 source and 512 KiB request bounds, the
  browser's 20-second timeout, same-origin assets/API, existing response
  security policy and no-store POST responses. Source remains processed in
  memory without app logging or persistence. Keep published logo and favicon
  URLs. These are existing compatibility and operational constraints from
  `index.py`, `website/static/demo.js` and `docs/website.md`, not new limits.
- Use npm with a committed lockfile and a pinned supported Node 24 LTS
  toolchain. Supply it through the repository's Nix entry point as well as CI;
  document clean installation, build, local serving and tests. Use Vite to
  build assets served by Flask; Node is a build/test dependency only.
- Enable Gherkin scenarios with `@cucumber/cucumber` driving Playwright's
  browser library against the real local app. Begin with Chromium, including
  desktop and narrow viewport journeys. Each scenario owns and cleans up its
  browser context; the harness owns the local server and failure artifacts.
- Add a separately conditioned web job to the existing Python workflow. Keep
  Python checks running independently. The exact change boundary and event
  comparison below own the web decision; prove both run and skip behavior.

**Deferred promises:** No visual renovation is queued: the owner confirmed
the current presentation is the selected design. This story selects no
replacement design.
Tag-triggered deployment, live GCP publication, DNS and traffic changes belong
to the release story. SSR, an additional Node service, additional browser
engines, a router, accounts, saved reports and editor libraries are not delivery
commitments. A small no-JavaScript notice links to CLI/documentation; this
migration does not promise the complete homepage without JavaScript.

**Key examples:**

- Open the homepage → find installation/docs or reach the demo by keyboard →
  analyze the Python `shipping_cost` example → see one function, CCN 3,
  NLOC 6, 23 function tokens and one parameter from the actual server.
- Select another language → explicitly load an available example or paste
  source → analyze it in that language. Changing the selection alone keeps
  entered text. Valid source without functions produces a clear empty result.
- Submit empty or oversized source → receive actionable feedback while text
  remains editable → correct it and successfully retry. During analysis,
  announce progress and prevent duplicate submission; unavailable or timed-out
  analysis hides stale results, preserves source and offers retry/local CLI.
- Start from a clean checkout → install with `npm ci`, build and run Cucumber
  with Playwright → exercise the built React page and real API without any
  retired frontend. Existing API/Python checks also run without a web build.
- Change a web source, web dependency/configuration or web test path → the
  web CI job runs. Change only `lizard_core/options.py`, a language reader,
  `docs/website.md` or planning prose → the web job skips and Python checks
  continue. A mixed change runs the web job.

**Web change boundary:** One classifier owns the following repository-relative
paths; the workflow consumes its Boolean output instead of repeating the list:

- `website/**`, `templates/**`, `index.py`, `app.yaml`, `.gcloudignore`;
- `requirements.txt`, `dev_requirements.txt`, `shell.nix`, `flake.nix`,
  `flake.lock`, `.gitignore`;
- `test/test_website.py`, `scripts/web_changes.py`, `test/test_web_changes.py`;
- `.github/workflows/python-app.yml`.

Frontend package manifests, lockfile, build/Cucumber configuration, source,
scenario support and browser artifacts policy live under `website/frontend/`
and are covered by `website/**`. Website-only runtime code belongs under the
same boundary. The existing unused Python lockfiles are not build inputs;
if implementation starts consuming them, align this list with that evidence.
Shared analyzer code stays under Python coverage even though the website calls
it: including every analyzer dependency would defeat the requested skip case.
Documentation alone does not trigger browser tests.

For existing-branch pushes compare event `before` to `after`; for PRs compare
the merge base of base/head to head. For a new branch compare its head with
its merge base against the default branch; an initial repository without such
a base compares against the empty tree. Include added, modified, deleted and
both sides of renamed paths, without truncating the changed-file list. An
unresolvable comparison fails the detection job visibly; it never silently
reports no web changes. Preserve the current branch/PR event scope; website
release tags remain the later story's concern.

**Exploration and investigation:**

| Direction | Fit and consequence |
| --- | --- |
| Keep Flask/Jinja and vanilla JavaScript, add browser tests | Smallest change and already functional, but does not fulfill the explicit complete React replacement. |
| Complete React page, Vite assets, existing Flask service | Selected: one frontend and one deployed Python service; reuses the analyzer/API and deployment shape. Requires a build artifact and moves rendered-page proof into browser tests. |
| React framework with a Node rendering/service tier | Adds runtime, routing and deployment ownership; no current journey requires these capabilities. |

At revision `40d89f7`, 27 website HTTP tests passed. A disposable local probe
used Node 24.21.0/npm 11.19.0, React 19.3.1, Vite 8.3.5, Cucumber 13.3.0 and
Playwright 1.64.1: both current-demo/retry and built-React/real-analysis
scenarios passed under Flask's CSP. Preventing the analysis request made the
React metric scenario fail; restoring it passed. The probe and its server were
removed before this record. It establishes feasibility, not completion of the
migration or hosted CI. Git fixture observations covered push run/skip inputs,
PR merge-base isolation and rename-out detection. The executable plan carries
commands, consumer effects and remaining implementation proof.

**Architecture:** Keep Flask as HTTP/API and runtime configuration owner,
`lizard.analyze_file.analyze_source_code` as metric owner, and React as the
single page/interaction owner. Use a minimal Flask HTML shell containing
escaped configuration data and Vite manifest-selected local assets. Do not
copy the language registry or source-limit policy into JavaScript. Keep the
current CSS presentation in one React source stylesheet and preserve public
image paths. Build outputs are generated and ignored; deployment packaging
includes the built manifest/assets, excluding Node modules and test/source
artifacts. Build/watch followed by Flask serving gives a same-origin local
workflow without weakening production CSP for a dev server.

Python tests own API/HTTP compatibility and filesystem-bound asset-loader
contracts; browser scenarios own rendered React behavior. Ordinary Python
checks require neither npm nor a prebuilt bundle. A missing build gets a clear
local build diagnostic while the API stays available; it never falls back to
the old frontend. This also gives later design changes one place to edit and
the release story one build artifact to deploy, without implementing either.

Applicable Accepted decisions are [ADR-0000 — Use ADRs for architectural
decisions](../../docs/adrs/0000-use-adrs-accepted.md), preserving human ADR
ownership, and [ADR-0001 — Ubiquitous language for Lizard domain
concepts](../../docs/adrs/0001-ubiquitous-language-accepted.md), preserving CCN,
NLOC, token count and parameter count meanings. Index and in-file statuses
agree; neither specifies a frontend stack and no conflict was found. This is
feature-local design, not an ADR proposal or approval.

**Sources checked on 2026-10-27:** [React versions](https://react.dev/versions),
[Node release support](https://nodejs.org/en/about/previous-releases),
[Vite backend integration](https://vite.dev/guide/backend-integration),
[Cucumber JS](https://github.com/cucumber/cucumber-js),
[Playwright library](https://playwright.dev/docs/library),
[npm clean installs](https://docs.npmjs.com/cli/v11/commands/npm-ci/), and
[GitHub path comparisons](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#git-diff-comparisons).
Exact package versions above came from the npm registry and the local probe;
recheck React stable when execution begins.

<a id="release-website-from-tags"></a>

### Deploy website releases from version tags

**Identity:** SEED-012#release-website-from-tags

**Beneficiaries:** Maintainers releasing lizard.ws and visitors receiving the
published website.

**Goal:** A maintainer publishes a website version tag and GitHub Actions
automatically deploys that version to the existing GCP-hosted website.

**Scope:** Use the website-specific tag convention
`web-<major>.<minor>.<patch>`, for example `web-1.2.3`. A matching valid tag
triggers a GitHub Actions workflow that builds and deploys the tagged source
to the website, reports the deployed version and URL, and exposes a failed
release clearly. Other tags and ordinary pushes do not trigger this website
deployment. Document tagging, required validation and GCP authentication,
release verification, and recovery to the previous deployed version.

**Key examples:**

- Publishing `web-1.2.3` deploys the source identified by that tag through
  GitHub Actions, with an inspectable deployment result.
- An unrelated tag, malformed website version tag, or ordinary push does
  not deploy the website.
- A deployment failure is visible in Actions and the maintainer can use the
  documented recovery path.

**Refinement needed:** Define tag validation, version-to-App-Engine mapping,
validation reuse, deployment credentials and failure/recovery behavior.
Keep website tags distinct from the analyzer package's release mechanism.
