# Modernize the website with React and conditional web tests

**Identity:** SEED-012#modernize-website-react
**Source:** [Selected story](../../seeds/SEED-012-website-evolution.md#modernize-website-react)

## Goal and boundaries

Give visitors the existing homepage/analysis journey through one React frontend,
and contributors a reproducible build and Cucumber/Playwright suite that runs
in CI for web changes and skips unrelated changes. The source owns scope,
examples, the exact path boundary and event comparisons; do not duplicate or
silently widen them here. Preserve the current presentation. The visual design
reference and automatic website deployment belong to the two sibling stories.
No product implementation, deployment or Take is authorized by this preparation.

## Current decisions

- Use npm, a committed `website/frontend/package-lock.json`, a pinned npm
  version and supported Node 24 LTS. Put package/configuration/source/scenarios
  under `website/frontend/`. Add Node to the root Nix development environment;
  preserve the existing Python toolchain. If its old nixpkgs pin cannot supply
  Node 24, use a separately pinned Node input rather than upgrading all Python
  packages. Keep CI and local version declarations consistent.
- At execution start resolve matching latest stable React/React DOM from the
  registry and record the installed versions in the lockfile. Preparation's
  observed 19.3.1 is evidence, not a freeze of a future release requirement.
- Vite builds one page entry and manifest into an ignored build directory
  under `website/static/`. Flask reads that manifest and serves the existing
  root route with a minimal template, escaped bootstrap data, metadata and
  no-JavaScript CLI/docs fallback. React owns all page content and interaction;
  the shell is not a second frontend. Local build/watch plus Flask avoids a
  separate HMR-origin/CSP exception. Do not add routing, SSR or a Node service.
- Reuse `index.py` validation/serialization, `lizard.analyze_file`, the language
  registry, existing examples, visual styles and public image URLs. Move the
  CSS into the React source without parallel stylesheet copies. Remove the old
  `demo.js` and replaced UI markup/assets in the migration slice itself.
- The browser runner is Cucumber JS with Playwright library, initially
  Chromium. Test the built Flask-served artifact with actual analysis, not
  Vite preview or fake metrics. The harness owns readiness, child-process
  shutdown, per-scenario browser contexts and useful failure output.
- Python tests continue without npm/build output. Keep real API tests; move
  DOM/content assertions to browser scenarios. HTTP shell/manifest tests may
  use temporary filesystem artifacts, the allowed filesystem seam. An unbuilt
  homepage gives a clear build diagnostic; API routes still work. No legacy
  frontend fallback conceals missing build artifacts.
- The existing Python workflow remains the CI entry point. A cheap detection
  job runs on its current push/PR events and a separate web job consumes its
  result. Python matrix jobs do not depend on web-job success or a web build.
  Use a Python standard-library Git/path classifier with one authoritative
  path list from the source, NUL-delimited `git diff --name-only --no-renames`
  output and full history needed for the source's comparison semantics.
  Missing comparison evidence fails detection visibly. No top-level workflow
  path filter silently suppresses Python checks or leaves a required web check
  pending. Only the web job installs Node packages/browsers and runs E2E.

PFE: no existing frontend package/build/browser runner exists. `index.py`
currently owns the correct API, registry-derived options and limits;
`website/static/demo.js` owns the interaction being replaced, not an additional
service to retain. The Git/path classifier is a small new responsibility;
there is no current product CI selector to reuse. Keep it independent of the
installed Open Dough lifecycle tooling. `docs/website.md` is the maintained
home for feature-local build/serve/test/package knowledge.

[ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md) keeps architectural
approval human-owned; [ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
constrains metric names and meanings. No conflict, supersession or ambiguous
status was found. Existing structure supports this feature-local arrangement;
no new North Star topic or ADR is needed for this plan.

## Outside-in proof ownership

| Source promise | Owner | Observation |
| --- | --- | --- |
| Clean reproducible browser-test entry point | 1 | Fresh npm install from lockfile; Cucumber discovers and executes scenarios through real Flask; intentional missing analysis fails, ordinary run passes; harness exits and cleans up on either outcome. |
| React owns the complete page, current content/presentation and analysis | 2 | Built-page scenarios observe install/docs, language/example behavior, actual CCN/NLOC/tokens/parameters and totals; no old script/style implementation remains. |
| Keyboard/narrow-screen access, pending, empty result and recoverable failure | 1 then 2 | Establish scenarios on the current page, then pass them on React using accessible names/roles; edit and retry retain source; stale results disappear while pending/failed. |
| Existing API, source/request bounds, no persistence/logging, response policy and imports | 2 | Real `test/test_website.py` requests, Unicode size/invalid input/legacy aliases; preserved in-memory analyzer path and inspected logging; browser requests under unchanged CSP. |
| Published images and reproducible deployable build | 2 | HTTP asset checks plus built Flask page; inspect App Engine upload list against include/exclude contract, no live deploy. |
| Browser CI runs for web and mixed changes, skips unrelated changes; other CI continues | 3 | Git-history fixture tests drive the actual selector CLI/output, plus hosted run and skip receipts for the actual job condition with Python jobs present. |
| Stable React and supported locked toolchain | 1 then 2 | Registry stable check at execution start; Nix/CI runtime observations and lockfile versions; `npm ci` leaves the lockfile unchanged. |

Keep these promises attached to their owners if execution refines the plan.
Existing probe evidence establishes approach feasibility only; final product
proof must use the implementation candidate.

## Ordered slices

### 1. Contributors can verify the existing analysis journey in a browser
Type: Behavior
Status: done
Proof: A clean contributor install runs Gherkin scenarios with Playwright
against the actual homepage, and a disabled analysis path makes the metric
scenario fail instead of yielding a false green.

Behavior: Given a clean checkout and the documented Nix entry point, installing
from the frontend lockfile and running the web-test command starts the real
local app, exercises the visitor journey and returns an actionable result,
with owned processes closed on success and failure.

Add the minimal npm/Cucumber/Playwright harness and supported Node provisioning.
Resolve the stable React version now for slice 2. Document exact commands in
`docs/website.md`, including browser installation. Establish reusable scenarios
for real sample metrics, language/example selection, empty result, empty and
oversized input followed by retry, keyboard/narrow viewport use, progress and
unavailable-analysis recovery. Preserve entered source and distinguish empty
results from an error. Use the real API; do not mock parser behavior or return
synthetic metric results. Network failure may be induced by taking the owned
server offline and restarting it; a stalled owned server can prove the bounded
wait without weakening timeouts. Keep tests bounded, initially one browser
worker, with Cucumber timeouts long enough for the genuine 20-second timeout.

Commands to establish: `nix develop -c npm --prefix website/frontend ci`,
`nix develop -c npm --prefix website/frontend run test:e2e` and a documented
locked Playwright Chromium-install command (CI also installs Linux browser
system dependencies). Exercise discovery/dry-run and an actual success/failure
cycle, not just package presence. Keep `nix develop -c python -m pytest
test/test_website.py` green.

Safe stopping point: contributors gain working regression coverage of the
existing page. CI conditioning and React replacement are explicitly pending;
this interim frontend is removed by slice 2. No framework-only scaffold slice.

### 2. Visitors use a complete React page served by the existing app
Type: Behavior
Status: planned
Proof: The regression journeys pass against a clean production build served by
Flask, existing API checks pass with and without build output, and the upload
artifact contains all referenced assets and no Node dependencies/source tests.

Behavior: Given the built application, opening the homepage provides the
existing discovery, installation and analysis journey through React; errors
remain recoverable and results come from the existing analyzer.

Move page content, current style and example interaction into a small React
implementation. Build with Vite, integrate manifest-selected assets and escaped
server-owned configuration, and preserve metadata, logo/favicon URLs and CSP.
Keep language changes non-destructive; only Load example replaces source.
Maintain a coherent idle/pending/success/error state, cancel/finish the bounded
request correctly and show a valid no-functions state. Preserve source privacy
copy and links to local usage. Remove all superseded UI code as part of the
same change. Avoid app/framework abstractions for the deferred renovation.

Move homepage DOM assertions out of Python tests into Cucumber. Python HTTP
checks retain API, configuration and static compatibility responsibilities.
Use a filesystem fixture for a manifest contract, not a fixture that pretends
to prove a working React build. Explicitly check missing-build diagnostics and
API availability in a checkout with generated assets absent. Then run
`nix develop -c npm --prefix website/frontend run build` and the browser suite
against those real generated assets; exercise the current desktop/narrow and
keyboard journeys without pixel-perfect redesign commitments. Reuse the
source's stated metric meanings and exact sample values.

Update `.gcloudignore` so only deployable Python/runtime files, shell template,
public images and the required generated bundle/manifest are uploaded. Do not
ship `node_modules`, frontend source, test reports or local probes through the
current broad `!website/**` inclusion. Inspect `gcloud meta
list-files-for-upload` locally, or an equivalent documented gcloud upload-list
command; this is file selection, not deployment. Check that Flask and App
Engine handlers use the same built-asset URL paths. Update the manual build
prerequisite in `docs/website.md`, retaining release history and rollback facts.

Safe stopping point: one functioning React frontend and one documented build
artifact; automated conditional CI remains the only undelivered outcome.

### 3. CI runs browser coverage exactly for the web change boundary
Type: Behavior
Status: planned
Proof: The actual change-selector entry point emits run/skip for repository
history fixtures, and hosted workflow evidence demonstrates web run and skip
while Python checks continue.

Behavior: Given a push or PR in the current workflow's event scope, relevant
changed paths cause the web job to install, build and run the same Cucumber
suite; unrelated-only changes skip that job without skipping Python checks.

Implement the source's path boundary and event semantics once in
`scripts/web_changes.py`, with CLI tests in `test/test_web_changes.py`. Cover
web/mixed and docs/analyzer-only changes, deletion/rename out of web paths,
PR merge-base excluding base-only changes, new branch comparison, and an
unresolvable base that fails visibly. Exercise real temporary Git histories
through the script that CI calls, rather than testing a second classifier.
Inspect each manifest/test/config path against the final actual layout.

Add detection and conditional web jobs to `.github/workflows/python-app.yml`;
keep the existing Python matrix independent. The web job uses Node 24, npm ci,
Python app dependencies, a production build and Chromium with the same
Cucumber entry point. Retain failure reports/screenshots/traces as appropriate
without uploading user source from public requests. Configuration changes are
inside the classified web boundary. A docs-only change still runs the cheap
detector and applicable Python checks; no npm/browser installation occurs.

Focused proof: `nix develop -c python -m pytest test/test_web_changes.py
test/test_website.py`, lockfile clean install/build and browser suite. Validate
workflow output-to-job wiring, including a forced wrong classifier result
that fails the relevant test. During authorized execution publication, obtain
one actual web-changing run and a subsequent docs-only increment's skipped
web job with Python checks present; use a substantive documentation update
already required here, not a meaningless commit. Record run URLs, SHAs, event,
changed paths and job conclusions. If hosted Actions is unavailable, keep
this proof pending rather than claiming local fixtures establish hosted wiring.
For PR semantics, real local history fixtures own the comparison proof; a
separate hosted PR need not be created solely for testing.

Safe stopping point: the selected story's promises are fulfilled. Preserve
later design and release stories and their order.

## Preparation observations and consumer trace

Checkout: `/Users/terryyin/git/lizard/.worktrees/modernize-the-website-with-react-and-conditional`,
revision `40d89f7e8e77fe62fd7a1c3c824fb746fd8e5c01`, observed 2026-10-27.
Scratch files existed only in ignored `.local/refinement-web-probe/` and were
removed; the local server exited. No tracked product edits were retained.

- `nix develop -c python -m pytest test/test_website.py -q`: 27 passed, 0.32s.
  Inspected setup: Flask test client and actual analyzer; assertions cover API
  metrics/validation, old homepage HTML, published images and old JS/CSS URLs.
  This observes HTTP behavior; it cannot prove browser rendering.
- Registry commands `npm view react version`, `npm view vite version engines
  --json`, `npm view @cucumber/cucumber version engines --json`, and `npm view
  playwright version engines --json` under Nix found React 19.3.1, Vite 8.3.5,
  Cucumber 13.3.0, Playwright 1.64.1 compatible with observed Node 24.21.0/npm
  11.19.0. No frontend package manager was already owned by the product.
- `nix develop -c nix eval --raw github:NixOS/nixpkgs/nixos-26.05#legacyPackages.aarch64-darwin.nodejs_24.version` returned 24.21.0 from
  nixpkgs revision `1f607aa79b6356ed45d3cd3c8079c196e79efa2c`. This confirms a
  separate supported Node package source without changing the Python pin;
  slice 1 still proves the final Nix shell/install command.
- `nix develop -c npm --prefix .local/refinement-web-probe run build` built a
  React entry and Vite manifest. The scratch server imported actual `index.app`,
  added only a probe route/static mount and supplied registry-derived language
  options and the source limit as escaped HTML data. The current after-request
  CSP remained active. Server command: `nix develop -c python -c 'import runpy;
  runpy.run_path(".local/refinement-web-probe/server.py")'` on localhost:18765.
- `nix develop -c npm --prefix .local/refinement-web-probe test` executed two
  Cucumber scenarios: current Python shipping example plus empty/retry, and a
  minimal built React button making a real `/analyze` request. All 12 steps
  passed. Shipping metrics were 3/6/23/1; React `classify` returned CCN 2,
  NLOC 4 with no browser errors. A scratch-only Playwright network abort under
  `PROBE_BLOCK_ANALYSIS=1 nix develop -c npm --prefix .local/refinement-web-probe
  test -- --name 'React build'` failed the metric observation at 15 seconds.
  Removing that condition passed both scenarios again in 0.538s. No synthetic
  analysis result supplied the successful proof. This is a disposable
  discrimination experiment, not the final test harness or full React migration.
- `nix develop -c gcloud meta list-files-for-upload` completed locally
  without cloud mutation. With one temporary file under
  `website/frontend/node_modules/`, the real upload list included that file,
  confirming the broad exclusion gap. The sentinel directory was removed
  immediately; slice 2 must demonstrate the corrected final upload list.
- A disposable Git repository under the same probe directory observed
  `git diff --name-only --no-renames -z BEFORE AFTER` for docs-only and web
  commits, `git merge-base BASE HEAD` then diff for a PR with a base-only
  analyzer change, and a rename out of the web tree. Assertions verified the
  exact resulting path lists. This establishes Git comparison behavior;
  slice 3 must still prove the classifier and hosted job wiring.

Consumer effects found by repository-wide search:

| Consumer | Effect and proof disposition |
| --- | --- |
| `templates/index.html`, `website/static/demo.js`, `website/static/site.css` | Replace page/DOM ownership together in slice 2; run slice 1 scenarios against the new build. |
| `test/test_website.py` homepage and old JS/CSS asset assertions | Move rendered-content proof to browser scenarios; retain valid HTTP/API and public-image contracts, add missing-build and real-build observations. Run the full focused file. |
| `index.py` homepage and security-header hooks | Consume build manifest and escape authoritative configuration; API and CSP stay real in the browser test, HTTP tests cover limits/compatibility. |
| `README.rst` public small-logo URL, `app.yaml` static routes | Preserve the published URLs and demonstrate bytes/MIME type; no unrelated README rewrite. |
| `.gcloudignore`, `docs/website.md` | Current broad website upload and no-build instructions become incorrect; upload-list observation and clean documented build own the correction. |
| Python workflow and root test/Make commands | Must retain Python-only operation without bundles; full pytest/make and slice 3 hosted run/skip own proof. |
| `lizard_core`, `lizard_languages`, `lizard_ext` consumers | Analyzer contract is reused unchanged. Full Python regression remains required; no browser-CI trigger expansion to all analyzer changes. |

At implementation, run the installed consumer-trace command against the actual
diff and resolve any newly observed consumers. This planning trace does not
pretend a yet-unwritten diff was already searched.

## Verification and execution gates

`AGENTS.md` requires Nix-prefixed tooling (Git excepted) and a full
`nix develop -c python -m pytest` before completing a change; also run
`nix develop -c make` for repository test/style checks. Focused Python and web
checks above apply at relevant slice boundaries. Execute Plan owns independent
post-change refactoring, agent commits, delivery, asynchronous CI and eventual
Story Wrap Up. This preparation creates no observer and performs no execution.

These slices each own one contributor/visitor outcome and proof loop; slice 2
is a cohesive replacement with no CI-breaking intermediate markup/asset
migration. There is no supplied numeric slice limit. Keep implementation,
focused proof and cleanup together; replan if actual integration complexity
invalidates these boundaries. Do not split files/layers into independent
scaffolding deliveries. If a future package or environment observation fails,
stop the dependent work and adjust this plan rather than relaxing the promise.

## Concern assessment

Slice-plan refinement was not needed: three bounded behavior outcomes own
all source promises, share one analyzer/configuration model and one path
classifier, and have explicit safe stopping points. No remaining slice-specific
concern was identified in this review. Hosted run/skip receipts are future
implementation acceptance proof, not a claim that CI was exercised during
preparation. No pre-Take human decision is required by this plan.

## Story obligations

### G1. Visible focus is not asserted by browser scenarios
Reported: slice 1 — "Visible focus styling is not asserted; keyboard reach and activation are."
Story clause: "Preserve keyboard access, visible focus, narrow-screen usability"
Disposition: receiving slice 2

### G2. No-JavaScript notice and published image URLs are not observed against the new page
Reported: slice 1 — "The no-JavaScript notice and the published logo/favicon URLs are not covered in the browser; they are HTTP-tested in pytest."
Story clause: "Keep published logo and favicon URLs."
Disposition: receiving slice 2

### G3. Linux browser install and clean-commit checkout are not exercised
Reported: slice 1 — "The Nix and Playwright path is proved on aarch64-darwin only. Linux `--with-deps` is documented but not run"
Story clause: "Supply it through the repository's Nix entry point as well as CI"
Disposition: receiving slice 3

## Execution record

- Mode: story-branch; branch `claude/modernize-the-website-with-react-and-conditional`
  on `origin`, target `master`; claim published at `3a11bc6`.
- Independent source-only review returned commitments P1–P50 (goal, scope,
  journeys, boundary, deferrals) and is used for every slice's acceptance.
- Slice 1 accepted: `nix develop -c npm --prefix website/frontend run test:e2e`
  passed 12 scenarios/107 steps against the current Flask page (coordinator
  rerun); `E2E_BLOCK_ANALYSIS=1 ... -- --name 'The Python sample reports'`
  failed at the metric status step; no Flask child remained afterwards. Full
  pytest 1751 passed. Stable React resolved at execution start: `react` and
  `react-dom` 19.3.1. Post-change refactor simplified `hooks.js` shutdown
  (exit hook kills the server; signals call `process.exit`); e2e and the
  forced failure were rerun after it. The interrupt path after that edit
  relies on Node's documented `process.exit` → `exit` behaviour and was not
  re-observed; pass/fail cleanup, which the plan requires, was.

## Learnings

- Stopping the owned Flask child with SIGSTOP/SIGCONT gives a real pending
  state and genuine 20-second timeout without faking responses (POSIX only).
- The Node nixpkgs input is locked at `718f670`, newer than the preparation
  observation; it still supplies Node 24.21.0/npm 11.19.0.
- `.gcloudignore` now excludes `website/frontend/` so a manual deploy before
  slice 2 cannot upload `node_modules`; slice 2 still owns the bundle contract.


The existing page already supplies the interaction and visual baseline. Its
API tests are fast but its HTML assertions couple Python-only testing to the
old rendered frontend. Moving those assertions to browser coverage is necessary
to make conditional web CI honest. The current upload wildcard would otherwise
ship a newly added npm tree; asset packaging must change with the frontend.
