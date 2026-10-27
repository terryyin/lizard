# Plan: Revive the Lizard homepage and online demo

**Identity:** SEED-011#revive-lizard-website

**Source:** [Revive the Lizard homepage and online demo](../../seeds/SEED-011-lizard-website-revival.md#revive-lizard-website)

## Goal and scope

Restore lizard.ws as a modern project homepage with a working, bounded online
demo on supported Python 3 in the existing App Engine project. Include the
whole visitor journey, compatibility at `/analyze` and existing logo URLs,
deployment configuration, operator documentation, preview proof, and public
release. Keep analysis semantics in Lizard's existing API. Exclude accounts,
stored source or reports, repository ingestion, parser work, and replatforming.

The owner has authorized implementation and deployment. Repository tooling
uses `nix develop -c`; git and gcloud are explicitly exempt. `AGENTS.md`
requires full `nix develop -c python -m pytest` before completion; run
`nix develop -c make` as the repository's combined test/style check.

## Existing solutions and architecture

- `index.py` already exposes a Flask application and homepage; extend or
  consolidate that application rather than creating a second web backend.
- `website/__init__.py` owns the legacy `/analyze` adapter, while `lizard`
  exposes `analyze_file.analyze_source_code`. Reuse that public analysis
  boundary; replace the legacy webapp2 adapter with the supported web app.
- `index.html` and `website/static/controllers.js` contain the current
  AngularJS/Bower interface. Replace the interface with local, minimal
  browser assets; preserve `website/static/img/logo.png`,
  `logo-small.png`, and favicon paths referenced outside the app.
- Keep one explicit web response representation; serialize metric properties
  such as parameter count deliberately instead of relying on object
  `__dict__`. No additional parsing implementation belongs in the website.
- Accepted ADR-0000 (`docs/adrs/0000-use-adrs-accepted.md`) reserves durable
  architecture approval for humans. The owner approved keeping App Engine
  and using Flask/Python 3. No current ADR specifies another hosting stack.
  ADR-0001 has conflicting index/in-file status and is not relied on here;
  its status is outside this story.

No new North Star topic or preparatory Structure slice is warranted: the
existing app and public analysis API already supply the needed boundaries.

## Outside-in proof

| Promise | Owning slice | Observable proof |
| --- | --- | --- |
| Homepage explains Lizard and links installation, usage, documentation, GitHub and PyPI | 1 | Flask HTTP test checks the rendered page and local assets; browser observation checks mobile and desktop layout and keyboard use |
| Pasted code and examples produce visible function metrics | 1 | HTTP request tests call `/analyze` with representative language fixtures and assert actual metrics; browser runs an example and observes the rendered result |
| Invalid, oversized and failed requests have clear safe behavior | 1 | Request tests exercise malformed JSON, invalid fields and request limits, asserting client errors and absence of source/internal details; browser observes error/recovery states |
| Existing JSON route and logo assets remain usable | 1 | `/analyze` response assertions and HTTP checks for published logo/favicon URLs |
| Supported runtime can install and run the app without changing core support claims | 1 | Isolated install under selected production Python, web HTTP tests, plus full repository suite and combined checks |
| Preview works before production traffic moves | 2 | Deploy an identified version without promotion; fetch its page/assets and call `/analyze` using harmless sample code; inspect error logs |
| Public domain, HTTPS and recovery work | 2 | Observe public redirects, certificate acceptance, homepage and sample analysis after traffic switch; record deployed version and prior version recovery command in maintained operator documentation |

## Decisive premises and probes

Preparation workspace: `/private/tmp/lizard-website-revival`, created for
this story on `codex/revive-lizard-website` from freshly fetched
`origin/master` at `62b6d76dd2c02cdd084f1b6f9fefff449eced3e8`.
Originating/integration checkout: `/Users/terryyin/git/lizard`.
Publication target: `origin`, trunk `refs/heads/master`; execution defaults
to Story Branch Mode, publishing code increments to the execution branch.

Consuming analysis-API observation in this workspace at the base above:
`nix develop -c python -c 'from lizard import analyze_file; result=analyze_file.analyze_source_code("example.py", "def classify(value):\n    if value > 0:\n        return 1\n    return 0\n"); print({"filename": result.filename, "functions": [(f.name, f.cyclomatic_complexity, f.nloc, f.parameter_count) for f in result.function_list]}); assert len(result.function_list)==1; assert result.function_list[0].cyclomatic_complexity==2; assert result.function_list[0].parameter_count==1'`
passed and returned `example.py`, `classify`, CCN 2, NLOC 4 and parameter
count 1. This reaches the exact public operation the web adapter will consume.
The worktree's Nix setup created its own ignored `.venv`; it did not reuse
another checkout's mutable dependencies. New HTTP and browser proof is
implementation-owned; no existing web regression suite is assumed to cover it.

Credentialed production-runtime installation and cloud writes require an
execution probe: choose a currently supported Python runtime, install the
declared dependencies in isolation, and run the web app's tests before
committing to that runtime. A failure stops deployment and changes the
runtime/dependency choice; it does not alter core Python support claims.

Cloud deployment is a bounded external probe in slice 2. Before any deploy,
read project billing and App Engine app/version/domain state with explicit
`--project=lizardanalyzer`. Billing was observed disabled and its sole account
closed, so no deploy outcome is assumed. Once the owner restores billing,
verify service access, current traffic and old version, default hostname,
certificate state, and DNS before writes. Failure preserves the tested local
result and stops the dependent cloud release. The owner controls any
registrar-only DNS work. Never replace an unknown DNS record or redirect an
unverified hostname.

## Slices

### 1. Visitors can understand Lizard and analyze a source sample
Type: Behavior
Status: done
Proof: app HTTP tests and browser observation of the complete homepage/demo
journey; isolated production-Python dependency install and web tests; full
repository suite and combined checks before delivery.

Behavior: a visitor opens the site, reads the introduction, loads or pastes a
sample, selects its language, and runs analysis → the page shows actual
function metrics and an accessible completion status. Invalid or oversized
input and service failure show clear recoverable errors. Existing `/analyze`
callers and logo asset URLs remain usable.

Deliver the coherent app, interface, production configuration and operator
documentation together. Preserve the magenta lizard logo; use warm off-white,
near-black text, a magenta accent and readable monospace code/results.
Explain metrics and that code is sent to the server but not saved; recommend
the local CLI for private code. Derive language handling from the analyzer's
existing registration where suitable. Keep result rendering as text, serve
assets locally, and avoid logging source bodies. Proportionate bounds may use
a 64 KiB source cap, bounded request size, a Gunicorn sync worker timeout, and
limited App Engine instances. Web-only dependencies must not accidentally
raise the core CLI's supported Python floor. Consolidate any retained
`/analyse` form compatibility through the same analysis boundary instead of
maintaining duplicate application logic.

This is one cohesive outside-in proof loop. Local correctness is independently
reviewable while closed billing prevents a cloud preview.

### 2. The verified site is served securely on lizard.ws
Type: Behavior
Status: planned
Proof: explicit-project cloud inspection, no-promotion preview deployment,
HTTP/browser sample analysis, observed production traffic/domain/HTTPS
behavior, and documented prior-version recovery.

Behavior: the tested app is ready locally and billing/service access is
restored → deploy a uniquely identified version with no public promotion,
verify page/assets and sample analysis, then direct production traffic to
that version. Establish managed HTTPS and canonical redirects using observed
domain mappings/DNS. Verify the public hostname and demo after the switch,
check error logs and retain the concrete rollback command. Do not claim this
slice complete while billing, DNS ownership or certificate provisioning leaves
the public journey unobserved.

## Current decisions

- Keep existing GCP/App Engine hosting and use Flask on supported Python 3.
- Reuse the existing public analyzer boundary and preserve `/analyze`.
- No submitted source persistence, request-body logging, external browser
  libraries, accounts or database are needed for the selected outcome.
- Run full local proof once at slice 1 completion; broaden or repeat only for
  changed implementation or a concrete integration failure.
- Independent refactoring and ordinary execution delivery apply at each
  slice boundary. Manual/browser observations are required by this plan.

## Execution context

- Mode: Story Branch Mode; execution checkout
  `/private/tmp/lizard-website-revival`, branch `codex/revive-lizard-website`.
- Admission published `e8d62aeaf00d2d0bcb19cc3d11b7ba1843c2f372` on
  `origin/master` and the execution branch, after reconciling onto
  `15d97456f4a340bf14500d3021f191d22fab954a`. Agent: Aino-chan;
  publisher: `codex-lizard-website-20261027`. The preparation-created worktree
  remains owned by this story. Default-checkout maintenance was deferred for
  pending admission drafts; they were not included as unrelated changes.
- Post-admission setup: `nix develop -c python -m pytest
  test/test_analyzer.py -q` passed, 22 tests, in this execution checkout.
- CI source: GitHub Actions `python-app.yml`, verified active workflow
  `38306292`, display name `Python application`, with branch push coverage.
- Observer: coordinator `aino-website-20261027`, yielded cell `44`, terminal
  session `20912`, directory `/tmp/dough-ci-501/watch-omNzoN`, stream PID
  `98424`. Request matches this checkout, `terryyin/lizard` and
  `codex/revive-lizard-website`; the installed retained-stream classifier
  verified this coordinator's ownership and live state. Nix startup delayed
  initial metadata, so the exact request and owner were recovered before any
  code increment. The trunk admission is unobserved by this branch observer.
- Independent source-only review: fresh agent `/root/source_commitments`,
  launched without inherited conversation, received only the original story
  clauses and explicit human amendments. Its returned individual commitments,
  qualifications, journey examples and exclusions are retained in the execution
  conversation for comparison with product proof at acceptance.
- Root owns the required browser observation, independent refactoring, full
  pytest and combined checks after implementation; the implementation agent
  owns focused HTTP tests and the isolated production-runtime probe.

## Learnings

- Existing GCP project is active, but billing is disabled and the only linked
  billing account is closed. The owner has been asked to restore it.
- Existing `www.lizard.ws` points to Google's host; apex DNS differs and must
  be inspected with its ownership before release changes.

## Story obligations

### G1. Validation preserves a contradictory completed report
Reported: slice 1 — "After a successful browser report, submitting whitespace-only code shows the input error but retains the old metrics and the Analysis complete badge."
Story clause: "report input and service errors clearly"
Disposition: proved by slice 1: after reloading the corrected `website/static/demo.js`, browser observation of successful analysis followed by whitespace and 65,537 ASCII characters showed `Try again`, the specific validation error, hidden `#report-output`, and enabled retry. Loading the example and retrying returned the actual Python metrics again. The shared `showError` path now clears the stale report.

### G2. Public release still needs cloud observations
Reported: slice 1 — "Public-host verification remains owned by slice 2. Billing is still closed; no cloud write or public preview has been performed."
Story clause: "Publish a verified preview before switching production traffic"
Disposition: receiving slice 2

## Slice 1 acceptance evidence

The source-only reviewer `/root/source_commitments` returned the original
commitments independently of the plan and implementation. The coordinator
compared those clauses and qualifications with the following actual paths and
observations. These establish semantic acceptance; independent refactoring and
full local checks remain separate delivery gates.

| Source trigger and expected result | Actual observation and owner |
| --- | --- |
| A visitor opens the homepage and understands Lizard, obtains installation and usage commands, and reaches documentation, GitHub and PyPI | Browser rendered the purpose, `pip install lizard`, `lizard src/` and threshold example. `test_homepage_and_existing_wsgi_import` asserts those commands/URLs. The exact three external destinations were fetched successfully. |
| A phone visitor has a usable, accessible homepage and demo; keyboard users can operate controls | Browser viewport 390 by 844 showed stacked editor/results and readable metrics, with document width 375 within viewport 390. The JavaScript example was loaded and analyzed using Tab/Return. Labels, heading structure, live status and busy state were inspected. Temporary viewport and offline overrides were reset. |
| A visitor loads an example or pastes source, chooses language or supplies filename, runs analysis and receives names, NLOC, CCN, token and parameter counts plus clear completion | Browser Python example returned `shipping_cost`, CCN 3, NLOC 6, parameter count 1, with file/function token counts. JavaScript example returned `shippingCost`, CCN 3, NLOC 5 and parameter count 1. A separately pasted Python `classify` function returned CCN 2, NLOC 4 and parameter count 1. HTTP tests exercise Python, C++ and JavaScript metrics and filename input through the real public analyzer. `showResult` uses text nodes for names and all five displayed fields. |
| Input is bounded and malformed/oversized input receives a clear client error without source or internal details | HTTP assertions cover invalid JSON, content type, payload/filename/Unicode fields, UTF-8 source/body limits, exact-limit acceptance and retry. Browser whitespace/oversize rejection and successful retry close G1. |
| Service failure reports a clear error, preserves usability, and does not expose source or internal exception details | Browser offline observation showed the generic analyzer-unreachable message, `Try again`, hidden results and enabled controls; retry after restoring connectivity succeeded. The internal exception response was inspected as a fixed generic 503, without exception text or logging. Internal parser exceptions were not deliberately injected. |
| Analysis uses Lizard's API, parser behavior stays unchanged, submitted source is neither persisted nor logged | `analysis_result` calls the existing `analyze_file.analyze_source_code` and returns an explicit metric representation. No analyzer/parser files changed. The adapter has no persistence or source logging; HTTP assertions show private marker text absent from responses and captured output. POST responses are `no-store`. |
| Existing `/analyze` JSON callers and published logos remain usable | HTTP tests assert the JSON route/default language, metric fields and parameter property, old WSGI import, old logo URLs and both favicon URLs. Retained `/analyse` form input uses the same adapter and returns JSON; its removed old form page has no remaining application consumer. |
| The app uses supported Python 3 and the accepted Flask/App Engine stack while bounding analysis time | Isolated Python 3.13.7 installed the exact production requirements and passed 27 HTTP tests; Nix Python 3.9 passed the same 27. Gunicorn configuration/import passed. The production entrypoint specifies one sync worker and a 15-second timeout; browser abort is 20 seconds. A deliberate worker timeout was not triggered; deployed-runtime verification remains slice 2. Core package dependencies/support claims are unchanged. |
| Preview precedes traffic change, HTTP reaches canonical HTTPS, and the public page/demo work where access permits | No cloud action performed during slice 1. Billing/service access and any registrar-only changes remain the story's explicit external conditions; slice 2 owns the preview, certificate, redirect, traffic and public-demo observations under G2. |
| Deployment, verification and recovery are documented, including how to return traffic to the prior version | `docs/website.md` names explicit-project inspection, full regression prerequisites, no-promotion deployment, preview/API checks, traffic switch, domain checks and recovery commands. Slice 2 will record the actual old/new versions and observed URLs. |
| Deferred accounts, stored reports, cloning/uploads, billing dashboards, parser changes, larger service and replatforming remain outside this outcome | The diff adds only the web adapter/interface, web dependencies/configuration, HTTP tests and operator instructions. None of the deferred capabilities is introduced. |

Focused proof accepted from the implementation owner (terminal results, exact
checkout and actual assertions inspected):

```sh
nix develop -c sh -c '.local/web-python313/bin/python -I -m pytest test/test_website.py -q --tb=short && python -m pytest test/test_website.py -q --tb=short && .local/web-python313/bin/python -I -m gunicorn --check-config index:app'
```

Result: 27 HTTP tests passed independently under each runtime; Gunicorn check
passed. Setup is `app.test_client()` with the real analyzer, plus a separately
created Python 3.13 environment with production requirements and pytest.
`gcloud meta list-files-for-upload` listed only runtime code, dependencies,
configuration, templates and public assets. The generated consumer trace is
`.local/website-consumer-trace.md`; changed web callers, dependency installers,
public website/asset references and actual HTTP assertions were inspected;
workflow and parser-fixture homonyms do not consume the changed web behavior.
Browser evidence is retained in the execution conversation and screenshots
`/private/tmp/lizard-website-mobile.jpg`,
`/private/tmp/lizard-website-desktop.jpg`, and the user-facing artifact
`/Users/terryyin/.codex/visualizations/2026/10/27/01a17b69-f903-7980-b23c-385e5333f7b1/lizard-homepage.jpg`.

### Independent refactoring and delivery gates

Fresh independent agent `/root/refactor_website` returned
`## REFACTOR COMPLETE`. It consolidated the server-owned source limit/message
through rendered form data and reused the textarea's default Python example.
Only `index.py`, `templates/index.html`, `website/static/demo.js` and the
homepage HTTP assertion changed during refactoring. The coordinator inspected
those boundaries and the actual updated assertion. Both 27-test HTTP runs
passed again, using:

```sh
nix develop -c sh -c '.local/web-python313/bin/python -I -m pytest test/test_website.py -q --tb=short && python -m pytest test/test_website.py -q --tb=short'
```

The debug-off preview server was stopped and restarted from the same checkout
on port 8085. Browser reload then verified default Python analysis, switching
to JavaScript/Load example and back to Python/Load example, the rendered
64-KiB limit's rejection of 65,537 bytes with hidden stale results and enabled
retry, and successful recovery. Python returned `shipping_cost`, CCN 3,
NLOC 6, 23 function tokens and parameter count 1. Other result rendering,
service-failure recovery, assets, keyboard/mobile layout, deployment settings,
privacy paths and Gunicorn import/configuration remained unchanged, so their
accepted evidence was reused. Refactor consumer trace:
`.local/website-refactor-consumer-trace.md`; the coordinator retained the
unchanged relevant consumer dispositions and inspected the changed HTTP/UI
paths rather than treating unrelated workflow homonyms as web consumers.

Final coordinator proof on the reviewed candidate:

- `nix develop -c python -m pytest`: 1,692 passed, 7 skipped, exit 0.
- `nix develop -c make`: combined test/style checks exited 0. Its pylint target
  reports existing diagnostics with the repository's `--exit-zero` contract;
  this is not a claim of zero pylint diagnostics.
- `git diff --check`: passed.

No generator or selective formatter applies to these directly maintained web
sources. The repository requires its ordinary test/style commands above;
there is no configured mutating commit hook or separate summary generator.
Production-runtime and public-host release observations remain slice 2.

Latest cloud checkpoint: `gcloud billing projects describe lizardanalyzer`
still returned `billingEnabled: false`. The correct-account billing console
shows an enabled Reopen billing account button and only `lizardanalyzer` as
a linked project. The owner was directed to that exact control. No cloud
deployment or traffic change was attempted while billing remained disabled.

## Plan assessment

Refinement was not needed: two Behavior slices retain the complete visitor
journey and isolate credentialed production release without splitting by
technical layer. No unresolved local scope, proof-boundary, or design concern
remains. Production-runtime installation and cloud release are explicitly
bounded probes; failed probes stop dependent release work. No assessment or
production proof is inferred from the existence of configuration files.
