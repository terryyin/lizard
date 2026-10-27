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
Status: planned
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

## Learnings

- Existing GCP project is active, but billing is disabled and the only linked
  billing account is closed. The owner has been asked to restore it.
- Existing `www.lizard.ws` points to Google's host; apex DNS differs and must
  be inspected with its ownership before release changes.

## Plan assessment

Refinement was not needed: two Behavior slices retain the complete visitor
journey and isolate credentialed production release without splitting by
technical layer. No unresolved local scope, proof-boundary, or design concern
remains. Production-runtime installation and cloud release are explicitly
bounded probes; failed probes stop dependent release work. No assessment or
production proof is inferred from the existence of configuration files.
