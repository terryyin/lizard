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
Status: done
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

### Deployment resume

The owner reopened billing and explicitly requested deployment. Read-only
cloud inspection confirmed `billingEnabled: true`, App Engine `SERVING`,
location `us-central`, default hostname `lizardanalyzer.appspot.com`, existing
application service account `lizardanalyzer@appspot.gserviceaccount.com`, and
default-service traffic `20180602t165847=1.0` before release. Existing versions
and their traffic are preserved until the preview passes.

Slice 1 was accepted on the execution branch at
`f818daf65e5981e4d58589d503b4016644206c62`, suffix base
`e8d62aeaf00d2d0bcb19cc3d11b7ba1843c2f372`. Its runtime code, dependencies and
configuration were unchanged at resume, so the previously accepted full
pytest/make and isolated Python 3.13 proof remain the live-action regression
prerequisite for that candidate. Documentation/planning-only edits do not
alter the deployed upload allowlist or that correspondence.

The previous observer stopped cleanly at the billing handoff, with zero unread
events and the recorded PID exited. The resumed observer uses coordinator
`aino-website-20261027`, yielded cell `93`, terminal session `95319`, directory
`/tmp/dough-ci-501/watch-2xSR4I`, stream PID `43566`. Managed existing-candidate
delivery confirmed `f818daf` was already published and registered it on this
owned stream without a new commit or push. New increments use that accepted
SHA as their previously published base.

Agent `/root/deploy_website` owns slice 2 cloud actions and the operator
release record; root retains plan evidence, independent public-browser
observation, proof acceptance, refactoring, delivery and closure. The complete
G2 receiving obligation was included in its delegation. It must preserve all
old versions, verify a no-promotion preview before switching traffic, and
apply the retained regression prerequisite before live actions.

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
Disposition: proved by slice 2: version `revive-20261027-f818daf` was deployed with no promotion, its HTTPS HTTP/API smoke passed at `2026-10-27T04:51:10Z`, and root independently observed the browser demo complete with actual metrics. Default production traffic remains `20180602t165847=1.0`; promotion is held for the separate HTTPS obligation G3.

### G3. Custom-domain HTTPS must work before promotion
Reported: slice 2 — "Google-managed certificate provisioning returned INTERNAL (13): Error provisioning managed certificate; no certificate is present, and lizard.ws is absent from this account's verified domains. The HTTPS preview works, but production promotion is held because secure: always would redirect visitors to an unavailable custom-domain TLS endpoint."
Story clause: "establish HTTPS and a canonical domain where existing access permits"
Disposition: proved by slice 2: domain ownership was verified, managed certificate `47191116` became active, normal HTTPS succeeded before promotion, and the tested version now receives all production traffic. The public HTTPS page/assets/API smoke and independent browser demo passed. HTTP `www` redirects to matching HTTPS, and the existing apex 301 reaches the chosen `https://www.lizard.ws/` endpoint with final 200 and normal TLS validation. No bare-domain HTTPS endpoint is claimed.

## Slice 2 deployment checkpoint

Fresh source-only reviewer `/root/release_source_commitments` received only
the original source and explicit human amendments, including reopened billing
and renewed deployment authorization. Its independent commitments preserve
the preview-before-traffic sequence, working public HTTPS/page/demo, recovery,
and the access qualification for domain work. No source outcome changed.

| Source trigger and expected result | Actual observation and remaining owner |
| --- | --- |
| Billing/service access is restored and the accepted app is deployed on supported Python 3 | Billing read true; required App Engine, Cloud Build and Artifact Registry APIs enabled. Candidate `f818daf` runtime/configuration/dependencies stayed unchanged, preserving the accepted full regression prerequisite. New version is `SERVING`, `python313`, F1, max instances 2 and max concurrent requests 1. |
| A new version passes preview checks before traffic moves | `gcloud app deploy app.yaml --project=lizardanalyzer --version=revive-20261027-f818daf --no-promote --no-stop-previous-version --quiet` completed after a bucket-scoped source-read IAM repair. All old versions remain. Service routing was observed as `20180602t165847=1.0` after preview proof, closing G2. |
| The deployed page, original assets and real demo work over trusted HTTPS; malformed/oversized input rejects safely and retry works | The explicit HTTPS smoke below exited 0 with homepage, six assets, real `shipping_cost` metrics, 400 invalid JSON, 413 source limit and a matching successful retry. Default certificate/hostname verification was enabled. Root separately observed the deployed page and browser's Working → Analysis complete transition, CCN 3, NLOC 6, 23 function tokens and one parameter. |
| Public HTTP redirects to the chosen HTTPS domain, whose page/demo work | Current `http://www.lizard.ws/` still serves the old site with status 200; custom HTTPS fails its TLS handshake. The mapping has no certificate, and managed provisioning failed. Root prepared Google Search Console DNS ownership verification for the same Google account and asked the owner to add its TXT value at GoDaddy. DNS currently has a different verification value; existing records are preserved. G3 stays open until actual ownership, certificate, redirect and public demo observations pass. |
| Deployment, verification and recovery are documented with actual versions | `docs/website.md` records the real new/old versions, preview URLs, commands, source-read IAM repair, preview observations, empty bounded error log, pending domain gate and rollback to `20180602t165847=1`. Documentation remains an uncommitted in-progress slice 2 result while the public release is unfinished. |

Accepted deployed HTTP proof, from the named execution checkout:

```sh
nix develop -c python .local/release-smoke.py https://revive-20261027-f818daf-dot-lizardanalyzer.appspot.com
```

Terminal result: exit 0 at `2026-10-27T04:51:10.028294+00:00`. Setup is the
already deployed no-promotion version, an explicit HTTPS base, urllib's
default trust/hostname checks, no-cache requests and a harmless built-in
Python example. `main()` asserts homepage content/CSP and six asset status,
MIME and nonempty-body checks; `analyze()` asserts real returned function
fields and `no-store`; the later main assertions observe malformed JSON,
source rejection and a successful equal-result retry. Root inspected these
actual assertions and the deployed browser continuation. The bounded command
`gcloud app logs read --project=lizardanalyzer --service=default
--version=revive-20261027-f818daf --level=error --limit=30` exited 0 with no
entries. Consumer dispositions are in `.local/deployment-consumer-trace.md`;
the changed operational effects reach the deployment/runbook, existing site
links, actual web app/assets and HTTP observations; unrelated workflow text
and parser fixtures do not consume those effects.

Independent browser evidence:
`https://revive-20261027-f818daf-dot-lizardanalyzer.appspot.com/#playground`
and screenshot
`/Users/terryyin/.codex/visualizations/2026/10/27/01a17b69-f903-7980-b23c-385e5333f7b1/lizard-deployed-preview.jpg`.

Resume G3 by observing the new ownership TXT record, completing Google's
ownership verification, enabling managed security on the existing `www`
mapping and checking normal HTTPS trust/hostname acceptance. Only after the
accepted preview and that certificate check may traffic switch to
`revive-20261027-f818daf=1`; then verify the public HTTP redirect and actual
HTTPS homepage/demo. Reuse regression proof while the deployed candidate and
relevant conditions still match; a code/config/dependency change requires the
plan's current regression prerequisite again. Do not weaken HTTPS enforcement
or promote while the custom endpoint is unverified. The bare-domain
DNS/forwarding remains separately owned at GoDaddy and must use observed
records/access rather than inferred changes.

At the ownership-record handoff, the resumed CI observer
`/tmp/dough-ci-501/watch-2xSR4I` stopped with zero unread events; its recorded
PID `43566` exited and yielded cell `93` completed. The stop receipt reports
CI coverage as unobserved, not a completion verdict. The temporary localhost
preview server was stopped because the deployed HTTPS preview is now the
user-facing review surface. The cloud version, old production traffic,
execution worktree/branch, pending Google verification tab, active story and
unfinished release records are preserved. No execution-complete record,
automatic retrospective or story cleanup is appropriate while G3 is open.

### Ownership verification resumed in Safari

The previous stream was already confirmed stopped and its process exited.
The new owned CI stream retains coordinator `aino-website-20261027`, yielded
cell `118`, terminal session `4896`, directory
`/tmp/dough-ci-501/watch-6o0aMO`, and PID `50375`, launched from this execution
checkout for `terryyin/lizard`, branch `codex/revive-lizard-website`.
Managed delivery again confirmed candidate `f818daf` as already published
and reused this stream for observation, with suffix base `e8d62ae`; no new
commit or push occurred. `git diff --check` passed for the pending records.

The owner explicitly requested operation of the logged-in GoDaddy Safari
session. Root added one additional apex TXT record with Google's prepared
verification token, preserving the existing verification TXT and all other
records. GoDaddy showed a successful save and the new row; an authoritative
`dig +short TXT lizard.ws @ns29.domaincontrol.com` returned both tokens.
Google Search Console then showed `Ownership auto verified` for the same
`terry.yinzhe@gmail.com` account, and `gcloud domains list-user-verified
--project=lizardanalyzer --format=json` included `lizard.ws`.

The deployment owner resumed the existing mapping's managed SSL request.
Certificate `47191116` was created with status `PENDING`; mapping security is
`AUTOMATIC`. A normal TLS probe still failed, so production traffic remains
on the previous version. G3 remains open until certificate acceptance and
the public release checks pass.

The certificate subsequently changed to `OK`, with renewal time
`2026-10-27T05:45:41.666467148Z`, and the mapping selected it as active
`certificateId: 47191116`. The first immediate normal TLS probe still
failed; certificate status alone does not establish public HTTPS readiness.
Allow bounded edge propagation and retain the normal TLS gate before traffic
or the prepared apex HTTPS redirect changes.

GoDaddy's existing apex forwarding rule is a permanent 301 to
`http://www.lizard.ws`; its two apex A records are managed by that service.
The forwarding UI says editing a rule also provisions an apex certificate.
Root prepared the same destination with HTTPS, preserving permanent 301,
but has not saved it while the `www` HTTPS endpoint is unavailable.
Do not replace the service-managed A records or unrelated email records.

Ownership evidence screenshots are retained under the conversation's
visualization directory as `lizard-dns-record.jpg` and
`lizard-google-verified.jpg`.

At the bounded `2026-10-27T05:52:00Z` checkpoint, normal curl still failed
with exit 35 (`SSL_ERROR_SYSCALL`); Python's default-verified urllib had
also failed with `SSLEOFError`, and root's independent IAB navigation returned
`ERR_CONNECTION_CLOSED`. The certificate remains issued/attached (`OK`),
but the public TLS handshake remains unobserved. The deployment owner
confirmed service traffic still `20180602t165847=1.0`, updated the maintained
release record, and returned with no command running. No promotion occurred.

Root cancelled the unsaved GoDaddy forwarding form and read back the existing
permanent 301 destination `http://www.lizard.ws`. The saved verification TXT
remains in DNS. A future resume must reopen the edit rather than assume a
pending form. After actual normal `www` HTTPS succeeds, apply the HTTPS
destination, promote the tested version, verify canonical HTTP/HTTPS journeys
and the actual public demo, and inspect bounded version error logs. Keep the
rollback version. Public checks, slice 2 refactoring/delivery, completion CI,
retrospective and wrap-up remain unfinished.

At this external-dependency handoff the exact observer
`/tmp/dough-ci-501/watch-6o0aMO` stopped cleanly with zero unread events.
PID `50375` exited and yielded cell `118` completed. Its stop receipt reports
coverage ended with `pendingCi: unobserved`; this is not a CI completion
verdict. Candidate `f818daf`, the branch/worktree, unfinished plan and release
record, deployed preview and verified Search Console property are preserved.
No code/configuration/dependency changed, no new commit or push occurred,
and slice 2 remains planned with G3 returned.

### Public release acceptance

On resume, normal HTTPS returned 200. The deployment owner rechecked the
active certificate, unchanged candidate `f818daf` and previous traffic before
executing:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=revive-20261027-f818daf=1 --quiet
nix develop -c python .local/release-smoke.py https://www.lizard.ws
gcloud app logs read --project=lizardanalyzer --service=default --version=revive-20261027-f818daf --level=error --limit=30
```

Promotion exited 0 and the service reports `revive-20261027-f818daf=1.0`.
All four previous versions remain `SERVING` with zero traffic. An immediate
post-switch content assertion saw the old homepage during propagation;
subsequent plain/cache-busted observations saw the new page, and the full
public smoke exited 0 at `2026-10-27T06:07:09.320231Z`. The already inspected
smoke assertions now reached the public domain: trusted HTTPS, homepage/CSP,
six nonempty assets, actual `shipping_cost` metrics (CCN 3, NLOC 6, one
parameter, 23 function tokens), malformed JSON 400, source-limit 413,
successful equal-result retry and POST `no-store`. The bounded error log
exited 0 with no entries. Accepted local/runtime regression proof still
applies because code, configuration and dependencies did not change.

At `2026-10-27T06:07:41.992855Z`, urllib observations saw HTTP `www` page
and original small-logo paths redirect 302 to corresponding HTTPS paths,
preserving path/query and finishing 200. Root independently ran:

```sh
curl --silent --show-error --max-time 20 --location --output /dev/null --write-out 'status=%{http_code} final=%{url_effective} tls_verify=%{ssl_verify_result}\n' http://lizard.ws/
```

It exited 0 with `status=200 final=https://www.lizard.ws/ tls_verify=0`.
The observed existing GoDaddy 301 to HTTP `www`, followed by App Engine's
302 to HTTPS, therefore supplies the canonical HTTP journey. No optional
one-hop GoDaddy edit was saved; Safari's reopened page went blank after
hydration. The chosen HTTPS endpoint is `www`; bare-domain HTTPS is not
claimed. This does not change the source's chosen-canonical-domain promise.

Root's independent public-browser observation at
`https://www.lizard.ws/#playground` saw `Working` then `Analysis complete`,
one real `shipping_cost` function, CCN 3, NLOC 6, 23 function tokens and one
parameter. Evidence is `lizard-public-release.jpg` in the conversation's
visualization directory. Preview proof and the retained full source-only
review still apply; this closes G3 and the public release promises in the
acceptance table. `docs/website.md` now carries actual release, verification
and rollback facts. The deployment consumer trace remains applicable, with
its public-host follow-up satisfied by these observations.

The previous observer was confirmed stopped and exited. The current stream
uses coordinator `aino-website-20261027`, yielded cell `152`, terminal session
`34850`, directory `/tmp/dough-ci-501/watch-CBP4PC`, PID `62446`, for this
checkout and execution branch. Keep it for slice delivery and completion.

The owner's later request separately queues three future website stories
(React/package management/conditional Cucumber-Playwright CI, the selected
first design, and `web-MAJOR.MINOR.PATCH` Actions deployment). Those backlog
changes do not authorize implementing them in this release or alter this
completed release's source contract. They remain queued and unrefined.

Fresh independent agent `/root/refactor_release_records` returned
`## REFACTOR COMPLETE`. It changed only `docs/website.md` so the reusable
runbook explicitly verifies normal public HTTPS before traffic promotion;
public page/demo/redirect checks remain after promotion. Root inspected that
ordering and the maintained actual release/rollback facts. Whitespace check
passed; the operator document is 248 lines. The scoped generated trace
`.local/website-release-refactor-consumer-trace.md` identifies this plan as
the guidance consumer, with reused proof, and no runtime/test consumers.
No source, configuration, dependency, cloud or browser change invalidated the
accepted regression or public proof, so no test/probe repetition was needed.
The longer temporary plan stays intact for retrospective/recovery and is
owned by ordinary wrap-up cleanup. No generation, selective formatting or
separate project summary operation applies to the directly maintained
Markdown release records.

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
