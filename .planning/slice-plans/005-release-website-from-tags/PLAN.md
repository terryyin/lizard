# Release the website from version tags

**Identity:** SEED-012#release-website-from-tags
**Source:** [Refined story](../../seeds/SEED-012-website-evolution.md#release-website-from-tags)

## Goal and scope

A maintainer pushes a website version tag and Actions validates, builds,
previews and promotes that exact source to the existing public website, with
an inspectable result and a usable recovery path. The source story owns tag
syntax, mapping, target, failure behavior and exclusions. This plan adds no
hosting migration, package release changes or automatic rollback.

Preparation is not execution authority. No release tag, IAM change, cloud
deployment or traffic change was made during preparation.

## Current decisions and reuse

- Reuse `docs/website.md`'s `lizardanalyzer` / `default` deployment and traffic
  commands, `.gcloudignore` upload boundary, `app.yaml`, and Python runtime.
  There is no release orchestrator in `scripts/`, tests or Actions to extend.
  Add the smallest website release entry point needed for this flow, keeping
  validation/mapping in one place rather than repeating rules in YAML/shell.
- `.github/workflows/release.yml` publishes analyzer distributions to PyPI;
  its responsibility is different. Use a separate website workflow. Its
  trigger and validation must exclude ordinary pushes and package tags.
- Reuse the web job's declared Node/npm versions, `npm ci`, build and browser
  scenarios. Release validation is unconditional for a valid tag; do not use
  `scripts/web_changes.py` to skip it. If setup is extracted to share with
  branch CI, preserve its existing path-gated behavior and failure artifacts.
  Do not create a general deployment framework.
- Checkout the event commit, including annotated-tag peeling, rather than
  today's branch head or a re-resolved mutable tag. Authenticate after checks;
  consume the tested bundle without a later rebuild. Record source identity
  in deployment metadata so retries can establish what a version contains.
- Use WIF through a dedicated deployment service account, scoped to repository
  identity, workflow and website tag refs; keep the runtime service account's
  role separate. Resolve actual IAM needs in slice 1. No stored key fallback.
- Capture the entire prior traffic split before mutation. Preview deploy uses
  `--no-promote --no-stop-previous-version`. A single concurrency group covers
  the release mutation sequence with `cancel-in-progress: false`; document
  GitHub's pending-run replacement behavior and maintainer reruns. Releases
  are not ordered by semantic version. Manual rollback must not race a release.
- Accepted ADR-0000, **Use ADRs** (`docs/adrs/0000-use-adrs-accepted.md`), keeps
  durable architecture decisions human-owned. ADR-0001, **Ubiquitous language**
  (`docs/adrs/0001-ubiquitous-language-accepted.md`), governs CCN/NLOC names in
  analysis checks. No conflict or new cross-product boundary is identified;
  no North Star file or ADR change is warranted.

## Decisive premises and observations

Observed on 2026-10-27 in
`/Users/terryyin/git/lizard/.worktrees/deploy-website-releases-from-version-tags`,
product revision `ca7f5ac105d0f3655bf3c5ccf8aa855a52688a42`.
Commands below used `nix develop -c` (shell command groups via `sh -c`).

| Premise / consuming operation | Observation and result | Coverage / remaining work |
| --- | --- | --- |
| The lockfile builds a deployable frontend | `npm --prefix website/frontend ci` then `npm --prefix website/frontend run build`: success; manifest, JS and CSS generated | Actual build, not just command presence |
| The uploader includes the built assets | `gcloud meta list-files-for-upload`: manifest and both named asset files listed; frontend sources/node_modules absent | Local upload selection only; remote build/deploy is slice 1 |
| Existing browser proof exercises real analysis | `npm --prefix website/frontend run test:e2e`: 16 scenarios / 134 steps passed in 25.5 s | Local Flask/Chromium; not a hosted preview |
| That proof detects a broken analysis path | `E2E_BLOCK_ANALYSIS=1 npm --prefix website/frontend run test:e2e`: exit 1, 11 failed scenarios, including Python sample metrics; 5 passed | Expected diagnostic failure, no source edit; per-run environment removed automatically |
| Current target and recovery state can be read | `gcloud app services describe default --project=lizardanalyzer --format=json`: `revive-20261027-f818daf` has 100% traffic | Local authenticated read, not Actions identity or deploy authority |
| Actions credential route exists | `gh api repos/terryyin/lizard/actions/variables --jq ".variables \u007c map({name,value})"` and `gcloud iam workload-identity-pools list --project=lizardanalyzer --location=global --format=json`: both empty arrays | Route is absent, not presumed working; slice 1 owns setup and consuming deployment probe |

The browser dry run also selected 16 scenarios, but the actual runs above own
proof. No scratch source changes remain. Nix emitted an existing
`platformdirs`/`virtualenv` dependency warning; build and browser operations
completed. Run shell initialization sequentially in this worktree: parallel
Nix invocations both recreate/install the local environment.

Platform evidence consulted during preparation:
[App Engine version naming](https://docs.cloud.google.com/appengine/docs/standard/reference/app-yaml),
[Google Actions WIF authentication](https://github.com/google-github-actions/auth),
[GitHub concurrency](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-workflow-concurrency).
The 63-character combined name limit leaves 32 for the mapped version with
the existing project/service. WIF/IAM propagation and actual hosted deployment
remain credentialed observations bounded by slice 1, not local-test claims.

## Ordered slices

### 1. Establish an authenticated Actions preview without changing public traffic
Type: Behavior
Status: planned
Proof: A bounded hosted probe deploys this checkout's tested source to a unique
non-serving version, checks its preview and records unchanged service traffic.

Behavior: Given the existing app and a configured repository-scoped deployment
identity, running the preview probe authenticates from Actions, uploads the
generated bundle, deploys Python 3.13 and serves a real analysis result while
the public traffic split remains unchanged.

Resolve/configure the pool/provider, deployment account, repository settings
and minimum App Engine/build/storage/runtime-account permissions; inspect
existing bindings before additions. Document exact configuration and ownership
in `docs/website.md`. Exercise the real Actions OIDC route with a temporary
diagnostic workflow/ref or equivalent bounded hosted probe, explicitly unable
to promote. Remove temporary diagnostic workflow/trust exceptions afterwards.
Check normal TLS, homepage, manifest assets and `POST /analyze` on the returned
version URL, then re-read traffic. Record run URL, source SHA, version and result
in this plan. Do not infer readiness of this route from the local gcloud login.

This is the permitted early infrastructure probe: the cheap prefix was observed
above; authenticated deployment is the first unavailable consuming operation.
Execution authority and actual cloud privileges still govern setup. If an
owner-held IAM/GitHub action is unavailable, give Terry the exact operation and
resume condition. If authentication, source upload, remote build or preview
fails, stop dependent slices and revise their assumptions. Do not promote or
declare a simulated deploy sufficient. Leave existing versions running.

### 2. Turn a valid website tag into a tested, inspectable preview candidate
Type: Behavior
Status: planned
Proof: Real tag-triggered Actions runs show the candidate SHA, validation,
generated upload, preview version/URL and unchanged public traffic. Focused
tests exercise the release CLI's tag contract; existing browser/API tests prove
the application. Invalid inputs exit before cloud authentication/deployment.

Behavior: Given slice 1's working route, a valid website tag runs the required
checks and deploys its exact tested source to a preview. Invalid website tags
fail validation, and unrelated tags/branches/PRs never deploy the website.

Implement the separate tag workflow and one tag-validation/mapping boundary;
include zero, multi-digit versions, malformed forms and the length boundary.
Pass tag text as data, not interpolated shell code. Retain checkout SHA, tagged
version, upload manifest and test reports. Run Python tests/checks plus npm build
and browser scenarios for the candidate irrespective of the branch change
selector. Preserve PyPI and branch-CI behavior. Check existing version/source
and serving state before any redeploy; ambiguous or serving mismatches fail
with guidance, while a matching serving result can be verified without overwrite.

Use harmless invalid-tag runs to observe a failing validation job and skipped
authentication/deployment. Compare a valid run's event/checkout/source metadata;
test annotated-tag handling as well as a lightweight tag. Do not create a
broken production release just to test failure. Keep any necessary probe tag
identities/run links in execution evidence. Document tagging and retry behavior
with this capability.

Accepted interim behavior: valid tags produce preview candidates only, clearly
reported as not promoted; slice 3 replaces this with automatic publication.
This is a safe stopping point and does not complete the story.

### 3. Promote verified candidates and make failed releases recoverable
Type: Behavior
Status: planned
Proof: A real successful tag run reaches the public HTTPS homepage/assets and
sample analysis; the reported deployed source/version matches the event.
Observe preview failure blocking promotion in a non-promoting probe, and
exercise the documented traffic restore against retained known-good versions
under the live-action authority for execution. Read back the final split and
public result. Local HTTP fixtures prove smoke-check rejection without mocking
the analyzer or pretending to exercise Google Cloud.

Behavior: Given a validated preview and a saved prior split, successful preview
checks promote the candidate and verify `https://www.lizard.ws/`. Failure before
promotion preserves the old split. A failure during/after promotion fails the
run, reports current/unknown traffic honestly, and provides the saved split's
recovery command; the maintainer can restore it without rebuilding.

Wire bounded preview/public HTTP checks into the existing flow, using normal
TLS and real sample metrics (CCN 3, NLOC 6, one parameter for the documented
Python example). Include assets, original logo paths and HTTP-to-HTTPS redirect.
Verify target traffic/version separately: a working old homepage is not proof
that the new source was promoted. Publish an always-run Actions summary with
tag, SHA, target/version, prior split, URLs, stage/result and recovery command;
do not label preview-only or failed public verification successful. Ensure
credential failures still yield an intelligible result without secrets.

Complete `docs/website.md` with WIF setup, required validation, tag publishing,
Actions inspection, failure phases, concurrency/pending-run behavior and
manual traffic restoration. Retain old versions; no automatic cleanup or
rollback. If hosted acceptance cannot be observed, keep that proof outstanding
with its owner instead of treating unit/workflow checks as completion.

## Verification and delivery gates

Use high-level CLI inputs/outputs for tag logic and real HTTP for verification;
do not mock parsing or cloud deployment as end-to-end proof. Inspect tests and
all consumers before factoring shared validation, including browser step
definitions, web-change selection and both workflows. Use the project's
consumer-trace procedure during implementation. Any shared setup extraction
must preserve normal branch CI selection and browser report collection.

`AGENTS.md` requires `nix develop -c python -m pytest` before completion and
`nix develop -c make` for combined tests/style. `docs/website.md` additionally
requires npm install/build/browser checks before deploy. Apply focused proof,
post-change refactoring, commit/delivery and asynchronous CI repair through
the execution skill; no execution begins from this plan alone. Before expensive
hosted/live proof, inspect the actual runner/setup and use one release at a time.
Retain command, revision, run URL, observation and result for accepted evidence.

## Plan assessment

Slice-plan refinement was not needed: the sequence separates the credentialed
integration probe, tag-to-preview candidate, and traffic/recovery outcome with
one common release model. No independent feature or speculative framework is
added. Each slice includes its proof and documentation. No numeric slice limit
is supplied; boundedness is judged by these outcome/proof loops.

No remaining slice-specific concern was identified outside the explicitly
bounded early probe. The absent WIF route is not assumed solved; its failed
probe stops both dependent slices. There is no pre-Take human decision: live
setup and release observations remain subject to execution authority when
execution is requested. No story implementation or completion is claimed.
