# Release the website from version tags

**Identity:** SEED-012#release-website-from-tags
**Source:** [Refined story](../../seeds/SEED-012-website-evolution.md#release-website-from-tags)

## Goal and scope

A maintainer's website tag validates, builds, previews and promotes its exact
source to the existing public service, with inspectable results and recovery.
The source story owns tag syntax, target, failures and exclusions. No hosting
migration, package release change, automatic rollback or numerical ordering.
Preparation made no tag, IAM, deployment or traffic change; execution is authorized.

## Current decisions and reuse

- Reuse `app.yaml`, Python 3.13, `.gcloudignore` and the existing
  `lizardanalyzer` / `default` service and release commands. No release
  orchestrator exists; add one small website entry point, no general framework.
- Keep PyPI's `release.yml` independent. Use `website-release.yml`; validate
  before authentication and exclude ordinary pushes/package tags from deployment.
- Reuse declared Node/npm, `npm ci`, build and browser scenarios. Release
  checks are unconditional; never use `web_changes.py` to skip them. Preserve
  branch CI's path gating and failure artifacts if setup is shared.
- Checkout the event commit, peeling annotated tags; never re-resolve a mutable
  tag or use today's branch head. Authenticate after checks, deploy the tested
  bundle without rebuilding, and retain source identity in version metadata.
- Dedicated WIF deployment account is separate from runtime identity. Permanent
  trust matches numeric repository/owner IDs, release workflow and website tag.
- Capture the entire prior split before mutation. Use `--no-promote
  --no-stop-previous-version`, one concurrency group, no running cancellation.
  Document pending-run replacement and reruns; manual rollback must not race it.
- ADR-0000, [Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md), reserves
  architecture decisions to humans; ADR-0001, [Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md),
  governs CCN/NLOC terminology. Both are Accepted; no conflict/new ADR is identified.

## Preparation premises

Observed 2026-10-27 in this worktree at `ca7f5ac105d0f3655bf3c5ccf8aa855a52688a42`,
using `nix develop -c` sequentially (parallel shell hooks recreate the same venv).

| Premise | Actual observation |
| --- | --- |
| Lockfile produces deployable upload | npm ci/build passed; `gcloud meta list-files-for-upload` included manifest and named JS/CSS, excluded frontend/node_modules |
| Browser proof exercises analysis | 16 scenarios / 134 steps passed; `E2E_BLOCK_ANALYSIS=1` produced 11 failed scenarios, including Python metrics, and exit 1; diagnostic environment removed |
| Existing recovery state readable | Service read showed `revive-20261027-f818daf` at 100% |
| Actions credential route absent | Repository variables and project Workload Identity pools both empty; real route remained slice 1 proof |

Local build/upload selection did not prove remote deployment. The known Nix
platformdirs/virtualenv warning did not prevent successful commands.
Platform references: [version naming](https://docs.cloud.google.com/appengine/docs/standard/reference/app-yaml),
[WIF action](https://github.com/google-github-actions/auth),
[concurrency](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-workflow-concurrency).
The 63-character combined name bound leaves 32 characters for this target's version.

## Execution context

- Mode story-branch; workspace `/Users/terryyin/git/lizard/.worktrees/deploy-website-releases-from-version-tags`;
  branch `codex/deploy-website-releases-from-version-tags`; publisher
  `dashboard-territory.local-lizard`; agent Hibiki-chan; remote `origin`, trunk `master`.
- Starting revision `6825db6122d1403ebd14f1cc682127233ec68a40`; accepted Take /
  initial published base `96e5aa4d77906e46f20f80a4bf1593c095314528` on origin/master.
  Ordinary increments target the remote execution branch. No numeric slice budget
  or replan override supplied; retain planning authority and bounded proof loops.
- Exact-checkout Nix setup succeeded; `nix develop -c python lizard.py --version`
  returned `1.24.3`. Dependency/product inputs remain unchanged by preparation/Take.
- Codex observer: `hibiki-seed012`, repo `terryyin/lizard`, target execution branch,
  workflow `python-app.yml` / `Python application`, cell `27`, session `94931`,
  directory `/tmp/dough-ci-501/watch-2aL4vZ`, PID `14608`; official owner classifier
  confirmed one live matching stream. Earlier `/tmp/dough-ci-501/watch-qKRFyu`
  stopped with zero unread events. The Take's trunk CI is unobserved.
- Proof decision: sequence one local regression/build/browser run and one hosted
  probe. Mac load 8.99/6.52/7.60, ~10 GiB inactive memory, no competing owned heavy
  work known. Remote runner/build headroom unknown; one Ubuntu 24.04 probe,
  timeout 30 minutes. Reconsider on contention/failure, never repeat blindly.
- Independent source-only review `/root/source_commitments` launched fresh without
  history, using only original story text; actual launch/return C01–C60 retained
  in conversation. Compare its individual commitments and journeys below.

## Slice 1 accepted proof

Local current regression, literal command (exit 0):
`PIP_QUIET=1 nix develop -c sh -c 'set -e; python -m pytest > /tmp/lizard-preview-pytest.log 2>&1; tail -3 /tmp/lizard-preview-pytest.log; make > /tmp/lizard-preview-make.log 2>&1; tail -5 /tmp/lizard-preview-make.log; npm --prefix website/frontend ci; npm --prefix website/frontend run build; npm --prefix website/frontend run test:e2e > /tmp/lizard-preview-browser.log 2>&1; tail -7 /tmp/lizard-preview-browser.log'`.
1,796 tests passed / 7 skipped; make succeeded; lockfile build and all 16 browser
scenarios / 134 steps passed. Existing scenarios use real Flask/analyzer results.

The temporary diagnostic commit `b34c4666b40322fb416822c5936d774eadab7f09` extends
`96e5aa4` with only probe orchestration/credential ignore. An isolated Git index
and SSH push published `codex/website-preview-probe-seed012`; execution HEAD/index
stayed unchanged. Git-data tree API refused HTTP 404; no API ref was published.
Recovery pair retained in `/tmp/lizard-preview-probe-candidate.json`.

[Hosted run 40482567466](https://github.com/terryyin/lizard/actions/runs/40482567466)
passed first attempt in 2m41s: unconditional checks/build/browser, OIDC auth,
source upload, remote Python 3.13 build/deploy, real preview and whole-split check.
Literal command: `PIP_QUIET=1 nix develop -c gh run watch 40482567466 --repo terryyin/lizard --exit-status --interval 20` (exit 0).

Assertions in the diagnostic workflow's `Verify normal HTTPS, generated assets
and real analysis` and `Record and verify unchanged traffic` steps exercised real
TLS/status, every manifest asset, runtime/source metadata, one function with
CCN 3 / NLOC 6 / one parameter, and entire service split equality. Setup supplied
only the actual service and configured federation identity, no mocked cloud state.
Coordinator independently inspected run/job/step conclusions and artifact JSON.

- Version `probe-40482567466-1`, runtime `python313`, metadata SHA exactly `b34c4666b40322fb416822c5936d774eadab7f09`.
- Preview <https://probe-40482567466-1-dot-lizardanalyzer.appspot.com>.
- Before, after and fresh service read: `{"allocations":{"revive-20261027-f818daf":1.0}}`.
  Old versions and zero-public-traffic preview retained.
- Evidence: run's `website-preview-probe` artifact; local
  `/tmp/lizard-preview-probe-40482567466/probe-evidence/` source, upload, version,
  analysis and traffic JSON; full log `/tmp/lizard-preview-probe-40482567466.log`.
- Cleanup confirmed: local workflow removed, exact remote probe ref deleted,
  provider ACTIVE with permanent numeric-ID/workflow/web-tag condition only.
  One stale immediate read resolved after 30 seconds without repeating mutation.
- Consumer trace command: `PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout "$PWD" --base HEAD --term gha-creds- --term WEBSITE_WIF_PROVIDER --term LIZARD_WEBSITE_SOURCE_SHA`;
  record `/tmp/lizard-preview-final-consumer-trace.md`. File discovery/options/web
  selector and their tests reuse full local/hosted pytest; generic behavior is
  unchanged. README/CLI/changelog/agent-map generic ignore mentions, installed
  lifecycle PLAN.md references and unrelated SEED-010 do not receive the effect
  and are excluded. No final-change test-support/journey consumer was located.
- Independent refactor `/root/preview_refactor` completed with `## REFACTOR COMPLETE`:
  cohesive development/release/identity guide split, byte-for-byte moved-content
  reconstruction and local-link validation via `PIP_QUIET=1 nix develop -c python /tmp/lizard-check-doc-refactor.py`;
  `git diff --check` passed. Runtime/proof unchanged; no test rerun. Trace:
  `/tmp/lizard-preview-refactor-trace.md`. All changed files are below 250 lines.

## Source comparison and remaining ownership

| Original trigger/setup → expected result | Actual observation / legitimate owner |
| --- | --- |
| Website tag → exact source to existing target; nonnegative decimal/no leading zero except 0; dot mapping, 32-character bound (C01–07) | Slice 2 owns final trigger/contract/candidate; slice 3 public publication. Probe source identity proved separately. |
| Malformed tags may validate, never auth/mutate; unrelated tags/branches/PRs never release; PyPI independent (C08–12) | Slice 2 guards/negative hosted runs. Authorized diagnostic ref removed; PyPI unchanged. |
| Tagged source → Python checks, existing browser scenarios, lockfile build, deploy that tested bundle; unconditional validation, reuse commands/sequence (C13–21) | Probe exercised all commands/generated upload. Slice 2 owns final tag path. |
| Actions → WIF plus separate deploy account scoped to repository/workflow/tags; setup/permissions documented (C22–25) | Real auth succeeded; permanent trust/grants/ownership documented and read back. |
| Deploy → no promotion/previous stop; verify homepage/assets/real analysis before traffic/public HTTPS (C26–31) | Real preview and unchanged split proved; slice 3 owns promotion/public check. |
| Actions result → tag, source, version, both URLs, prior split, result (C32–38) | Probe source/version/preview/split/result recorded; slices 2/3 own final tag/public record. |
| Validation/auth/deploy/preview failure → visibly failed, unchanged traffic; promotion/public failure → observed state/exact recovery command, maintainer restores saved split (C39–43) | Probe never promotes; slices 2/3 own final failure matrix. No broken production release for testing. |
| Retain versions; serialize without running cancellation; document pending replacement/retry (C44–48) | Versions retained; probe shared group/cancel=false; final workflow/docs in slices 2/3. |
| Immutable tags/same-source retry/traffic restore; ID reuse only known source+non-serving; serving success verifies without redeploy; ambiguity fails with guidance (C49–56) | Slice 2 owns identity/serving retry; slice 3 restoration. Unique probe avoided overwrite. |
| No numeric ordering; maintainer chooses tag (C57–58) | No ordering added; preserve in final workflow. |
| Early real credential/deploy probe, no assumed integration (C59–60) | Hosted route and temporary cleanup proved. |
| A serving → tag B → tested mapped preview → public B, source/both URLs reported | Preview path proved; final tag/public journey slices 2/3. |
| Zero valid; malformed/overlong and package/branch/PR examples do not deploy | Slice 2 contract and negative-run proof. |
| A survives build/credential/preview failure; failed public B restores A without rebuild/delete | Slices 2/3 failure/recovery, including live restoration proof. |
| Retry never overwrites serving version or silently deploys moved tag | Slice 2 final retry proof. |
| Automatic rollback/cleanup, prereleases/build suffixes, PR previews, DNS/billing, hosting/package changes deferred | None introduced; bounded diagnostic removed, versions retained. |

## Story obligations

### G1. Observe and clean up the real Actions preview route
Reported: slice 1 — "Outstanding required observations: hosted OIDC authentication, source upload, remote Python 3.13 build, preview HTTP results, metadata identity, unchanged hosted traffic, diagnostic-ref/workflow/trust cleanup."
Story clause: "Authenticate Actions through Workload Identity Federation and a deployment service account scoped to this repository's website release workflow/tags; document initial setup and required permissions."
Disposition: proved by slice 1: run 40482567466/artifact, independently inspected source/runtime/metrics/entire split; diagnostic workflow/ref removed and permanent provider condition read back ACTIVE.

## Ordered slices

### 1. Establish an authenticated Actions preview without changing public traffic
Type: Behavior
Status: done
Proof: Real bounded Actions probe deploys tested source to a unique non-serving
version, verifies HTTPS/homepage/assets/analysis, and records unchanged traffic.

Behavior: Given the configured existing app and repository deployment identity,
Actions authenticates, uploads generated assets, deploys Python 3.13 and serves
real analysis while the complete public split remains unchanged.
Inspect/configure minimal pool/provider/account/repository/IAM grants; document
ownership/configuration. Probe must be unable to promote. Remove temporary
workflow/ref/trust after evidence. Keep versions running. This authorized early
infrastructure probe resolves the first unavailable consuming operation; owner-held
IAM/GitHub failure requires exact operation/resume condition. Auth/upload/build/
preview failure stops dependent slices; a simulation never completes this slice.

### 2. Turn a valid website tag into a tested, inspectable preview candidate
Type: Behavior
Status: planned
Proof: Real tag-triggered runs prove event/checkout SHA, checks/generated upload,
preview URL/version and unchanged traffic. CLI tests cover tag contract; malformed
hosted tags fail validation and skip authentication/deployment.

Behavior: With the proven route, a valid tag produces its exact tested preview;
invalid tags fail and unrelated tags/branches/PRs never deploy. Implement one
validation/mapping home, including zero/multidigit/malformed/length boundaries;
pass tag as data. Unconditional Python/npm/browser proof, retained source/upload/
reports; preserve PyPI and branch CI. Establish existing version/source/serving
state before redeploy: reject ambiguity or serving mismatch with recovery guidance;
matching serving success can verify without overwrite. Exercise lightweight and
annotated tags and harmless invalid tags; no broken production release. Record
run links/probe tags and document tagging/retry. Interim valid releases are clearly
preview-only; slice 3 must replace this before story completion.

### 3. Promote verified candidates and make failed releases recoverable
Type: Behavior
Status: planned
Proof: Real tag run reaches public HTTPS/assets/sample analysis with matching
source/version. Nonpromoting failure probe blocks promotion. Exercise documented
saved-split restore against retained known-good versions; read final split/public
result. Local real-HTTP fixtures prove smoke rejection, never fake cloud/analyzer.

Behavior: Given a saved prior split and verified preview, promote then verify
public HTTPS; failure before promotion preserves old split. Promotion/public
failure visibly fails, honestly reports current/unknown traffic and exact restore
command. Include bounded TLS/homepage/assets/original-logo/HTTP-redirect checks
and sample CCN 3/NLOC 6/one parameter; verify traffic/version separately from an
old working homepage. Always-run summary covers tag/SHA/target/version/prior
split/URLs/stage/result/recovery, including credential failure without secrets.
Complete documentation for WIF/checks/tags/inspection/failure phases/concurrency/
pending replacement and manual restore. Retain old versions, no automatic cleanup
or rollback. Unobserved hosted acceptance remains outstanding with its owner.

## Verification and delivery gates

Use high-level CLI and real HTTP observables. Inspect all consumers before shared
setup extraction; preserve selector and failure reports. Use consumer-trace.
Before live deployment, current candidate must pass full pytest, make, npm ci/
build/browser and upload inspection; hosted proof follows actual runner/setup.
Apply proof acceptance, independent post-change refactor, managed delivery and
asynchronous CI repair. Record literal command/revision/run/observation/result.
No slice-specific concern remains beyond the now-proved early integration.
This shortened plan preserves the original outcome/proof loops and Ready basis;
execution evidence does not renew readiness or grant additional scope.
