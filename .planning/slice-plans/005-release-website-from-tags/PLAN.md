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
- Wait boundedly for preview startup; deployment completion alone does not prove HTTP readiness.
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

Local upload selection did not prove deployment. Nix's known platformdirs warning did not prevent checks. Source/platform references remain in the story and identity guide; the target's 63-character combined bound leaves 32 for the mapped version.

## Execution context

- Mode story-branch; workspace `/Users/terryyin/git/lizard/.worktrees/deploy-website-releases-from-version-tags`;
  branch `codex/deploy-website-releases-from-version-tags`; publisher
  `dashboard-territory.local-lizard`; agent Hibiki-chan; remote `origin`, trunk `master`.
- Starting revision `6825db6122d1403ebd14f1cc682127233ec68a40`; accepted Take /
  initial published base `96e5aa4d77906e46f20f80a4bf1593c095314528` on origin/master.
  Ordinary increments target the remote execution branch. No numeric slice budget
  or replan override supplied; retain planning authority and bounded proof loops.
- Slice 1 publication accepted as `0fb43372a348795b6d4eb34fbd6df9c6f3c38a1e`,
  suffix base `96e5aa4d77906e46f20f80a4bf1593c095314528`; same branch observer reused.
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
  Slice 2's fresh `/root/tag_source_review` independently returned the same full
  source promises and qualified journeys (67 separated commitments), retained in conversation.

## Slice 1 accepted proof

Local pytest1796/7skipped, make, lockfile build and browser16scenarios134steps passed; literal command retained in the implementation return. The hosted run below independently repeated the required candidate checks using real Flask/analyzer results.

Temporary diagnostic source `b34c4666b40322fb416822c5936d774eadab7f09` extends `96e5aa4` only with probe orchestration/ignore. Isolated-index SSH publication preserved execution HEAD/index; recovery `/tmp/lizard-preview-probe-candidate.json`. The exact temporary ref was later deleted.

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
- Independent `/root/preview_refactor` returned `## REFACTOR COMPLETE`: cohesive guide split, byte-for-byte reconstruction/local links via `PIP_QUIET=1 nix develop -c python /tmp/lizard-check-doc-refactor.py`, whitespace passed, unchanged runtime proof reused. Trace `/tmp/lizard-preview-refactor-trace.md`.

## Slice 2 accepted proof

Tested diagnostic source `94fc79a41c1df2140cf2d36d117454ae7c622e61`, parent `0fb43372`; isolated index preserves execution HEAD/index. Recovery `/tmp/lizard-slice2-probe-candidate.json`.
Current CLI/HTTP36/full1832 tests passed, 7 skipped: `PIP_QUIET=1 nix develop -c sh -c 'set -e; python -m pytest test/test_website_smoke.py test/test_website_release.py -q; python -m pytest > /tmp/lizard-tags-readiness-pytest.log 2>&1; tail -3 /tmp/lizard-tags-readiness-pytest.log; make > /tmp/lizard-tags-readiness-make.log 2>&1; tail -5 /tmp/lizard-tags-readiness-make.log; python /tmp/lizard-check-tags.py'`.
Current make/npm ci/build/browser16scenarios134steps also passed (`/tmp/lizard-tags-{make,browser}.log`); guard-only changes reuse unchanged application/browser proof. Actual CLI tests use temporary Git/JSON files, cover tag boundaries/event peeling/moved or unavailable objects/checkout mismatch/multiple saved allocations/source-only identity/retry alternatives/unknown traffic/failure summary.
`PIP_QUIET=1 nix develop -c python /tmp/lizard-check-tags.py` passed workflow YAML/shell/Python/upload assertions; manifest and named assets included, frontend/evidence/credentials excluded. Trace `/tmp/lizard-tag-candidate-consumer-trace.md` retains every disposition.
[Malformed web-01.0.0 run 40486098691](https://github.com/terryyin/lizard/actions/runs/40486098691) failed only at tag validation (exit2); preview skipped with zero steps. Retained `/tmp/lizard-tags-invalid-40486098691/release.json` contains no deployment state. This expected negative observation is not a broken production release.
Lightweight `web-0.0.0` [run 40486246278](https://github.com/terryyin/lizard/actions/runs/40486246278) deployed matching source/runtime, then failed HTTP500 at preview GET `/`; saved/fresh traffic remains old version=1. Structured request log shows platform dispatch abort, instanceIndex=-1, 6s after version creation. One later comparison returned200 for new legacy/regional and known-good/public controls (`/tmp/lizard-tags-web000-route-diagnostic.json`). This invalidates immediate-readiness assumption; add bounded startup handling with transient/persistent real-HTTP proof before new immutable tags. Failed run remains failed; no blind rerun.
Repair proved red (2fail/2pass) then green: initial-homepage5xx waits at most120s,5s spacing/≤10s requests; content/assets/metrics and normalTLS remain strict. RealHTTP→realFlask tests cover transient500 recovery, persistent500 deadline, immediate404 and HTTP rejection; only BUILD_DIR filesystem setup patched. Updated trace `/tmp/lizard-tag-readiness-consumer-trace.md` retains all dispositions.
Repaired isolated source `5d320dd497c0eb96d3f57cf5521ec05a8bfdb510` (same parent,8testedpaths), recovery `/tmp/lizard-slice2-ready-probe-candidate.json`: [lightweight web-0.0.2 run40488146510](https://github.com/terryyin/lizard/actions/runs/40488146510) succeeded; source/event/checkout exact, python313/versionweb-0-0-2 metadata match, realHTTPS/assets/metrics3/6/1, before/after/fresh traffic unchanged. Artifacts `/tmp/lizard-tags-lightweight-40488146510/release-evidence/` independently inspected.
Annotated `web-0.0.3` [run40488849369](https://github.com/terryyin/lizard/actions/runs/40488849369) succeeded: actual event object `6daa7515aeb9930b5b826ff7e57a454835c1e7f1` peeled to source/checkout `5d320dd4…`, versionweb-0-0-3/python313 metadata matches. Both successful artifacts plus fresh reads show entire old split unchanged, realmetrics3/6/1 and tested manifest assets. Coordinator inspected artifact JSON and actual run/job conclusions. Negative proof remains applicable: validator/permission gates unchanged by startup repair.
Literal hosted commands: `PIP_QUIET=1 nix develop -c gh run watch 40488146510 --repo terryyin/lizard --exit-status --interval 20` and same command with40488849369, both exit0. Artifacts `/tmp/lizard-tags-{lightweight-40488146510,annotated-40488849369}/release-evidence/`; actual preview/upload assertions via `PIP_QUIET=1 nix develop -c python /tmp/lizard-check-tag-evidence.py FOLDER 5d320dd497c0eb96d3f57cf5521ec05a8bfdb510 TAG` (each folder/tag above, exit0). Final trace `/tmp/lizard-tag-final-consumer-trace.md`, every consumer disposition inspected; unchanged branch/PyPI/selector/app boundaries reuse sufficient proof.
Independent `/root/tag_refactor` returned `## REFACTOR COMPLETE`: one registry now owns CLI choices/dispatch, same order/persistence/operations. Focused36 passed via `PIP_QUIET=1 nix develop -c sh -c 'python -m pytest test/test_website_smoke.py test/test_website_release.py -q; test_result=$?; test "$test_result" -eq 0'`; whitespace passed, trace `/tmp/lizard-tag-refactor-trace.md`. Coordinator inspected dispatch; cloud/HTTP/workflow boundaries unchanged, hosted/full/browser proof reused.

## Source comparison and remaining ownership

| Original trigger/setup → expected result | Actual observation / legitimate owner |
| --- | --- |
| Website tag → exact source to existing target; nonnegative decimal/no leading zero except 0; dot mapping, 32-character bound (C01–07) | CLI contract/event tests and real lightweight/annotated tag previews proved; slice 3 public publication. |
| Malformed tags may validate, never auth/mutate; unrelated tags/branches/PRs never release; PyPI independent (C08–12) | CLI rejection and hosted invalid validation+skipped preview proved; trigger/permission gates inspected; branch/PyPI unchanged. |
| Tagged source → Python checks, existing browser scenarios, lockfile build, deploy that tested bundle; unconditional validation, reuse commands/sequence (C13–21) | Both successful tag runs independently ran all existing commands, built and deployed their generated upload; event/source/runtime match. |
| Actions → WIF plus separate deploy account scoped to repository/workflow/tags; setup/permissions documented (C22–25) | Real auth succeeded; permanent trust/grants/ownership documented and read back. |
| Deploy → no promotion/previous stop; verify homepage/assets/real analysis before traffic/public HTTPS (C26–31) | Real preview and unchanged split proved; slice 3 owns promotion/public check. |
| Actions result → tag, source, version, both URLs, prior split, result (C32–38) | Tag/source/version/URLs/prior split/preview-only result recorded in real artifacts/summary; slice 3 public result/recovery. |
| Validation/auth/deploy/preview failure → visibly failed, unchanged traffic; promotion/public failure → observed state/exact recovery command, maintainer restores saved split (C39–43) | Probe never promotes; slices 2/3 own final failure matrix. No broken production release for testing. |
| Retain versions; serialize without running cancellation; document pending replacement/retry (C44–48) | Versions retained; probe shared group/cancel=false; final workflow/docs in slices 2/3. |
| Immutable tags/same-source retry/traffic restore; ID reuse only known source+non-serving; serving success verifies without redeploy; ambiguity fails with guidance (C49–56) | Real-Git/JSON CLI tests prove safe decisions, mismatch/unknown rejection and guidance; immutable tags retained/documented. Slice 3 restoration/live-serving verification (G4). |
| No numeric ordering; maintainer chooses tag (C57–58) | No ordering added; preserve in final workflow. |
| Early real credential/deploy probe, no assumed integration (C59–60) | Hosted route and temporary cleanup proved. |
| A serving → tag B → tested mapped preview → public B, source/both URLs reported | Preview path proved; final tag/public journey slices 2/3. |
| Zero valid; malformed/overlong and package/branch/PR examples do not deploy | CLI boundary tests + hosted zero-component valid tags and malformed rejection; workflow trigger and independent PyPI inspected. |
| A survives build/credential/preview failure; failed public B restores A without rebuild/delete | Slices 2/3 failure/recovery, including live restoration proof. |
| Retry never overwrites serving version or silently deploys moved tag | CLI real-Git/JSON source and serving decisions proved; live matching-serving continuation remains G4 in slice 3. |
| Automatic rollback/cleanup, prereleases/build suffixes, PR previews, DNS/billing, hosting/package changes deferred | None introduced; bounded diagnostic removed, versions retained. |

## Story obligations

### G1. Observe and clean up the real Actions preview route
Reported: slice 1 — "Outstanding required observations: hosted OIDC authentication, source upload, remote Python 3.13 build, preview HTTP results, metadata identity, unchanged hosted traffic, diagnostic-ref/workflow/trust cleanup."
Story clause: "Authenticate Actions through Workload Identity Federation and a deployment service account scoped to this repository's website release workflow/tags; document initial setup and required permissions."
Disposition: proved by slice 1: run 40482567466/artifact, independently inspected source/runtime/metrics/entire split; diagnostic workflow/ref removed and permanent provider condition read back ACTIVE.

### G2. Preview-only behavior must become automatic public publication
Reported: slice 2 — "Valid tags currently produce verified preview-only candidates and never promote public traffic."
Story clause: "then move traffic and verify the public HTTPS endpoint."
Disposition: interim until slices 3

### G3. Observe the new tag-to-preview integration
Reported: slice 2 — "Outstanding observations remain owned by slice 2: actual tag-triggered event/checkout identity, new CLI's authenticated inspect/deploy/verify integration, preview metadata/assets/metrics, and unchanged traffic."
Story clause: "A pushed `web-<major>.<minor>.<patch>` tag releases its exact commit"
Disposition: proved by slice 2: runs40488146510/40488849369 and retained artifacts; exact event/source/runtime, generated upload, realHTTPS/assets/metrics and full unchanged split independently inspected; invalid run40486098691 skipped auth/deployment.

### G4. Verify the real already-serving retry path
Reported: slice 2 — "A live already-serving retry has not been exercised; its guard decision has CLI proof, while a real serving rerun can be observed after slice 3 promotes a candidate."
Story clause: "a successful already-serving release can be verified without redeploy."
Disposition: receiving slice 3

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
Status: done
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
