# Remove local proof from Lizard

**Identity:** SEED-036#remove-local-proof
**Source:** [canonical story](../../seeds/SEED-036-remove-local-proof.md#remove-local-proof)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

A Lizard developer maintains a smaller repository without a local Cucumber proof
collection or persistence feature, while ordinary local tests and GitHub Actions
CI proof continue to serve their existing purposes.

**In scope:** delete `website/frontend/proof` and local-proof-only artifacts;
delete `proof-plugin.js`, `proof-snapshot.js`, `RECORD_CUCUMBER_PROOF` wiring,
recording commands and `test:proof`; remove local `proof` / `proofGeneration`
from `.planning/open-dough.json` while preserving Intent source locations and CI
proof entry identities (`test`, `website/frontend/features`); delete dedicated
proof tests, exclusive helpers/deps (including `proper-lockfile` when unused
elsewhere), the CI `test:proof` step, and project-owned local-proof docs/guidance;
one-time whole-product search audit instead of a permanent absence checker.

**Excluded:** richer CI proof; any replacement local archive; changing Intent
entry identities beyond dropping local-proof fields; editing installed managed
Open Dough guidance (receive shared guidance cleanup via released Open Dough
update). Spent story/plan cleanup belongs to wrap-up, not product deletion.

**Architecture / PFE:** Delete the existing local-proof owner in place
(`website/frontend` recorder + retained snapshot + Intent local-proof fields +
dedicated tests). Reuse the surviving CI proof publisher under `scripts/ci-proof/`
and the GitHub Actions fragment → summary path unchanged. No new representation,
compat shim, or North Star topic. Accepted ADRs
[ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md) and
[ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md) need no change.

**Assumptions:** CI proof (report → fragment → summary → dashboard) is a separate
responsibility from the deleted local recorder. Ordinary review evidence and CI
run artifacts keep their surviving purpose without a local fallback.

## Outside-in proof

- Ordinary Cucumber route without recording: `npm --prefix website/frontend run test:e2e:dry`
  (and surviving website/analyzer suites as affected).
- Intent config still names `test` and `website/frontend/features` without
  `proof` / `proofGeneration`.
- Workflow no longer runs `test:proof`; CI proof fragment/summarize steps and
  `--expect test=1 --expect website/frontend/features=1` remain.
- One-time search: no remaining local-proof collection/persistence entry points,
  exclusive fixtures, or orphaned deps in the maintained product tree.
- Dedicated feature tests are deleted, not replaced by a permanent absence checker.

## Settled decisive premises

Observed in this preparation workspace at product revision
`6b5969042cc91c1fcaff4af77e8a225c51b51a3b` (2026-11-05):

| Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- |
| Ordinary Cucumber loads no proof plugin when `RECORD_CUCUMBER_PROOF` is unset | 1 | `RECORD_CUCUMBER_PROOF= node --input-type=module` import of `website/frontend/cucumber.mjs` | `plugin: []`; with `RECORD_CUCUMBER_PROOF=1`, `plugin: ["./proof-plugin.js"]` |
| Ordinary Cucumber dry-run succeeds without the plugin file or RECORD wiring | 1 | Scratch-deleted `proof-plugin.js`, forced `plugin: []`, ran `website/frontend/node_modules/.bin/cucumber-js --dry-run`, then restored | Exit 0 — 16 scenarios / 134 steps skipped; no plugin load |
| `scripts/ci-proof/` does not reference local website proof recorder paths | 1 | `rg` for `proof-plugin`, `proof-snapshot`, `RECORD_CUCUMBER`, `website/frontend/proof` under `scripts/ci-proof/` | No matches |
| CI expects Intent entries `test` and `website/frontend/features` | 1 | `.github/workflows/python-app.yml` fragment `--entry` and summarize `--expect` | Entries present; separate web job step runs `npm … run test:proof` |
| Dedicated proof tests and helper are exclusive to the feature | 1 | `test:proof` → `proof-plugin.test.js` + `proof-refresh.test.js`; `cucumber-runner.js` imported only by those two; `npm run test:proof` after `npm ci` | 24 pass; runner not used elsewhere |
| `proper-lockfile` is exclusive to local proof | 1 | `rg proper-lockfile` under `website/frontend` excluding lockfile noise | Only `package.json`, `proof-snapshot.js`, and proof-refresh fixture import |

## Ordered slices

### 1. Local Cucumber proof collection and persistence are gone
Type: Behavior
Status: done
Proof: After deletion, `test:e2e:dry` succeeds with no proof plugin; Intent Gherkin
location has no `proof`/`proofGeneration`; `test:proof` script, dedicated tests,
exclusive deps, and the workflow's Cucumber proof-recorder step are gone; CI
proof fragment/summarize expectations for `test` and `website/frontend/features`
remain; whole-product search finds no local-proof collection/persistence entry
points or orphaned exclusive support.

Accepted proof (slice 1):
- `npm --prefix website/frontend run test:e2e:dry` → exit 0; 16 scenarios / 134 steps skipped; `cucumber.mjs` has no `plugin` field.
- `.planning/open-dough.json` keeps Intent locations `test` and `website/frontend/features` without `proof`/`proofGeneration`.
- `.github/workflows/python-app.yml` lacks the Cucumber proof-recorder / `test:proof` step; fragment `--entry` and summarize `--expect test=1 --expect website/frontend/features=1` remain; `scripts/ci-proof/` has no local recorder refs.
- Product-tree search: no `proof-plugin`, `proof-snapshot`, `RECORD_CUCUMBER_PROOF`, `test:proof`, `proper-lockfile`, `proofGeneration`, or `website/frontend/proof` entry points (managed Open Dough guidance and this story/plan excluded by scope).
- `nix develop -c python -m pytest test/test_analyzer.py` → 22 passed.
- Refactor disposition: none — already clean (`## REFACTOR COMPLETE`).

Behavior: Given the repository still carries `website/frontend/proof`, the
recorder modules, `RECORD_CUCUMBER_PROOF` wiring, Intent local-proof fields,
`test:proof`, and its CI step — when this slice completes, those surfaces are
deleted outright (no stub, archive, opt-in, or migration guide), ordinary local
Cucumber/website and analyzer verification still serve their existing purposes,
and GitHub Actions CI proof publication for the same Intent entry identities
continues without the dedicated local-proof recorder job step.

## Concern evidence

Slice-plan refinement: not needed — one cohesive Behavior, single proof loop,
no cumulative special-case design, and premises settled by observation.

Remaining slice-specific concerns: none identified.

## Story obligations

### G1. Deferred richer CI proof
Reported: slice 1 — "Deferred: richer CI proof than today's contract."
Story clause: "Deferred promises: richer CI proof than today's contract"
Disposition: excluded "Deferred promises: richer CI proof than today's contract"

### G2. Deferred replacement local archive
Reported: slice 1 — "Deferred: any replacement local verification archive."
Story clause: "any replacement local verification archive"
Disposition: excluded "any replacement local verification archive"

### G3. Deferred Intent identity changes
Reported: slice 1 — "Deferred: changing Intent entry identities beyond dropping local-proof fields."
Story clause: "changing Intent entry identities beyond dropping local-proof fields"
Disposition: excluded "changing Intent entry identities beyond dropping local-proof fields"
