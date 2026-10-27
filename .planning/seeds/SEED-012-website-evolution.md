---
id: SEED-012
status: active
planted: 2026-10-27
planted_during: Owner request for three website backlog items
trigger_when: Modernize, renovate and automate releases of lizard.ws
scope: bounded
---

# SEED-012: Website evolution

These are queued stories in the owner's requested order. Adding them
does not start implementation or make them ready for execution.

<a id="renovate-website-first-design"></a>

### Renovate the website using the first approved design

**Identity:** SEED-012#renovate-website-first-design

**Beneficiaries:** Developers discovering Lizard and using its online demo.

**Goal:** The website presents the first of the previously proposed frontend
designs, selected by the owner, across the homepage and online demo.

**Scope:** Apply that selected design to the website, including responsive
layout and the existing installation, documentation and analysis journeys.
Retain keyboard usability, clear metric results and recoverable error states
through the renovation.

**Key examples:**

- A visitor sees the selected first design on desktop and phone layouts.
- A visitor can still find installation/documentation links and complete a
  real analysis from the renovated interface.

**Design reference:** The owner explicitly chose “the first one” from the
previously proposed designs. The exact mockup/reference was not identifiable
in the retained artifacts when this story was queued. Attach that reference
and evaluable visual examples during refinement before implementation;
do not substitute a newly invented design or mark the story ready.

**Current frontend:** the page is the React app in `website/frontend/src/`
with its single stylesheet; the Cucumber/Playwright scenarios in
`website/frontend/features/` are the regression suite for the journeys
above (`docs/website.md`).

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
The deployable page bundle is generated: run `npm --prefix website/frontend
ci` and `run build` before `gcloud app deploy`, since `website/static/dist/`
is ignored by Git and allowlisted by `.gcloudignore`. Reuse the CI `web`
job's install and build commands (`docs/website.md`).
