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
