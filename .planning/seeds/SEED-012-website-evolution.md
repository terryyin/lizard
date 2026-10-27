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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/005-release-website-from-tags/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"bf5c63a8ff43f99d18b0016d049d0ecd0a6531ec6dd9bfd76623acbc152c5380","plan":"63b77872745e5b4f620ec2470f8c1117e3ae2473a36e044e277ab42490b7e8d7"}}
```

**Beneficiaries:** Maintainers releasing lizard.ws and visitors receiving the
published website.

**Goal:** A maintainer publishes a website version tag and GitHub Actions
automatically deploys that version to the existing GCP-hosted website.

**Scope:** A pushed `web-<major>.<minor>.<patch>` tag releases its exact commit
to the existing `lizardanalyzer` App Engine `default` service and
`https://www.lizard.ws/`. Each component is a nonnegative decimal integer
without leading zeroes (except `0`); prerelease/build suffixes are deferred.
Map dots to hyphens: `web-1.2.3` becomes `web-1-2-3`. Reject a mapped version
longer than 32 characters before deployment, because App Engine limits
`VERSION-dot-SERVICE-dot-PROJECT_ID` to 63 characters for this target.
Malformed website tags may start a validation run but must never authenticate
for deployment or change the service. Unrelated tags, branch pushes and pull
requests do not start website deployment; keep the PyPI release independent.

Validate and build the tagged source, including Python tests/checks and the
existing frontend browser scenarios. Build from the npm lockfile and deploy
that tested generated bundle. A branch's path-filtered CI result cannot replace
release validation. Reuse the existing build and validation commands and the
preview-before-promotion sequence in `docs/website.md`.

Authenticate Actions through Workload Identity Federation and a deployment
service account scoped to this repository's website release workflow/tags;
document initial setup and required permissions. Deploy without promotion or
stopping the previous version, verify the preview homepage/assets and real
analysis, then move traffic and verify the public HTTPS endpoint. Record the
tag, source SHA, App Engine version, preview/public URLs, prior traffic split
and release result in Actions. A failed validation, authentication, deployment
or preview check leaves existing traffic untouched and is visibly failed.
After a promotion or public-verification failure, report the observed traffic
state and exact recovery command; the maintainer restores the saved split.
Retain versions for diagnosis and rollback. Serialize release mutations without
cancelling a running deployment; document pending-run cancellation and retry.

Document immutable release tags, retrying a failed run for the same source,
and recovery by restoring traffic rather than moving tags. Reuse a version ID
on retry only when the source identity is established and the version is not
serving; a successful already-serving release can be verified without redeploy.
If identity or serving state is ambiguous, fail with recovery guidance.

**Deferred promises:** Automatic rollback, version garbage collection,
prereleases, preview environments for PRs, DNS/billing changes, a hosting
migration, and analyzer-package release changes. No numerical version ordering
policy is introduced; a maintainer chooses the tag to publish.

**Key examples:**

- Given the service is serving version A, publishing `web-1.2.3` at commit B
  validates B, builds its bundle, deploys `web-1-2-3` without traffic, verifies
  its preview, then serves B publicly; Actions identifies B and both URLs.
- `1.2.3`, a branch push and a PR do not deploy the website. `web-1.2`,
  `web-01.2.3`, `web-1.2.3-rc.1` and an overlong mapped version cannot reach
  deployment. `web-0.0.0` is a valid version shape.
- With A serving, a build, credential or preview failure leaves A serving
  and fails the run. If public verification fails after B receives traffic,
  the failed run records the prior split and the maintainer can restore A
  without rebuilding or deleting B.
- Retrying a run cannot overwrite a serving version with an unverified build
  or silently deploy a moved tag's different commit.

**Preparation evidence:** The existing release guide supplies the target,
preview/promotion/recovery sequence and known previous version. On 2026-10-27,
read-only inspection confirmed the `default` service serving
`revive-20261027-f818daf` at 100%; the project had no Workload Identity pools
and the repository had no Actions repository variables. Credential setup and
an actual Actions-to-App-Engine preview remain an early execution probe,
not an assumed available integration.

**Platform references:** [App Engine version naming](https://docs.cloud.google.com/appengine/docs/standard/reference/app-yaml),
[Google Actions authentication](https://github.com/google-github-actions/auth).
