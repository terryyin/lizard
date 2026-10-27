# Releasing lizard.ws

See [local development and browser checks](website.md) for the application setup
and [Actions deployment identity](website-deployment-identity.md) for credential setup.

## Website tags and Actions previews

The `Website release` workflow currently produces a **verified preview only**.
It does not promote public traffic. Automatic promotion and public verification
are the next implementation slice; inspect the reported result rather than
treating a preview-only run as a completed public release.

Publish an immutable website tag at the intended commit, using Git directly:

```sh
git tag -a web-1.2.3 -m 'Website 1.2.3' COMMIT
git push origin refs/tags/web-1.2.3
```

Lightweight tags also work. Each version component is a nonnegative decimal
integer without leading zeroes except `0`; `web-0.0.0` is valid. Dots map to
hyphens (`web-1.2.3` → `web-1-2-3`), with at most 32 mapped characters for
this existing project/service. Prerelease and build suffixes are unsupported.
There is no numeric ordering requirement: the maintainer chooses the tag.
Never move or reuse a published tag for a different source.

Only `web-*` tag pushes select `.github/workflows/website-release.yml`.
Malformed website tags fail the credential-free validation job; the dependent
preview job cannot authenticate or deploy. Ordinary branches, pull requests and
package tags do not select this workflow. The existing PyPI workflow is separate.
Tag text is passed through environment data, never inserted into shell code.

Validation peels the immutable event object and compares it with the checkout;
the preview job checks out that validated commit. Annotated tags are supported.
Reruns use the original event object, not the current branch head or a moved
tag. If that object is no longer available, recover it before retrying; the
workflow fails instead of guessing another source.

Every valid tag runs Python tests and checks, the declared Node/npm tooling,
`npm ci`, the build and all browser scenarios, even when branch CI would skip
its browser job. Authentication follows validation. Deployment uploads that
tested generated bundle without rebuilding, using `--no-promote` and
`--no-stop-previous-version`. The preview checks normal HTTPS, generated assets
and real sample analysis, and compares the complete traffic split before/after.

Inspect the run's summary and `website-source` / `website-release` artifacts.
They retain the tag, event object, peeled/checked-out source, mapped version,
upload list, preview/public URLs, previous split, stage and result. Browser
reports are retained too; credential files are excluded. The source and tag
are also written into the App Engine version's environment metadata.

Retry a failed run for the same immutable source using Actions **Re-run jobs**.
An existing version can be redeployed only when its source identity is
established and it has zero public traffic. A matching already-serving version
is verified without redeploying it. Missing/different metadata or unknown traffic
state fails with guidance to inspect the version and use a new immutable tag;
never overwrite a serving mismatch. Old versions remain available for recovery.

## Inspect the existing project

Use gcloud directly, with explicit project selection:

```sh
gcloud billing projects describe lizardanalyzer
gcloud app describe --project=lizardanalyzer
gcloud app versions list --project=lizardanalyzer
gcloud app services describe default --project=lizardanalyzer
gcloud app domain-mappings list --project=lizardanalyzer
```

Record the currently serving version and traffic split before changing them.
The default hostname and region come from `gcloud app describe`; do not guess
a hostname from the project ID. If billing is disabled, restore it and verify
service access before deployment. Reopening a billing account can resume
services in every project linked to it; inspect those projects first.

## Preview before release

Build the frontend from the lockfile, then run the full suite and combined
checks on the candidate before any deploy:

```sh
nix develop -c npm --prefix website/frontend ci
nix develop -c npm --prefix website/frontend run build
nix develop -c npm --prefix website/frontend run test:e2e
nix develop -c python -m pytest
nix develop -c make
gcloud meta list-files-for-upload
```

Review the upload list: it must contain only application/analyzer code,
`requirements.txt`, `app.yaml`, the shell template, the favicon and images,
and `website/static/dist/` (the manifest and every asset it names).
`.gcloudignore` excludes repository history, planning records, tests, local
environments and all of `website/frontend/` (sources, scenarios,
`node_modules`, reports). Deploying without a build serves the 503 diagnostic.
Choose a unique version ID and keep the prior version running:

```sh
gcloud app deploy app.yaml --project=lizardanalyzer --version=VERSION --no-promote --no-stop-previous-version
gcloud app versions describe VERSION --service=default --project=lizardanalyzer
gcloud app browse --project=lizardanalyzer --service=default --version=VERSION --no-launch-browser
```

Use the returned preview URL to check the homepage, styles, scripts, original
logo paths, keyboard navigation, narrow-screen layout, a sample analysis,
empty/oversized input and an error followed by a successful retry. The sample
Python function on the homepage should report one function, CCN 3, NLOC 6,
and one parameter. Inspect logs without enabling request-body logging:

```sh
gcloud app logs read --project=lizardanalyzer --service=default --version=VERSION --level=error --limit=30
```

## Traffic, domains and recovery

The preferred public URL is `https://www.lizard.ws/`. The `secure: always`
handlers redirect HTTP to HTTPS. Verify the domain mapping, managed certificate
and normal HTTPS trust/hostname acceptance before moving traffic.

Only after preview and public HTTPS checks pass, direct traffic to the new version:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=VERSION=1
```

After switching, verify the public homepage, demo and HTTP redirects. Keep the
original logo paths working because external README and package links use them.

The bare domain needs a verified DNS/domain mapping or a registrar redirect
to `https://www.lizard.ws/`. Do not replace unrelated DNS records, MX records,
or verification TXT records. Obtain the exact records from the observed GCP
domain mapping; if DNS remains outside GCP, the domain owner applies them at
the registrar. Do not redirect or claim HTTPS for an unverified hostname.

Record the actual new version, previous version, preview URL and observed
public-domain result here at release. Deployment is unfinished while billing,
domain control or certificate provisioning blocks that public verification.
To recover, restore the recorded previous version or recorded traffic split:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=PREVIOUS_VERSION=1
```

Retain the tested new version for diagnosis; avoid deleting either version
as part of a traffic rollback. Scaling limits reduce exposure; budget alerts
can help monitoring but do not impose a hard spending limit.

## Current public release

Verified on 2026-10-27. The public homepage, assets and real analysis demo are
served at <https://www.lizard.ws/> with normal HTTPS trust and hostname checks.

| Item | Value |
| --- | --- |
| Project / service | `lizardanalyzer` / `default` |
| Runtime source | `f818daf65e5981e4d58589d503b4016644206c62` |
| Serving version / traffic | `revive-20261027-f818daf` / 100% |
| Runtime / limits | `python313`, F1, maximum two instances, one concurrent request per instance |
| Previous version for rollback | `20180602t165847` |
| Preview URL | <https://revive-20261027-f818daf-dot-lizardanalyzer.appspot.com> |
| Managed certificate | `47191116`, automatic management, status `OK` |
| `www` DNS | CNAME `ghs.googlehosted.com.` |

All previous versions remain available; release and rollback do not require
stopping or deleting them. HTTP on `www` redirects to HTTPS, preserving path
and query. The existing GoDaddy permanent 301 from `http://lizard.ws/` to
`http://www.lizard.ws/` reaches the chosen HTTPS endpoint through that redirect.
The forwarding-service A records and verification TXT records are retained.
HTTPS on the bare hostname has not been verified; use the chosen `www` URL.

The public verification covered the homepage and six local asset URLs,
actual `shipping_cost` metrics (CCN 3, NLOC 6, one parameter, 23 function
tokens), malformed JSON rejection, oversized source rejection, and a successful
retry. POST responses are `no-store`. Independent browser analysis completed,
and the bounded version error-log check returned no entries.

Deployment uses the existing App Engine service account
`lizardanalyzer@appspot.gserviceaccount.com`. It needs read access to source
objects in `gs://staging.lizardanalyzer.appspot.com`; the bucket grants it
`roles/storage.objectViewer`. App Engine, Cloud Build and Artifact Registry
APIs are enabled. Resolve a source-bucket access error at that bucket rather
than adding another broad project role.

Managed certificates require verified domain ownership for the deploying
Google account. Retain the Google verification TXT record in GoDaddy. To
restore automatic management on the existing mapping when needed:

```sh
gcloud app domain-mappings update www.lizard.ws --certificate-management=automatic --project=lizardanalyzer
```

An issued certificate can precede serving-endpoint propagation. Check normal
HTTPS acceptance as well as certificate status before moving traffic; do not
bypass TLS validation or repeatedly replace a pending certificate.

To restore the recorded previous version:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=20180602t165847=1
```
