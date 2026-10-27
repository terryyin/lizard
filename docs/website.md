# Running and releasing lizard.ws

The website is a Flask adapter around `lizard.analyze_file.analyze_source_code`.
`index:app` is the application; `website.application` remains a compatibility
import. The homepage, demo, styles and examples are local assets. No browser
libraries or frontend build are required.

## Local development

```sh
nix develop -c python -m flask --app index run --port 8080
nix develop -c python -m pytest test/test_website.py
```

Development dependencies include a Flask release compatible with the current
Python. This does not add Flask to the distributed analyzer's dependencies.
Production uses Python 3.13 and `requirements.txt`. To check that environment
separately, use Python 3.13 to create an isolated environment, install
`requirements.txt` and pytest, then run `python -m pytest test/test_website.py`
from the checkout. The Nix environment remains the repository tooling wrapper.
When invoking that separate Python from Nix, use `python -I -m pip` and
`python -I -m pytest` so Nix's Python-specific import path does not leak into
the production environment.

The JSON API is `POST /analyze` with `Content-Type: application/json`:

```json
{"lang":".py","code":"def answer():\n    return 42\n"}
```

`filename` can replace `lang`. Supported suffixes come from Lizard's existing
language readers; omitting both uses `.c` for existing callers. The response
contains file `filename`, `nloc`, `token_count` and `function_list`. Each
function includes its name, long name, start/end lines, NLOC, cyclomatic
complexity, token count and parameter count. `/analyse` also accepts JSON or
the older form fields `content` and optional `lang`, through the same adapter.

The app limits source to 64 KiB of UTF-8 and request bodies to 512 KiB.
Gunicorn uses one synchronous worker per instance with a 15-second timeout;
the browser stops waiting after 20 seconds and offers recovery. App Engine
is configured with `max_instances: 2`. These controls bound work but are not a billing
cap or a distributed rate limiter. Source is processed in memory and is not
saved or logged by the app. Do not enable debug mode or request-body logging
on the public service. Use the local CLI for private source or larger files.

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

Run the full suite and combined checks on the candidate before any deploy:

```sh
nix develop -c python -m pytest
nix develop -c make
gcloud meta list-files-for-upload
```

Review the upload list: it must contain only application/analyzer code,
`requirements.txt`, `app.yaml`, templates and public assets. `.gcloudignore`
excludes repository history, planning records, tests and local environments.
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
