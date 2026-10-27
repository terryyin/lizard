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

## Release record: 2026-10-27

The preview was verified before promotion. Production now serves the tested
version, and the public HTTPS homepage and analyzer are verified.

| Item | Observed value |
| --- | --- |
| Source candidate | `f818daf65e5981e4d58589d503b4016644206c62` |
| Project / service | `lizardanalyzer` / `default` |
| New version | `revive-20261027-f818daf`, created `2026-10-27T04:49:48Z` |
| Previous production version | `20180602t165847` |
| Current production split | `revive-20261027-f818daf=1.0` |
| Public URL | <https://www.lizard.ws/> |
| Preview URL | <https://revive-20261027-f818daf-dot-lizardanalyzer.appspot.com> |
| Regional URL returned by the version resource | <https://revive-20261027-f818daf-dot-lizardanalyzer.uc.r.appspot.com> |
| Deployed runtime and limits | `python313`, `F1`, `maxInstances: 2`, `maxConcurrentRequests: 1` |

Billing was confirmed enabled. Before deployment, the unchanged candidate
passed the full pytest suite (1,692 passed, seven skipped), the combined
`make` checks, and 27 website HTTP tests under both Nix Python 3.9 and the
isolated production Python 3.13 environment. The preview was deployed with:

```sh
gcloud app deploy app.yaml --project=lizardanalyzer --version=revive-20261027-f818daf --no-promote --no-stop-previous-version --quiet
```

The first attempt failed because the existing App Engine service account
could not read the staging bucket. The existing account already had its
project Editor role; no additional broad project role was granted. The
specific repair added source-object read access on the staging bucket:

```sh
gcloud storage buckets add-iam-policy-binding gs://staging.lizardanalyzer.appspot.com --member=serviceAccount:lizardanalyzer@appspot.gserviceaccount.com --role=roles/storage.objectViewer --quiet
```

The required `appengine.googleapis.com`, `cloudbuild.googleapis.com` and
`artifactregistry.googleapis.com` APIs were enabled. The next deployment
completed successfully.

HTTPS preview observations at `2026-10-27T04:51:10Z` used normal certificate
and hostname verification. The homepage, CSS, JavaScript, both published
logo URLs and both favicon URLs returned 200. The real Python example
returned `shipping_cost`, CCN 3, NLOC 6, one parameter and 23 function tokens.
Malformed JSON returned 400, 65,537 bytes of source returned 413, and the
valid example succeeded again after those rejected requests.

The bounded version-specific error-log checks after preview and public
verification both completed successfully with no entries returned:

```sh
gcloud app logs read --project=lizardanalyzer --service=default --version=revive-20261027-f818daf --level=error --limit=30
```

The initial managed-certificate attempt returned
`INTERNAL (13): Error provisioning managed certificate` while `lizard.ws`
was absent from the current account's verified domains. After the
verification TXT record was added in GoDaddy, Search Console verified ownership
and `gcloud domains list-user-verified --project=lizardanalyzer --format=json`
listed `lizard.ws`. Enabling automatic certificate management then succeeded:

```sh
gcloud app domain-mappings update www.lizard.ws --certificate-management=automatic --project=lizardanalyzer --quiet
```

The `www.lizard.ws` CNAME mapping still points to `ghs.googlehosted.com`.
Managed certificate `47191116` progressed from `PENDING` to `OK`, with
`lastRenewalTime: 2026-10-27T05:45:41.666467148Z`, and the mapping now has
that active `certificateId`. Normal TLS initially remained unavailable
while the certificate reached the serving endpoint, so production stayed
on the old version during that wait. After propagation, normal curl GET
returned `status=200 tls_verify=0`. Only then was traffic promoted:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=revive-20261027-f818daf=1 --quiet
```

The command exited 0, and the service's observed allocation became
`revive-20261027-f818daf=1.0`. Versions `1`, `20180602t164047`,
`20180602t165227` and the rollback version `20180602t165847` all remain
`SERVING`, with zero traffic; none was stopped or deleted.

The public smoke command completed with exit 0 at
`2026-10-27T06:07:09.320231Z`:

```sh
nix develop -c python .local/release-smoke.py https://www.lizard.ws
```

Using default HTTPS trust and hostname checks, it verified the homepage,
CSS, JavaScript, both logo URLs and both favicon URLs returned 200. The
public `/analyze` request returned one `shipping_cost` function with CCN 3,
NLOC 6, one parameter and 23 function tokens; the response was `no-store`.
Malformed JSON returned 400 without a traceback, oversized source returned
413, and the valid request succeeded again. A brief old-homepage response
immediately after the traffic switch cleared before this passing check.

At `2026-10-27T06:07:41.992855Z`, both the HTTP homepage and original small
logo URL redirected with 302 to their matching HTTPS URLs and returned 200
after following the redirect. The logo check also confirmed its query
string was preserved.

The chosen canonical endpoint is `https://www.lizard.ws/`. GoDaddy's
existing bare-domain forwarding remains a permanent 301 from
`http://lizard.ws/` to `http://www.lizard.ws/`, followed by the site's 302
to `https://www.lizard.ws/`. An independent normal `curl -L` observation
confirmed this chain finishes at the chosen HTTPS URL with status 200 and
`tls_verify=0`. The forwarding-service A records and existing rule were
preserved; changing the rule to a direct HTTPS destination is optional and
was not saved. HTTPS on the bare `lizard.ws` hostname was not verified and
is outside the chosen public endpoint.

The independent public browser check also observed the demo progress from
working to completed analysis and display `shipping_cost` with CCN 3,
NLOC 6, 23 function tokens and one parameter.

If this release needs recovery, restore the recorded prior version:

```sh
gcloud app services set-traffic default --project=lizardanalyzer --splits=20180602t165847=1
```
