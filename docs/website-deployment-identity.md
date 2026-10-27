# Actions deployment identity

Website deployment uses GitHub OIDC and Workload Identity Federation through
a dedicated deployment account. It does not use a stored service-account key.
Terry owns the initial GCP IAM and repository-variable setup; the runtime
account remains separate. The following resources were configured on
2026-10-27 after inspecting the existing project, account and bucket policies:

| Resource | Value |
| --- | --- |
| Project / project number | `lizardanalyzer` / `54408718577` |
| GitHub repository / numeric ID | `terryyin/lizard` / `4738329` |
| GitHub owner numeric ID | `788834` |
| Workload Identity pool / provider | `github-actions` / `website-release`, location `global` |
| Deployment account | `website-deployer@lizardanalyzer.iam.gserviceaccount.com` |
| Runtime account | `lizardanalyzer@appspot.gserviceaccount.com` |
| Repository variable `WEBSITE_WIF_PROVIDER` | `projects/54408718577/locations/global/workloadIdentityPools/github-actions/providers/website-release` |
| Repository variable `WEBSITE_DEPLOY_SERVICE_ACCOUNT` | `website-deployer@lizardanalyzer.iam.gserviceaccount.com` |

Enable `iam.googleapis.com`, `iamcredentials.googleapis.com` and
`sts.googleapis.com` for federation, in addition to the existing App Engine,
Cloud Build, Artifact Registry and Storage APIs. The provider's issuer is
`https://token.actions.githubusercontent.com`. Its attribute mapping is:

```text
google.subject=assertion.sub
attribute.repository_id=assertion.repository_id
attribute.repository_owner_id=assertion.repository_owner_id
attribute.ref=assertion.ref
attribute.workflow_ref=assertion.workflow_ref
```

Use numeric repository/owner claims to prevent a renamed or recreated
repository from inheriting trust. The permanent provider condition is:

```text
assertion.repository_id == '4738329' &&
assertion.repository_owner_id == '788834' &&
assertion.ref.startsWith('refs/tags/web-') &&
assertion.workflow_ref == 'terryyin/lizard/.github/workflows/website-release.yml@' + assertion.ref
```

The tag validator must run before requesting credentials: the provider's
prefix restriction is an identity boundary, not the version-format validator.
Grant `roles/iam.workloadIdentityUser` on the deployment account to this member:

```text
principalSet://iam.googleapis.com/projects/54408718577/locations/global/workloadIdentityPools/github-actions/attribute.repository_id/4738329
```

The deployment account has these grants; existing unrelated bindings are
preserved:

| Scope | Role | Purpose |
| --- | --- | --- |
| Project `lizardanalyzer` | `roles/appengine.deployer` | Read App Engine state and deploy versions |
| Project `lizardanalyzer` | `roles/cloudbuild.builds.editor` | Submit and observe the App Engine source build |
| Bucket `gs://staging.lizardanalyzer.appspot.com` | `roles/storage.objectAdmin` | Upload source objects in the deployment staging bucket |
| Runtime account only | `roles/iam.serviceAccountUser` | Deploy using the existing runtime identity |

The runtime account retains its existing bucket-level
`roles/storage.objectViewer` grant. The deployment account has no Owner,
Editor, service-account-key, IAM-administration or traffic-routing grant.
The preview probe therefore cannot promote or stop versions. Its commands
also use `--no-promote --no-stop-previous-version`; use a unique version ID
because App Engine Deployer can overwrite an existing serving version.

The initial hosted probe used a temporary workflow/ref with an exact provider
exception, then removed that workflow and exception after observing success.
The temporary remote ref was deleted; the Actions run and artifacts remain.
The provider now accepts only the permanent condition above. Local gcloud
credentials do not prove the Actions route works. Allow up to five minutes
for new federation and IAM settings to propagate; inspect an authentication
failure before attempting another run.

The [authenticated Actions probe](https://github.com/terryyin/lizard/actions/runs/40482567466)
succeeded on 2026-10-27 after its own Python checks, lockfile build and 16
browser scenarios. It deployed source
`b34c4666b40322fb416822c5936d774eadab7f09` as
`probe-40482567466-1`, running `python313`, using the dedicated identity above.
The [preview](https://probe-40482567466-1-dot-lizardanalyzer.appspot.com)
passed normal TLS, homepage, every generated manifest asset and real
`shipping_cost` analysis (CCN 3, NLOC 6, one parameter). Both captured traffic
splits were `revive-20261027-f818daf: 1.0`; no old version was stopped and no
public traffic was moved. The run's `website-preview-probe` artifact retains
source, upload, deployed metadata, preview URL, API result and before/after
traffic. The probe version is retained for diagnosis; it receives no public
traffic.

For account/permission recovery, inspect the named provider and IAM policies
before changing them. Restore only a missing grant at the scope listed above;
do not grant Editor or reuse the runtime identity as the deployer. GitHub
Actions needs `contents: read` and job-scoped `id-token: write`. Check out the
source before `google-github-actions/auth@v3`, then use
`google-github-actions/setup-gcloud@v3`. Generated `gha-creds-*.json` files are
ignored by Git and excluded by the deployment upload allowlist; never archive
them. The deployment records `LIZARD_WEBSITE_SOURCE_SHA` in the App Engine
version's environment metadata so the tested source can be inspected.

References: [Google's federation setup](https://github.com/google-github-actions/auth#workload-identity-federation-through-a-service-account),
[numeric GitHub identity claims](https://docs.cloud.google.com/iam/docs/workload-identity-federation-with-deployment-pipelines),
and [App Engine deployment permissions](https://docs.cloud.google.com/appengine/docs/standard/roles#recommended_role_for_application_deployment).
